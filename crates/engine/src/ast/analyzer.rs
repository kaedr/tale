use std::{cell::RefCell, rc::Rc};

use crate::{
    error::{TaleError, TaleResultVec},
    state::{SymbolTable, SymbolValue},
};

use super::{
    AST, Atom, Duration, Eval, Expr, Modifier, Node, RcNode, Script, Statement, Table, TableGroup,
    TableRows, TypedNode,
};

pub trait Analyze {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()>;
}

impl Analyze for AST {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        self.nodes.inner_t().analyze(symbols)
    }
}

impl<T> Analyze for Node<T>
where
    T: Analyze + TypedNode,
{
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        self.inner_t().analyze(symbols)
    }
}

impl<T> Analyze for Vec<Rc<Node<T>>>
where
    T: Analyze + TypedNode,
{
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        self.iter()
            .map(|node| {
                // Call analyze on each node in the vector
                node.inner_t().analyze(symbols)
            })
            .reduce(|acc, item| match (acc, item) {
                (Ok(()), Ok(())) => Ok(()),
                (Ok(()), Err(errors)) => Err(errors),
                (Err(acc_errors), Ok(())) => Err(acc_errors),
                (Err(mut acc_errors), Err(errors)) => {
                    acc_errors.extend(errors);
                    Err(acc_errors)
                }
            })
            .unwrap_or(Ok(()))
    }
}

impl Analyze for Statement {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        match self {
            Statement::Empty => Ok(()),
            Statement::Script(node) => {
                symbols
                    .borrow_mut()
                    .register(node.inner_t().name().inner_t().to_lowercase());
                node.inner_t().analyze(symbols)
            }
            Statement::Table(node) => {
                symbols
                    .borrow_mut()
                    .register(node.inner_t().name().inner_t().to_lowercase());
                node.inner_t().analyze(symbols)
            }
            Statement::TableGroup(node) => {
                symbols
                    .borrow_mut()
                    .register(node.inner_t().name().inner_t().to_lowercase());
                node.inner_t().analyze(symbols)
            }
            Statement::Assignment(name, value) => {
                // For `Assignment`, we need to analyze both the name and the value
                name.inner_t().analyze(symbols)?;
                value.inner_t().analyze(symbols)?;
                // Register the variable in the symbol table
                symbols.borrow_mut().register(name.inner_t().to_lowercase());
                Ok(())
            }
            Statement::Clear(duration, target) => {
                // For `Clear`, we need to analyze the duration and the target
                duration.inner_t().analyze(symbols)?;
                target.inner_t().analyze(symbols)?;
                Ok(())
            }
            Statement::Invoke(node) => node.inner_t().analyze(symbols),
            Statement::Load(node) => node.inner_t().analyze(symbols),
            Statement::Modify(modifier, target) => {
                // For `Modify`, we need to analyze the modifier and the target
                modifier.inner_t().analyze(symbols)?;
                target.inner_t().analyze(symbols)?;
                Ok(())
            }
            Statement::Output(node) => node.inner_t().analyze(symbols),
            Statement::Show(node) => {
                if node.inner_t().0 {
                    Ok(())
                } else {
                    // TODO: make sure there's something to show
                    node.inner_t().1.analyze(symbols)
                }
            }
            Statement::Sequence(node) => node.inner_t().analyze(symbols),
            Statement::Expr(node) => node.inner_t().analyze(symbols),
        }
    }
}

impl Analyze for Script {
    fn analyze(&self, _symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        Ok(())
    }
}

impl Analyze for Table {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        self.rows.analyze(symbols)?;
        match &*self.rows.inner_t() {
            TableRows::Keyed(items) => {
                match items.iter().try_fold(SymbolValue::Placeholder, |prev, row| {
                    match (&prev, row.0.inner_t().eval(symbols)?) {
                        (SymbolValue::Placeholder, other) => Ok(other),
                        (SymbolValue::List(_), SymbolValue::List(item)) => Ok(SymbolValue::List(item)),
                        (SymbolValue::String(_), SymbolValue::String(item)) => Ok(SymbolValue::String(item)),
                        _ => Err(vec![TaleError::analyzer(
                            row.0.source_span(), row.0.position(),
                            format!("Keyed rows must all have the same key type.\n(Previous row was: {})", prev)
                        )]),
                    }
                }) {
                    Ok(key_kind) => {
                        match key_kind {
                            SymbolValue::List(_) => {
                                self.rows.add_detail("key_type".into(), "numeric".into());
                                ()
                            },
                            SymbolValue::String(_) => {
                                self.rows.add_detail("key_type".into(), "text".into());
                                ()
                            },
                            _ => unreachable!("Parser bug, row keys should only be textual or numeric"),
                        }
                    },
                    Err(err) => return Err(err),
                }
            }
            _ => (),
        };
        Ok(())
    }
}

impl Analyze for TableGroup {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        for table in self.sub_tables.iter() {
            symbols
                .borrow_mut()
                .register(table.inner_t().name().inner_t().to_lowercase());
            table.inner_t().analyze(symbols)?;
        }
        Ok(())
    }
}

impl Analyze for TableRows {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        match self {
            TableRows::Keyed(items) => {
                for (key, stmt) in items.iter() {
                    let key_copy = key.inner_t().clone();
                    match key_copy {
                        Expr::Atom(Atom::Ident(id)) => {
                            *key.inner_t_mut() = Expr::Atom(Atom::Str(id));
                        }
                        _ => (),
                    }
                    match &*stmt.inner_t() {
                        Statement::Expr(expr) => ammend_id_to_str(symbols, expr)?,
                        other => other.analyze(symbols)?,
                    }
                }
                Ok(())
            }
            TableRows::Flat(items) => {
                for item in items.iter() {
                    match &*item.inner_t() {
                        Statement::Expr(expr) => ammend_id_to_str(symbols, expr)?,
                        other => other.analyze(symbols)?,
                    }
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

fn ammend_id_to_str(symbols: &RefCell<SymbolTable>, expr: &RcNode<Expr>) -> TaleResultVec<()> {
    let expr_copy = expr.inner_t().clone();
    match expr_copy {
        Expr::Atom(Atom::Ident(id)) => {
            if symbols.borrow().is_def(&id) {
                Ok(())
            } else {
                *expr.inner_t_mut() = Expr::Atom(Atom::Str(id));
                Ok(())
            }
        }
        Expr::Roll(reps, target) => match (&*reps.inner_t(), &*target.inner_t()) {
            (Expr::Atom(Atom::Ident(idl)), Expr::Atom(Atom::Ident(idr))) => {
                if symbols.borrow().is_def(&idl) || symbols.borrow().is_def(&idr) {
                    Ok(())
                } else {
                    if let Some(sauce) = expr.get_detail("words_only") {
                        *expr.inner_t_mut() = Expr::Atom(Atom::Str(sauce));
                    }
                    Ok(())
                }
            }
            _ => Ok(()),
        },
        other => other.analyze(symbols),
    }
}

impl Analyze for Expr {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        match self {
            Expr::Empty => Ok(()),
            Expr::Atom(_atom) => Ok(()),
            Expr::Neg(_expr) => Ok(()),
            Expr::Add(_lhs, _rhs) => Ok(()),
            Expr::Sub(_lhs, _rhs) => Ok(()),
            Expr::Mul(_lhs, _rhs) => Ok(()),
            Expr::Div(_lhs, _rhs) => Ok(()),
            Expr::Mod(_lhs, _rhs) => Ok(()),
            Expr::Pow(_lhs, _rhs) => Ok(()),
            Expr::Lookup(key, target) => analyze_lookup(symbols, key, target),
            Expr::Roll(reps, target) => analyze_roll(symbols, reps, target),
            Expr::Interpol(_exprs) => Ok(()),
            Expr::List(_atoms) => Ok(()),
        }
    }
}

fn analyze_lookup(
    symbols: &RefCell<SymbolTable>,
    key: &RcNode<Expr>,
    _target: &RcNode<Expr>,
) -> TaleResultVec<()> {
    ammend_id_to_str(symbols, key)
}

fn analyze_roll(
    symbols: &RefCell<SymbolTable>,
    reps: &RcNode<Expr>,
    target: &RcNode<Expr>,
) -> TaleResultVec<()> {
    let (left, right) = (reps.inner_t().clone(), target.inner_t().clone());
    match (left, right) {
        (Expr::Atom(Atom::Ident(lhs)), Expr::Atom(Atom::Ident(rhs))) => {
            match (symbols.borrow().is_def(&lhs), symbols.borrow().is_def(&rhs)) {
                (true, true) => {
                    // Both are defined, this is okay
                    Ok(())
                }
                (true, false) => Err(vec![TaleError::analyzer(
                    target.source_span(),
                    target.position(),
                    format!("Roll target '{rhs}' is not defined"),
                )]),
                (false, true) => Err(vec![TaleError::analyzer(
                    reps.source_span(),
                    reps.position(),
                    format!("Roll reps '{lhs}' is not defined"),
                )]),
                (false, false) => {
                    let joined = format!("{lhs} {rhs}");
                    if symbols.borrow().is_def(&joined) {
                        *reps.inner_t_mut() = Expr::Atom(Atom::Number(1));
                        *target.inner_t_mut() = Expr::Atom(Atom::Ident(joined.clone()));
                        Ok(())
                    } else {
                        Err(vec![TaleError::analyzer(
                            reps.source_span(),
                            reps.position(),
                            format!("Roll: neither '{lhs}' nor '{rhs}' are defined"),
                        )])
                    }
                }
            }
        }
        _ => Ok(()),
    }
}

impl Analyze for Atom {
    fn analyze(&self, _symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        Ok(())
    }
}

impl Analyze for Modifier {
    fn analyze(&self, _symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        Ok(())
    }
}

impl Analyze for Duration {
    fn analyze(&self, _symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        Ok(())
    }
}

#[cfg(test)]
#[allow(unused_must_use)]
mod tests {
    use crate::utils::tests::read_sample_file_to_string;

    use crate::state::StateTable;

    #[test]
    fn analyze_roll() {
        let name = "18_statement_roll.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");

        table.symbols_mut().register("farm animals".to_string());

        let outcome = table.analyze_current();
        assert!(
            outcome.is_ok(),
            "Analysis failed with errors: {:?}",
            outcome.err().unwrap()
        );

        assert_eq!(
            "Sequence: [\n\t\
                Roll 1, 3d6,\n\t\
                Roll 1, (1d20 + 7),\n\t\
                Roll 1, `magic item table a`,\n\t\
                Roll 7, `magic item table a`,\n\t\
                Roll 1d6, `magic item table a`,\n\t\
                Roll 1d6, `magic item table a`,\n\t\
                Roll (1d4 + 2), `farm animals`,\n\t\
                Roll (1d6 - 1), `farm animals`,\n\t\
                Roll (1d4 - 2), `farm animals`,\n\t\
                Roll 1, `farm animals`,\n\t\
                Roll 1, `farm animals`,\n\t\
                Roll 1, `farm animals`,\n\t\
                Roll 3, 1d6,\n\t\
                Roll 1d6, 1d6,\n\t\
                Roll 6, 3d6\n\
            ]",
            table.asts().get(table.current()).unwrap().to_string()
        )
    }
}
