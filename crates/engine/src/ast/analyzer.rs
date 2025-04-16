use std::{cell::RefCell, rc::Rc};

use crate::{
    error::{TaleError, TaleResultVec},
    state::{StateTable, SymbolTable, SymbolValue},
};

use super::{
    Ast, Atom, Duration, Eval, Expr, Modifier, Node, RcNode, Script, Statement, Table, TableGroup,
    TableRows, TypedNode,
};

pub trait Analyze {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()>;
}

impl Analyze for Ast {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        self.nodes.analyze(symbols)
    }
}

impl<T> Analyze for Node<T>
where
    T: Analyze + TypedNode,
{
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        self.inner_t().analyze(symbols).map_err(|mut errs| {
            for err in &mut errs {
                // Amend default spans with actual spans
                if err.start() == 0 && err.end() == 0 {
                    err.update_span(self.source_span());
                    err.append_message(&format!(" (In: {})", self.node_type()));
                }
                if err.position() == (0, 0) {
                    err.update_position(self.position());
                }
            }
            errs
        })
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
                node.analyze(symbols)
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
                node.analyze(symbols)
            }
            Statement::Table(node) => {
                symbols
                    .borrow_mut()
                    .register(node.inner_t().name().inner_t().to_lowercase());
                node.analyze(symbols)
            }
            Statement::TableGroup(node) => {
                symbols
                    .borrow_mut()
                    .register(node.inner_t().name().inner_t().to_lowercase());
                node.analyze(symbols)
            }
            Statement::Assignment(name, value) => {
                // Register the variable in the symbol table
                symbols.borrow_mut().register(name.inner_t().to_lowercase());
                // For `Assignment`, we need to analyze both the name and the value
                name.analyze(symbols)?;
                value.analyze(symbols)?;
                Ok(())
            }
            Statement::Clear(duration, target) => {
                // For `Clear`, we need to analyze the duration and the target
                duration.analyze(symbols)?;
                target.analyze(symbols)?;
                Ok(())
            }
            Statement::Invoke(node) | Statement::Load(node) => node.analyze(symbols),
            Statement::Modify(modifier, target) => {
                // For `Modify`, we need to analyze the modifier and the target
                modifier.analyze(symbols)?;
                target.analyze(symbols)?;
                Ok(())
            }
            Statement::Output(node) => node.analyze(symbols),
            Statement::Show(node) => {
                if node.inner_t().0 {
                    Ok(())
                } else {
                    // TODO: make sure there's something to show
                    node.inner_t().1.analyze(symbols)
                }
            }
            Statement::Sequence(node) => node.analyze(symbols),
            Statement::Expr(expr) => ammend_id_to_str(symbols, expr),
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
        if let TableRows::Keyed(items) = &*self.rows.inner_t() {
            match items.iter().try_fold(SymbolValue::Placeholder, |prev, row| {
                    match (&prev, row.0.inner_t().eval(symbols, &StateTable::default())?) {
                        (SymbolValue::Placeholder, other) => Ok(other),
                        (SymbolValue::List(_), SymbolValue::List(item)) => Ok(SymbolValue::List(item)),
                        (SymbolValue::String(_), SymbolValue::String(item)) => Ok(SymbolValue::String(item)),
                        _ => Err(vec![TaleError::analyzer(
                            row.0.source_span(), row.0.position(),
                            format!("Keyed rows must all have the same key type.\n(Previous row was: {prev})")
                        )]),
                    }
                }) {
                    Ok(key_kind) => {
                        match key_kind {
                            SymbolValue::List(_) => {
                                self.rows.add_detail("key_type".into(), "numeric".into());
                                Ok(())
                            },
                            SymbolValue::String(_) => {
                                self.rows.add_detail("key_type".into(), "text".into());
                                Ok(())
                            },
                            _ => unreachable!("Parser bug, row keys should only be textual or numeric"),
                        }
                    },
                    Err(err) => Err(err),
                }
        } else {
            Ok(())
        }
    }
}

impl Analyze for TableGroup {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        for table in &self.sub_tables {
            symbols
                .borrow_mut()
                .register(table.inner_t().name().inner_t().to_lowercase());
            table.analyze(symbols)?;
        }
        Ok(())
    }
}

impl Analyze for TableRows {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        match self {
            TableRows::Keyed(items) => {
                for (key, stmt) in items {
                    let key_copy = key.inner_t().clone();
                    if let Expr::Atom(Atom::Ident(id)) = key_copy {
                        key.replace_inner_t(Expr::Atom(Atom::Str(id)));
                    }
                    stmt.analyze(symbols)?;
                }
                Ok(())
            }
            TableRows::Flat(items) => {
                for item in items {
                    item.analyze(symbols)?;
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
            if !symbols.borrow().is_def(&id) {
                if let Some(ogt) = expr.get_detail("original_text") {
                    expr.replace_inner_t(Expr::Atom(Atom::Str(ogt)));
                } else {
                    expr.replace_inner_t(Expr::Atom(Atom::Str(id)));
                }
            }
        }
        Expr::Roll(reps, target) => {
            if let (Expr::Atom(Atom::Ident(idl)), Expr::Atom(Atom::Ident(idr))) =
                (&*reps.inner_t(), &*target.inner_t())
            {
                if !symbols.borrow().is_def(idl) || !symbols.borrow().is_def(idr) {
                    if let Some(sauce) = expr.get_detail("words_only") {
                        expr.replace_inner_t(Expr::Atom(Atom::Str(sauce)));
                    }
                }
            }
        }
        _ => (),
    };
    expr.analyze(symbols)
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
    if let (Expr::Atom(Atom::Ident(lhs)), Expr::Atom(Atom::Ident(rhs))) = (left, right) {
        match (symbols.borrow().is_def(&lhs), symbols.borrow().is_def(&rhs)) {
            (true, true) => (),
            (true, false) => {
                return Err(vec![TaleError::analyzer(
                    target.source_span(),
                    target.position(),
                    format!("Roll target '{rhs}' is not defined"),
                )]);
            }
            (false, true) => {
                return Err(vec![TaleError::analyzer(
                    reps.source_span(),
                    reps.position(),
                    format!("Roll reps '{lhs}' is not defined"),
                )]);
            }
            (false, false) => {
                let joined = format!("{lhs} {rhs}");
                if symbols.borrow().is_def(&joined) {
                    reps.replace_inner_t(Expr::Atom(Atom::Number(1)));
                    target.replace_inner_t(Expr::Atom(Atom::Ident(joined.clone())));
                } else {
                    return Err(vec![TaleError::analyzer(
                        reps.source_span(),
                        reps.position(),
                        format!("Roll: neither '{lhs}' nor '{rhs}' are defined"),
                    )]);
                }
            }
        }
    };
    reps.analyze(symbols)?;
    target.analyze(symbols)
}

impl Analyze for Atom {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        match self {
            Atom::Dice(x, y) => {
                if x == &0 || y == &0 {
                    Err(vec![TaleError::evaluator(
                        0..0,
                        (0, 0),
                        "Cannot roll zero dice or zero sides.".to_string(),
                    )])
                } else {
                    Ok(())
                }
            }
            Atom::Ident(id) => {
                if symbols.borrow().is_def(id) {
                    Ok(())
                } else {
                    Err(vec![TaleError::analyzer(
                        0..0,
                        (0, 0),
                        format!("Identifier '{id}' is not defined"),
                    )])
                }
            }
            Atom::Number(_) | Atom::Str(_) | Atom::Raw(_) => Ok(()),
        }
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
    use std::cell::RefCell;

    use crate::samples::STATEMENT_ROLL;

    use crate::state::{StateTable, SymbolTable};

    #[test]
    fn analyze_roll() {
        let table = StateTable::default();
        let symbols: RefCell<SymbolTable> = RefCell::default();
        table.add_source("roll".to_string(), STATEMENT_ROLL.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{errors:?}"), "Ok(())");

        symbols.borrow_mut().register("farm animals".to_string());

        let outcome = table.analyze_current(&symbols);
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
                Roll 1d6, `magic items #3`,\n\t\
                Roll 1, `farm animals`,\n\t\
                Roll 1, `farm animals`,\n\t\
                Roll 1, `farm animals`,\n\t\
                Roll 3, 1d6,\n\t\
                Roll 1d6, 1d6,\n\t\
                Roll 6, 3d6\n\
            ]",
            table
                .asts()
                .borrow()
                .get(&*table.current())
                .unwrap()
                .to_string()
        );
    }
}
