use std::{cell::RefCell, ops::Range, rc::Rc};

use crate::SymbolTable;

use super::{
    AST, Atom, Duration, Expr, Modifier, Node, RcNode, Script, Statement, Table, TableGroup,
};

pub type SemErrors = Vec<(Range<usize>, String)>; // (span, message)

pub trait Analyze {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors>;
}

impl Analyze for AST {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors> {
        self.nodes.inner_t().analyze(symbols)
    }
}

impl<T> Analyze for Node<T>
where
    T: Analyze,
{
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors> {
        self.inner_t().analyze(symbols)
    }
}

// impl<T> Analyze for Vec<T>
// where
//     T: Analyze,
// {
//     fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors> {
//         self.iter()
//             .map(|node| {
//                 // Call analyze on each node in the vector
//                 node.analyze(symbols)
//             })
//             .collect::<Result<(), SemErrors>>()
//     }
// }

impl<T> Analyze for Vec<Rc<Node<T>>>
where
    T: Analyze,
{
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors> {
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
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors> {
        match self {
            Statement::Empty => Ok(()),
            Statement::Script(node) => node.inner_t().analyze(symbols),
            Statement::Table(node) => node.inner_t().analyze(symbols),
            Statement::TableGroup(node) => node.inner_t().analyze(symbols),
            Statement::Assignment(name, value) => {
                // For `Assignment`, we need to analyze both the name and the value
                name.inner_t().analyze(symbols)?;
                value.inner_t().analyze(symbols)?;
                // Register the variable in the symbol table
                symbols.borrow_mut().register(name.inner_t().to_string());
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
                    Ok(()) // When showing tags, no analysis is needed, just return Ok
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
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors> {
        Ok(())
    }
}

impl Analyze for Table {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors> {
        Ok(())
    }
}

impl Analyze for TableGroup {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors> {
        Ok(())
    }
}

impl Analyze for Expr {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors> {
        match self {
            Expr::Empty => Ok(()),
            Expr::Atom(atom) => Ok(()),
            Expr::Neg(node) => Ok(()),
            Expr::Add(node, node1) => Ok(()),
            Expr::Sub(node, node1) => Ok(()),
            Expr::Mul(node, node1) => Ok(()),
            Expr::Div(node, node1) => Ok(()),
            Expr::Mod(node, node1) => Ok(()),
            Expr::Pow(node, node1) => Ok(()),
            Expr::Lookup(node, node1) => Ok(()),
            Expr::Roll(reps, target) => analyze_roll(symbols, reps, target),
            Expr::Interpol(node) => Ok(()),
            Expr::List(node) => Ok(()),
        }
    }
}

fn analyze_roll(
    symbols: &RefCell<SymbolTable>,
    reps: &RcNode<Expr>,
    target: &RcNode<Expr>,
) -> Result<(), SemErrors> {
    let (left, right) = (reps.inner_t().clone(), target.inner_t().clone());
    match (left, right) {
        (Expr::Atom(Atom::Ident(lhs)), Expr::Atom(Atom::Ident(rhs))) => {
            match (symbols.borrow().is_def(&lhs), symbols.borrow().is_def(&rhs)) {
                (true, true) => {
                    // Both are defined, this is okay
                    Ok(())
                }
                (true, false) => Err(vec![(
                    target.source_span(),
                    format!("target '{}' is not defined", rhs),
                )]),
                (false, true) => Err(vec![(
                    reps.source_span(),
                    format!("reps '{}' is not defined", lhs),
                )]),
                (false, false) => {
                    let joined = format!("{} {}", lhs, rhs);
                    if symbols.borrow().is_def(&joined) {
                        *reps.inner_t_mut() = Expr::Atom(Atom::Number(1));
                        *target.inner_t_mut() = Expr::Atom(Atom::Ident(joined.clone()));
                        Ok(())
                    } else {
                        Err(vec![(
                            reps.source_span(),
                            format!("neither '{}' nor '{}' are defined", lhs, rhs),
                        )])
                    }
                }
            }
        }
        _ => Ok(()),
    }
}

impl Analyze for Atom {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors> {
        Ok(())
    }
}

impl Analyze for Modifier {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors> {
        Ok(())
    }
}

impl Analyze for Duration {
    fn analyze(&self, symbols: &RefCell<SymbolTable>) -> Result<(), SemErrors> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use lexer::utils::read_sample_file_to_string;

    use crate::StateTable;

    #[test]
    fn analyze_roll() {
        let name = "18_statement_roll.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "[]");

        table
            .symbols
            .borrow_mut()
            .register("farm animals".to_string());

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
            table.asts.get(&table.current).unwrap().to_string()
        )
    }
}
