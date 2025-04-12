use std::cell::RefCell;

use crate::{
    ast::*,
    error::{TaleError, TaleResultVec},
    state::SymbolTable,
    state::SymbolValue,
};
use rand::Rng;

pub trait Eval {
    /// Evaluates the expression in the context of the provided symbol table.
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<SymbolValue>;
}

impl<T> Eval for Node<T>
where
    T: Eval + TypedNode,
{
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<SymbolValue> {
        self.inner_t().eval(symbols).map_err(|mut errs| {
            errs.iter_mut().for_each(|err| {
                // Amend default spans with actual spans
                if err.start() == 0 && err.end() == 0 {
                    err.update_span(self.source_span());
                    err.append_message(&format!(" (In: {})", self.node_type()));
                }
                if err.position() == (0, 0) {
                    err.update_position(self.position());
                }
            });
            errs
        })
    }
}

impl<T> Eval for Vec<T>
where
    T: Eval,
{
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<SymbolValue> {
        self.iter()
            .fold(Ok(SymbolValue::List(Vec::new())), |prev, curr| {
                match (prev, curr.eval(symbols)) {
                    (Ok(SymbolValue::List(mut list)), Ok(v)) => {
                        list.push(v);
                        Ok(SymbolValue::List(list))
                    }
                    (Ok(_), Err(e)) => Err(e), // Propagate the error from the current node
                    (Err(e), Ok(_)) => Err(e), // Keep the previous error
                    (Err(mut e1), Err(e2)) => {
                        e1.extend(e2); // Merge Errors
                        Err(e1)
                    }
                    _ => unreachable!(),
                }
            })
    }
}

impl<T> Eval for Vec<RcNode<T>>
where
    T: Eval + TypedNode,
{
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<SymbolValue> {
        self.iter()
            .fold(Ok(SymbolValue::List(Vec::new())), |prev, curr| {
                match (prev, curr.eval(symbols)) {
                    (Ok(SymbolValue::List(mut list)), Ok(v)) => {
                        list.push(v);
                        Ok(SymbolValue::List(list))
                    }
                    (Ok(_), Err(e)) => Err(e), // Propagate the error from the current node
                    (Err(e), Ok(_)) => Err(e), // Keep the previous error
                    (Err(mut e1), Err(e2)) => {
                        e1.extend(e2); // Merge Errors
                        Err(e1)
                    }
                    _ => unreachable!(),
                }
            })
    }
}

impl Eval for AST {
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<SymbolValue> {
        self.nodes().eval(symbols)
    }
}

impl Eval for Script {
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<SymbolValue> {
        self.name().eval(symbols)
    }
}

impl Eval for Table {
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<SymbolValue> {
        let tags = match self.tags().eval(symbols)? {
            SymbolValue::List(tags) => tags.iter().map(SymbolValue::to_string).collect::<Vec<_>>(),
            _ => unreachable!(),
        };
        let table_name = self.name().eval(symbols)?;
        symbols.borrow_mut().push_tags(tags, table_name.to_string());
        Ok(table_name)
    }
}

impl Eval for TableGroup {
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<SymbolValue> {
        let tags = match self.tags().eval(symbols)? {
            SymbolValue::List(tags) => tags.iter().map(SymbolValue::to_string).collect::<Vec<_>>(),
            _ => unreachable!(),
        };
        let table_name = self.name().eval(symbols)?;
        symbols.borrow_mut().push_tags(tags, table_name.to_string());
        Ok(table_name)
    }
}

impl Eval for Statement {
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<SymbolValue> {
        match self {
            Statement::Empty => Ok(SymbolValue::Placeholder),
            Statement::Script(script) => script_def(symbols, script),
            Statement::Table(table) => table_def(symbols, table),
            Statement::TableGroup(table_group) => table_group_def(symbols, table_group),
            Statement::Assignment(name, value) => assignment_stmt(symbols, name, value),
            Statement::Clear(duration, target) => clear_stmt(symbols, duration, target),
            Statement::Invoke(target) => invoke_stmt(symbols, target),
            Statement::Load(target) => load_stmt(symbols, target),
            Statement::Modify(modifier, target) => modify_stmt(symbols, modifier, target),
            Statement::Output(expr) => expr.eval(symbols),
            Statement::Show(node) => show_stmt(symbols, node),
            Statement::Sequence(seq) => seq.eval(symbols),
            Statement::Expr(expr) => expr.eval(symbols),
        }
    }
}

fn script_def(
    symbols: &RefCell<SymbolTable>,
    script: &RcNode<Script>,
) -> TaleResultVec<SymbolValue> {
    let name = script.eval(symbols)?.to_string();
    match symbols
        .borrow_mut()
        .insert(name.clone(), SymbolValue::Script(script.clone()))
    {
        false => Ok(SymbolValue::String(format!(
            "Overwriting previous value stored in `{name}`"
        ))),
        true => Ok(SymbolValue::Script(script.clone())),
    }
}

fn table_def(symbols: &RefCell<SymbolTable>, table: &RcNode<Table>) -> TaleResultVec<SymbolValue> {
    insert_table_def(symbols, table)
}

fn table_group_def(
    symbols: &RefCell<SymbolTable>,
    table_group: &RcNode<TableGroup>,
) -> TaleResultVec<SymbolValue> {
    let mut outcomes = Vec::new();
    for table in table_group.inner_t().sub_tables.iter() {
        outcomes.push(insert_table_def(symbols, table)?);
    }
    outcomes.push(insert_table_def(
        symbols,
        &rc_node(table_group.inner_t().clone().into()),
    )?);
    Ok(SymbolValue::List(outcomes))
}

fn insert_table_def(
    symbols: &RefCell<SymbolTable>,
    table: &RcNode<Table>,
) -> TaleResultVec<SymbolValue> {
    let name = table.eval(symbols)?.to_string();
    match symbols
        .borrow_mut()
        .insert(name.clone(), SymbolValue::Table(table.clone()))
    {
        false => Ok(SymbolValue::String(format!(
            "Overwriting previous value stored in `{name}`"
        ))),
        true => Ok(SymbolValue::Table(table.clone())),
    }
}

fn assignment_stmt(
    symbols: &RefCell<SymbolTable>,
    name: &RcNode<Atom>,
    value: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let name = name.inner_t().to_string().trim_matches('`').to_string();
    let value = value.eval(symbols)?;
    match symbols.borrow_mut().insert(name.to_string(), value) {
        false => Ok(SymbolValue::String(format!(
            "Overwriting previous value stored in `{name}`"
        ))),
        true => Ok(SymbolValue::Placeholder),
    }
}

fn clear_stmt(
    symbols: &RefCell<SymbolTable>,
    duration: &RcNode<Duration>,
    target: &RcNode<Atom>,
) -> TaleResultVec<SymbolValue> {
    let value = target.eval(symbols)?;
    match value {
        SymbolValue::Table(table) => {
            table
                .inner_t_mut()
                .clear_modifier(duration.inner_t().clone());
            Ok(SymbolValue::Placeholder)
        }
        _ => Err(vec![TaleError::evaluator(
            target.source_span(),
            target.position(),
            format!("Cannot clear modifiers for: '{value}' (Not a Table or Group name)"),
        )]),
    }
}

fn invoke_stmt(
    symbols: &RefCell<SymbolTable>,
    target: &RcNode<Atom>,
) -> TaleResultVec<SymbolValue> {
    let value = target.eval(symbols)?;
    roll_invoke_or_err(symbols, value)
}

fn load_stmt(symbols: &RefCell<SymbolTable>, target: &RcNode<Atom>) -> TaleResultVec<SymbolValue> {
    todo!()
}

fn modify_stmt(
    symbols: &RefCell<SymbolTable>,
    modifier: &RcNode<Modifier>,
    target: &RcNode<Atom>,
) -> TaleResultVec<SymbolValue> {
    let value = target.eval(symbols)?;
    match value {
        SymbolValue::Table(table) => {
            table.inner_t_mut().add_modifier(modifier.inner_t().clone());
            Ok(SymbolValue::Placeholder)
        }
        _ => Err(vec![TaleError::evaluator(
            target.source_span(),
            target.position(),
            format!("Cannot modify: '{value}' (Not a Table or Group name)"),
        )]),
    }
}

fn show_stmt(
    symbols: &RefCell<SymbolTable>,
    node: &RcNode<(bool, Atom)>,
) -> TaleResultVec<SymbolValue> {
    if node.inner_t().0 {
        let target = node.inner_t().1.to_lowercase();
        Ok(symbols
            .borrow()
            .get_tags(target.to_string().split_whitespace().collect::<Vec<_>>()))
    } else {
        node.inner_t().1.eval(symbols)
    }
}

impl Eval for Expr {
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<SymbolValue> {
        match self {
            Expr::Empty => Ok(SymbolValue::Placeholder),
            Expr::Atom(atom) => atom.eval(symbols),
            Expr::Neg(node) => match node.eval(symbols)? {
                SymbolValue::Numeric(n) => Ok(SymbolValue::Numeric(-n)),
                other => Err(vec![TaleError::evaluator(
                    node.source_span(),
                    node.position(),
                    format!("Cannot negate {:?}", other),
                )]),
            },
            Expr::Add(lhs, rhs) => add_expr(symbols, lhs, rhs),
            Expr::Sub(lhs, rhs) => sub_expr(symbols, lhs, rhs),
            Expr::Mul(lhs, rhs) => mul_expr(symbols, lhs, rhs),
            Expr::Div(lhs, rhs) => div_expr(symbols, lhs, rhs),
            Expr::Mod(lhs, rhs) => mod_expr(symbols, lhs, rhs),
            Expr::Pow(lhs, rhs) => pow_expr(symbols, lhs, rhs),
            Expr::Lookup(value, target) => lookup_expr(symbols, value, target),
            Expr::Roll(reps, target) => roll_expr(symbols, reps, target),
            Expr::Interpol(node) => interpol_expr(symbols, node),
            Expr::List(node) => node.eval(symbols),
        }
    }
}

fn add_expr(
    symbols: &RefCell<SymbolTable>,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let left_value = lhs.eval(symbols);
    let right_value = rhs.eval(symbols);

    match (left_value, right_value) {
        (Ok(SymbolValue::Numeric(l)), Ok(SymbolValue::Numeric(r))) => {
            Ok(SymbolValue::Numeric(l + r))
        }
        (Err(err), Ok(_)) => Err(err),
        (Ok(_), Err(err)) => Err(err),
        (Err(mut l_err), Err(r_err)) => {
            l_err.extend(r_err);
            Err(l_err)
        }
        _ => Err(vec![TaleError::evaluator(
            lhs.merge_source_span(rhs),
            lhs.position(),
            "Cannot add non-numeric values.".to_string(),
        )]),
    }
}

fn sub_expr(
    symbols: &RefCell<SymbolTable>,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let left_value = lhs.eval(symbols);
    let right_value = rhs.eval(symbols);

    match (left_value, right_value) {
        (Ok(SymbolValue::Numeric(l)), Ok(SymbolValue::Numeric(r))) => {
            Ok(SymbolValue::Numeric(l - r))
        }
        (Err(err), Ok(_)) => Err(err),
        (Ok(_), Err(err)) => Err(err),
        (Err(mut l_err), Err(r_err)) => {
            l_err.extend(r_err);
            Err(l_err)
        }
        _ => Err(vec![TaleError::evaluator(
            lhs.merge_source_span(rhs),
            lhs.position(),
            "Cannot subract non-numeric values.".to_string(),
        )]),
    }
}

fn mul_expr(
    symbols: &RefCell<SymbolTable>,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let left_value = lhs.eval(symbols);
    let right_value = rhs.eval(symbols);

    match (left_value, right_value) {
        (Ok(SymbolValue::Numeric(l)), Ok(SymbolValue::Numeric(r))) => {
            Ok(SymbolValue::Numeric(l * r))
        }
        (Err(err), Ok(_)) => Err(err),
        (Ok(_), Err(err)) => Err(err),
        (Err(mut l_err), Err(r_err)) => {
            l_err.extend(r_err);
            Err(l_err)
        }
        _ => Err(vec![TaleError::evaluator(
            lhs.merge_source_span(rhs),
            lhs.position(),
            "Cannot multiply non-numeric values.".to_string(),
        )]),
    }
}

fn div_expr(
    symbols: &RefCell<SymbolTable>,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let left_value = lhs.eval(symbols);
    let right_value = rhs.eval(symbols);

    match (left_value, right_value) {
        (Ok(SymbolValue::Numeric(l)), Ok(SymbolValue::Numeric(r))) => {
            Ok(SymbolValue::Numeric(l / r))
        }
        (Err(err), Ok(_)) => Err(err),
        (Ok(_), Err(err)) => Err(err),
        (Err(mut l_err), Err(r_err)) => {
            l_err.extend(r_err);
            Err(l_err)
        }
        _ => Err(vec![TaleError::evaluator(
            lhs.merge_source_span(rhs),
            lhs.position(),
            "Cannot divide non-numeric values.".to_string(),
        )]),
    }
}

fn mod_expr(
    symbols: &RefCell<SymbolTable>,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let left_value = lhs.eval(symbols);
    let right_value = rhs.eval(symbols);

    match (left_value, right_value) {
        (Ok(SymbolValue::Numeric(l)), Ok(SymbolValue::Numeric(r))) => {
            Ok(SymbolValue::Numeric(l % r))
        }
        (Err(err), Ok(_)) => Err(err),
        (Ok(_), Err(err)) => Err(err),
        (Err(mut l_err), Err(r_err)) => {
            l_err.extend(r_err);
            Err(l_err)
        }
        _ => Err(vec![TaleError::evaluator(
            lhs.merge_source_span(rhs),
            lhs.position(),
            "Cannot modulo non-numeric values.".to_string(),
        )]),
    }
}

fn pow_expr(
    symbols: &RefCell<SymbolTable>,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let left_value = lhs.eval(symbols);
    let right_value = rhs.eval(symbols);

    match (left_value, right_value) {
        (Ok(SymbolValue::Numeric(l)), Ok(SymbolValue::Numeric(r))) => {
            Ok(SymbolValue::Numeric(l.pow(r as u32)))
        }
        (Err(err), Ok(_)) => Err(err),
        (Ok(_), Err(err)) => Err(err),
        (Err(mut l_err), Err(r_err)) => {
            l_err.extend(r_err);
            Err(l_err)
        }
        _ => Err(vec![TaleError::evaluator(
            lhs.merge_source_span(rhs),
            lhs.position(),
            "Cannot exponentiate non-numeric values.".to_string(),
        )]),
    }
}

fn lookup_expr(
    symbols: &RefCell<SymbolTable>,
    value: &RcNode<Expr>,
    target: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let key = value.eval(symbols)?;
    let target_val = target.eval(symbols)?;
    match &target_val {
        SymbolValue::Table(table) => table.inner_t().lookup(symbols, key),
        _ => Err(vec![TaleError::evaluator(
            target.source_span(),
            target.position(),
            format!("Cannot lookup on: '{target_val}' (Not a Table or Group name)"),
        )]),
    }
}

fn roll_expr(
    symbols: &RefCell<SymbolTable>,
    reps: &RcNode<Expr>,
    target: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let reps_val = reps.eval(symbols)?;
    let target_val = target.eval(symbols)?;
    match (reps_val, target_val.clone()) {
        (SymbolValue::Numeric(x), SymbolValue::Numeric(_)) => match x {
            ..=0 => Ok(SymbolValue::Placeholder),
            1..=1 => Ok(target_val),
            2.. => Ok(SymbolValue::List(
                (0..x).map(|_| target.eval(symbols).unwrap()).collect(),
            )),
        },
        (SymbolValue::Numeric(x), target) => match x {
            ..=0 => Ok(SymbolValue::Placeholder),
            1..=1 => roll_invoke_or_err(symbols, target),
            2.. => Ok(SymbolValue::List(
                (0..x)
                    .map(|_| roll_invoke_or_err(symbols, target.clone()))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
        },
        _ => Err(vec![TaleError::evaluator(
            reps.merge_source_span(target),
            reps.position(),
            format!("Invalid types for roll expression: {reps} and {target}."),
        )]),
    }
}

fn roll_invoke_or_err(
    symbols: &RefCell<SymbolTable>,
    target: SymbolValue,
) -> TaleResultVec<SymbolValue> {
    match target {
        SymbolValue::Script(script) => script.inner_t().invoke(symbols),
        SymbolValue::Table(table) => table.inner_t().roll_on(symbols),
        SymbolValue::Numeric(_) => Ok(target),
        SymbolValue::String(_) => Ok(target),
        _ => Err(vec![TaleError::evaluator(
            0..0,
            (0, 0),
            format!("No such Table or Script: '{target}'"),
        )]),
    }
}

fn interpol_expr(
    symbols: &RefCell<SymbolTable>,
    node: &RcNode<Vec<RcNode<Expr>>>,
) -> TaleResultVec<SymbolValue> {
    node.inner_t()
        .iter()
        .fold(Ok(SymbolValue::Placeholder), |acc, expr| {
            match (acc, expr.eval(symbols)) {
                (Ok(SymbolValue::Placeholder), Ok(value)) => Ok(value),
                (Ok(lv), Ok(rv)) => Ok(SymbolValue::String(format!("{lv} {rv}"))),
                (Ok(_), Err(err)) => Err(err),
                (Err(err), Ok(_)) => Err(err),
                (Err(mut l_err), Err(r_err)) => {
                    l_err.extend(r_err);
                    Err(l_err)
                }
            }
        })
}

impl Eval for Atom {
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<SymbolValue> {
        match self {
            Atom::Number(n) => Ok(SymbolValue::Numeric(*n as isize)),
            Atom::Dice(x, y) => roll_dice(*x, *y),
            Atom::Str(s) => Ok(SymbolValue::String(s.clone())),
            Atom::Ident(id) => symbols.borrow().get_value(id),
            Atom::Raw(token) => Ok(SymbolValue::String(token.to_string())),
        }
    }
}

fn roll_dice(x: usize, y: usize) -> TaleResultVec<SymbolValue> {
    if x == 0 || y == 0 {
        return Err(vec![TaleError::evaluator(
            0..0,
            (0, 0),
            "Cannot roll zero dice or zero sides.".to_string(),
        )]);
    }

    let mut rng = rand::rng();
    // Rolling `x` dice with `y` sides
    let mut total = 0;
    for _ in 0..x {
        total += rng.random_range(1..=y);
    }

    Ok(SymbolValue::Numeric(total as isize))
}

#[cfg(test)]
#[allow(unused_must_use)]
mod tests {

    #[test]
    fn it_works() {
        //todo!()
    }
}
