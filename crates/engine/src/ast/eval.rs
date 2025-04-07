use std::cell::RefCell;

use crate::{SymbolTable, SymbolValue, ast::*};
use rand::Rng;

pub trait Eval {
    /// Evaluates the expression in the context of the provided symbol table.
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> Result<SymbolValue, String>;
}

impl<T> Eval for Node<T>
where
    T: Eval,
{
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> Result<SymbolValue, String> {
        self.inner_t().eval(symbols)
    }
}

impl<T> Eval for Vec<T>
where
    T: Eval,
{
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> Result<SymbolValue, String> {
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
                        e1.push_str(&format!("\n{}", e2)); // Merge Errors
                        Err(e1)
                    }
                    _ => unreachable!(),
                }
            })
    }
}

impl<T> Eval for Vec<RcNode<T>>
where
    T: Eval,
{
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> Result<SymbolValue, String> {
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
                        e1.push_str(&format!("\n{}", e2)); // Merge Errors
                        Err(e1)
                    }
                    _ => unreachable!(),
                }
            })
    }
}

impl Eval for AST {
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> Result<SymbolValue, String> {
        self.nodes().eval(symbols)
    }
}

impl Eval for Script {
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> Result<SymbolValue, String> {
        self.name().eval(symbols)
    }
}

impl Eval for Table {
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> Result<SymbolValue, String> {
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
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> Result<SymbolValue, String> {
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
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> Result<SymbolValue, String> {
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
            Statement::Sequence(seq) => seq.inner_t().eval(symbols),
            Statement::Expr(expr) => expr.eval(symbols),
        }
    }
}

fn script_def(
    symbols: &RefCell<SymbolTable>,
    script: &RcNode<Script>,
) -> Result<SymbolValue, String> {
    let name = script.eval(symbols)?.to_string();
    match symbols
        .borrow_mut()
        .insert(name.clone(), SymbolValue::Script(script.clone()))
    {
        true => Ok(SymbolValue::String(format!(
            "Overwriting previous value stored in {}",
            name
        ))),
        false => Ok(SymbolValue::Placeholder),
    }
}

fn table_def(symbols: &RefCell<SymbolTable>, table: &RcNode<Table>) -> Result<SymbolValue, String> {
    insert_table_def(symbols, table)
}

fn table_group_def(
    symbols: &RefCell<SymbolTable>,
    table_group: &RcNode<TableGroup>,
) -> Result<SymbolValue, String> {
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
) -> Result<SymbolValue, String> {
    let name = table.eval(symbols)?.to_string();
    match symbols
        .borrow_mut()
        .insert(name.clone(), SymbolValue::Table(table.clone()))
    {
        true => Ok(SymbolValue::String(format!(
            "Overwriting previous value stored in {}",
            name
        ))),
        false => Ok(SymbolValue::Placeholder),
    }
}

fn assignment_stmt(
    symbols: &RefCell<SymbolTable>,
    name: &RcNode<Atom>,
    value: &RcNode<Expr>,
) -> Result<SymbolValue, String> {
    let name = name.eval(symbols)?;
    let value = value.eval(symbols)?;
    match symbols.borrow_mut().insert(name.to_string(), value) {
        true => Ok(SymbolValue::String(format!(
            "Overwriting previous value stored in {}",
            name
        ))),
        false => Ok(SymbolValue::Placeholder),
    }
}

fn clear_stmt(
    symbols: &RefCell<SymbolTable>,
    duration: &RcNode<Duration>,
    target: &RcNode<Atom>,
) -> Result<SymbolValue, String> {
    let name = target.eval(symbols)?;
    match symbols.borrow().get_table(name.to_string()) {
        Some(table) => {
            table
                .inner_t_mut()
                .clear_modifier(duration.inner_t().clone());
            Ok(SymbolValue::Placeholder)
        }
        None => Err(format!("No such Table: '{}'", target)),
    }
}

fn invoke_stmt(
    symbols: &RefCell<SymbolTable>,
    target: &RcNode<Atom>,
) -> Result<SymbolValue, String> {
    let name = target.eval(symbols)?;
    invoke_roll_or_err(symbols, name.to_string())
}

fn load_stmt(symbols: &RefCell<SymbolTable>, target: &RcNode<Atom>) -> Result<SymbolValue, String> {
    todo!()
}

fn modify_stmt(
    symbols: &RefCell<SymbolTable>,
    modifier: &RcNode<Modifier>,
    target: &RcNode<Atom>,
) -> Result<SymbolValue, String> {
    let name = target.eval(symbols)?;
    match symbols.borrow().get_table(name.to_string()) {
        Some(table) => {
            table.inner_t_mut().add_modifier(modifier.inner_t().clone());
            Ok(SymbolValue::Placeholder)
        }
        None => Err(format!("No such Table: '{}'", target)),
    }
}

fn show_stmt(
    symbols: &RefCell<SymbolTable>,
    node: &RcNode<(bool, Atom)>,
) -> Result<SymbolValue, String> {
    let target = node.inner_t().1.eval(symbols)?;
    if node.inner_t().0 {
        Ok(symbols
            .borrow()
            .get_tags(target.to_string().split_whitespace().collect::<Vec<_>>()))
    } else {
        match target.to_string().as_str() {
            "tables" | "table" => symbols.borrow().list_tables(),
            "scripts" | "script" => symbols.borrow().list_scripts(),
            "values" | "variables" | "names" => symbols.borrow().list_names(),
            other => symbols.borrow().show_value(other),
        }
    }
}

impl Eval for Expr {
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> Result<SymbolValue, String> {
        match self {
            Expr::Empty => Ok(SymbolValue::Placeholder),
            Expr::Atom(atom) => atom.eval(symbols),
            Expr::Neg(node) => match node.eval(symbols)? {
                SymbolValue::Placeholder => Err("Cannot negate a placeholder value.".to_string()),
                SymbolValue::Numeric(n) => Ok(SymbolValue::Numeric(-n)),
                SymbolValue::String(_) => Err("Cannot negate a String.".to_string()),
                SymbolValue::Script(_) => Err("Cannot negate a Script.".to_string()),
                SymbolValue::Table(_) => Err("Cannot negate a Table.".to_string()),
                SymbolValue::List(_) => Err("Cannot negate a List.".to_string()),
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
            Expr::List(node) => node.inner_t().eval(symbols),
        }
    }
}

fn add_expr(
    symbols: &RefCell<SymbolTable>,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> Result<SymbolValue, String> {
    let left_value = lhs.eval(symbols)?;
    let right_value = rhs.eval(symbols)?;

    match (left_value, right_value) {
        (SymbolValue::Numeric(l), SymbolValue::Numeric(r)) => Ok(SymbolValue::Numeric(l + r)),
        _ => Err("Cannot add non-numeric values.".to_string()),
    }
}

fn sub_expr(
    symbols: &RefCell<SymbolTable>,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> Result<SymbolValue, String> {
    let left_value = lhs.eval(symbols)?;
    let right_value = rhs.eval(symbols)?;

    match (left_value, right_value) {
        (SymbolValue::Numeric(l), SymbolValue::Numeric(r)) => Ok(SymbolValue::Numeric(l - r)),
        _ => Err("Cannot subract non-numeric values.".to_string()),
    }
}

fn mul_expr(
    symbols: &RefCell<SymbolTable>,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> Result<SymbolValue, String> {
    let left_value = lhs.eval(symbols)?;
    let right_value = rhs.eval(symbols)?;

    match (left_value, right_value) {
        (SymbolValue::Numeric(l), SymbolValue::Numeric(r)) => Ok(SymbolValue::Numeric(l * r)),
        _ => Err("Cannot multiply non-numeric values.".to_string()),
    }
}

fn div_expr(
    symbols: &RefCell<SymbolTable>,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> Result<SymbolValue, String> {
    let left_value = lhs.eval(symbols)?;
    let right_value = rhs.eval(symbols)?;

    match (left_value, right_value) {
        (SymbolValue::Numeric(l), SymbolValue::Numeric(r)) => Ok(SymbolValue::Numeric(l / r)),
        _ => Err("Cannot divide non-numeric values.".to_string()),
    }
}

fn mod_expr(
    symbols: &RefCell<SymbolTable>,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> Result<SymbolValue, String> {
    let left_value = lhs.eval(symbols)?;
    let right_value = rhs.eval(symbols)?;

    match (left_value, right_value) {
        (SymbolValue::Numeric(l), SymbolValue::Numeric(r)) => Ok(SymbolValue::Numeric(l % r)),
        _ => Err("Cannot modulus non-numeric values.".to_string()),
    }
}

fn pow_expr(
    symbols: &RefCell<SymbolTable>,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> Result<SymbolValue, String> {
    let left_value = lhs.eval(symbols)?;
    let right_value = rhs.eval(symbols)?;

    match (left_value, right_value) {
        (SymbolValue::Numeric(l), SymbolValue::Numeric(r)) => {
            Ok(SymbolValue::Numeric(l.pow(r as u32)))
        }
        _ => Err("Cannot exponentiate non-numeric values.".to_string()),
    }
}

fn lookup_expr(
    symbols: &RefCell<SymbolTable>,
    value: &RcNode<Expr>,
    target: &RcNode<Expr>,
) -> Result<SymbolValue, String> {
    let key = value.eval(symbols)?;
    let target_val = target.eval(symbols)?;
    match target_val {
        SymbolValue::String(target) => {
            if let Some(table) = symbols.borrow().get_table(target.clone()) {
                // Roll on the table
                table.inner_t().lookup(key)
            } else {
                Err(format!("No such Table: '{}'", target))
            }
        }
        _ => Err(format!(
            "Cannot lookup on: '{}' (Not a Table or Group name)",
            target_val
        )),
    }
}

fn roll_expr(
    symbols: &RefCell<SymbolTable>,
    reps: &RcNode<Expr>,
    target: &RcNode<Expr>,
) -> Result<SymbolValue, String> {
    let reps_val = reps.eval(symbols)?;
    let target_val = target.eval(symbols)?;
    match (reps_val, target_val.clone()) {
        (SymbolValue::Numeric(x), SymbolValue::Numeric(_)) => {
            if x > 1 {
                Ok(SymbolValue::List(
                    (0..x).map(|_| target.eval(symbols).unwrap()).collect(),
                ))
            } else {
                // TODO: Handle < 1 rolls in analysis
                Ok(target_val)
            }
        }
        (SymbolValue::Numeric(x), SymbolValue::String(target)) => {
            if x > 1 {
                Ok(SymbolValue::List(
                    (0..x)
                        .map(|_| roll_invoke_or_err(symbols, target.clone()))
                        .collect::<Result<Vec<_>, _>>()?,
                ))
            } else {
                // TODO: Handle < 1 rolls in analysis
                roll_invoke_or_err(symbols, target)
            }
        }
        _ => Err(format!(
            "Invalid types for roll expression: {} and {}.",
            reps, target
        )),
    }
}

fn roll_invoke_or_err(
    symbols: &RefCell<SymbolTable>,
    target: String,
) -> Result<SymbolValue, String> {
    if let Some(table) = symbols.borrow().get_table(target.clone()) {
        // Roll on the table
        table.inner_t().roll()
    } else if let Some(script) = symbols.borrow().get_script(target.clone()) {
        // Execute the script and return its result
        script.inner_t().invoke()
    } else {
        Err(format!("No such Table: '{}'", target))
    }
}

fn invoke_roll_or_err(
    symbols: &RefCell<SymbolTable>,
    target: String,
) -> Result<SymbolValue, String> {
    if let Some(script) = symbols.borrow().get_script(target.clone()) {
        // Execute the script and return its result
        script.inner_t().invoke()
    } else if let Some(table) = symbols.borrow().get_table(target.clone()) {
        // Roll on the table
        table.inner_t().roll()
    } else {
        Err(format!("No such Script: '{}'", target))
    }
}

fn interpol_expr(
    symbols: &RefCell<SymbolTable>,
    node: &RcNode<Vec<RcNode<Expr>>>,
) -> Result<SymbolValue, String> {
    node.inner_t()
        .iter()
        .map(|expr| expr.eval(symbols).map(|value| value.to_string()))
        .collect::<Result<Vec<_>, String>>()
        .map(|values| SymbolValue::String(values.join(" ")))
}

impl Eval for Atom {
    fn eval(&self, symbols: &RefCell<SymbolTable>) -> Result<SymbolValue, String> {
        match self {
            Atom::Number(n) => Ok(SymbolValue::Numeric(*n as isize)),
            Atom::Dice(x, y) => roll_dice(*x, *y),
            Atom::Str(s) => Ok(SymbolValue::String(s.clone())),
            Atom::Ident(id) => symbols.borrow().get_value(id),
            Atom::Raw(token) => Ok(SymbolValue::String(token.to_string())),
        }
    }
}

fn roll_dice(x: usize, y: usize) -> Result<SymbolValue, String> {
    if x == 0 || y == 0 {
        return Err("Cannot roll zero dice or zero sides.".to_string());
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
    use super::*;

    #[test]
    fn it_works() {
        //todo!()
    }
}
