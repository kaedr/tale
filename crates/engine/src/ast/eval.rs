use std::{cell::RefCell, fs::read_to_string};

use crate::{
    ast::*,
    error::{TaleError, TaleResultVec},
    state::{StateTable, SymbolTable, SymbolValue},
};
use glob::glob;
use rand::Rng;

pub trait Eval {
    /// Evaluates the expression in the context of the provided symbol table.
    fn eval(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
    ) -> TaleResultVec<SymbolValue>;
}

impl<T> Eval for Rc<T>
where
    T: Eval,
{
    fn eval(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
    ) -> TaleResultVec<SymbolValue> {
        self.as_ref().eval(symbols, state)
    }
}

impl<T> Eval for Node<T>
where
    T: Eval + TypedNode,
{
    fn eval(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
    ) -> TaleResultVec<SymbolValue> {
        self.inner_t().eval(symbols, state).map_err(|mut errs| {
            for err in &mut errs {
                // Amend default spans with actual spans
                if err.start() == 0 && err.end() == 0 {
                    err.update_span(self.source_span());
                }
                if err.position() == (0, 0) {
                    err.update_position(self.position());
                }
                // This added a lot of probably not super helpful info
                // Will be able to do more intelligent messaging by improving errors
                // with an ability to nest structs about there they came from. Additionally
                // adding more info about the type of error it might be.
                // err.append_message(&format!(" (In: {})", self.node_type()));
            }
            errs
        })
    }
}

impl<T> Eval for Vec<T>
where
    T: Eval,
{
    fn eval(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
    ) -> TaleResultVec<SymbolValue> {
        let (values, errors): (Vec<_>, Vec<_>) = self
            .iter()
            .map(|item| item.eval(symbols, state))
            .partition(Result::is_ok);
        if errors.is_empty() {
            Ok(values
                .into_iter()
                .fold(SymbolValue::List(Vec::new()), |list, value| {
                    if let SymbolValue::List(mut list) = list {
                        list.push(value.unwrap());
                        SymbolValue::List(list)
                    } else {
                        unreachable!("Broken Eval for Vec<T> impl");
                    }
                }))
        } else {
            Err(errors.into_iter().flat_map(Result::unwrap_err).collect())
        }
    }
}

impl Eval for Ast {
    fn eval(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
    ) -> TaleResultVec<SymbolValue> {
        self.nodes().eval(symbols, state)
    }
}

impl Eval for Script {
    fn eval(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
    ) -> TaleResultVec<SymbolValue> {
        self.name().eval(symbols, state)
    }
}

impl Eval for Table {
    fn eval(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
    ) -> TaleResultVec<SymbolValue> {
        let tags = match self.tags().eval(symbols, state)? {
            SymbolValue::List(tags) => tags.iter().map(SymbolValue::to_string).collect::<Vec<_>>(),
            _ => unreachable!(),
        };
        let table_name = self.name().inner_t().bare_string();
        symbols.borrow_mut().push_tags(tags, &table_name);
        Ok(SymbolValue::String(table_name))
    }
}

impl Eval for TableGroup {
    fn eval(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
    ) -> TaleResultVec<SymbolValue> {
        let tags = match self.tags().eval(symbols, state)? {
            SymbolValue::List(tags) => tags.iter().map(SymbolValue::to_string).collect::<Vec<_>>(),
            _ => unreachable!(),
        };
        let table_name = self.name().eval(symbols, state)?;
        symbols
            .borrow_mut()
            .push_tags(tags, &table_name.to_string());
        Ok(table_name)
    }
}

impl Eval for Statement {
    fn eval(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
    ) -> TaleResultVec<SymbolValue> {
        match self {
            Statement::Empty => Ok(SymbolValue::Placeholder),
            Statement::Script(script) => script_def(symbols, state, script),
            Statement::Table(table) => table_def(symbols, state, table),
            Statement::TableGroup(table_group) => table_group_def(symbols, state, table_group),
            Statement::Assignment(name, value) => assignment_stmt(symbols, state, name, value),
            Statement::Clear(duration, target) => clear_stmt(symbols, state, duration, target),
            Statement::Invoke(target) => invoke_stmt(symbols, state, target),
            Statement::Load(target) => load_stmt(symbols, state, target),
            Statement::Modify(modifier, target) => modify_stmt(symbols, state, modifier, target),
            Statement::Show(node) => show_stmt(symbols, state, node),
            Statement::Sequence(seq) => seq.eval(symbols, state),
            Statement::Output(expr) | Statement::Expr(expr) => expr.eval(symbols, state),
        }
    }
}

fn script_def(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    script: &RcNode<Script>,
) -> TaleResultVec<SymbolValue> {
    let name = script.eval(symbols, state)?.to_string();
    if symbols
        .borrow_mut()
        .insert(name.clone(), SymbolValue::Script(script.clone()))
    {
        Ok(SymbolValue::Script(script.clone()))
    } else {
        Ok(SymbolValue::String(format!(
            "Overwriting previous value stored in `{name}`"
        )))
    }
}

fn table_def(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    table: &RcNode<Table>,
) -> TaleResultVec<SymbolValue> {
    insert_table_def(symbols, state, table)
}

fn table_group_def(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    table_group: &RcNode<TableGroup>,
) -> TaleResultVec<SymbolValue> {
    let mut outcomes = Vec::new();
    for table in &table_group.inner_t().sub_tables {
        outcomes.push(insert_table_def(symbols, state, table)?);
    }
    outcomes.push(insert_table_def(
        symbols,
        state,
        &rc_node(table_group.inner_t().clone().into()),
    )?);
    Ok(SymbolValue::List(outcomes))
}

fn insert_table_def(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    table: &RcNode<Table>,
) -> TaleResultVec<SymbolValue> {
    let name = table.eval(symbols, state)?.to_string();
    if symbols
        .borrow_mut()
        .insert(name.clone(), SymbolValue::Table(table.clone()))
    {
        Ok(SymbolValue::Table(table.clone()))
    } else {
        Ok(SymbolValue::String(format!(
            "Overwriting previous value stored in `{name}`"
        )))
    }
}

fn assignment_stmt(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    name: &RcNode<Atom>,
    value: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let name = name.inner_t().to_string().trim_matches('`').to_string();
    let value = value.eval(symbols, state)?;
    if symbols.borrow_mut().insert(name.to_string(), value) {
        Ok(SymbolValue::Placeholder)
    } else {
        Ok(SymbolValue::String(format!(
            "Overwriting previous value stored in `{name}`"
        )))
    }
}

fn clear_stmt(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    duration: &RcNode<Duration>,
    target: &RcNode<Atom>,
) -> TaleResultVec<SymbolValue> {
    let value = target.eval(symbols, state)?;
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
    state: &StateTable,
    target: &RcNode<Atom>,
) -> TaleResultVec<SymbolValue> {
    let value = target.eval(symbols, state)?;
    roll_invoke_or_err(symbols, state, value)
}

fn load_stmt(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    target: &RcNode<Atom>,
) -> TaleResultVec<SymbolValue> {
    let mut results = Vec::new();
    let name = target.inner_t().bare_string();
    for entry in glob(&name).map_err(|err| TaleError::system(format!("Glob error: {err}")))? {
        match entry {
            Ok(path) => {
                let source = read_to_string(&path).map_err(TaleError::from)?;
                // This allows us to support relative path loading within .tale files
                let return_loc = std::env::current_dir().map_err(TaleError::from)?;
                let tale_path = path.to_string_lossy();
                let parent_dir = path.parent().ok_or(TaleError::system(format!(
                    "Error getting parent dir of: {tale_path}"
                )))?;
                if !parent_dir.display().to_string().is_empty() {
                    std::env::set_current_dir(parent_dir).map_err(TaleError::from)?;
                }
                results.push(SymbolValue::String(format!("Loading: '{}'", tale_path)));
                results.push(state.nested_pipeline(symbols, &tale_path, &source)?);
                results.push(SymbolValue::String(format!(
                    "'{}' loaded successfully!",
                    tale_path
                )));
                std::env::set_current_dir(return_loc).map_err(TaleError::from)?;
            }
            Err(err) => Err(TaleError::system(format!("Glob error: {err}")))?,
        }
    }
    if results.is_empty() {
        Err(TaleError::system(format!(
            "No such file or directory: {name}"
        )))?
    } else {
        Ok(SymbolValue::List(results))
    }
}

fn modify_stmt(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    modifier: &RcNode<Modifier>,
    target: &RcNode<Atom>,
) -> TaleResultVec<SymbolValue> {
    let value = target.eval(symbols, state)?;
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
    state: &StateTable,
    node: &RcNode<(bool, Atom)>,
) -> TaleResultVec<SymbolValue> {
    if node.inner_t().0 {
        let target = node.inner_t().1.to_lowercase();
        Ok(symbols
            .borrow()
            .get_tags(&target.to_string().split_whitespace().collect::<Vec<_>>()))
    } else {
        node.inner_t().1.eval(symbols, state)
    }
}

impl Eval for Expr {
    fn eval(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
    ) -> TaleResultVec<SymbolValue> {
        match self {
            Expr::Empty => Ok(SymbolValue::Placeholder),
            Expr::Atom(atom) => atom.eval(symbols, state),
            Expr::Neg(node) => match node.eval(symbols, state)? {
                SymbolValue::Numeric(n) => Ok(SymbolValue::Numeric(-n)),
                other => Err(vec![TaleError::evaluator(
                    node.source_span(),
                    node.position(),
                    format!("Cannot negate {other:?}"),
                )]),
            },
            Expr::Add(lhs, rhs) => add_expr(symbols, state, lhs, rhs),
            Expr::Sub(lhs, rhs) => sub_expr(symbols, state, lhs, rhs),
            Expr::Mul(lhs, rhs) => mul_expr(symbols, state, lhs, rhs),
            Expr::Div(lhs, rhs) => div_expr(symbols, state, lhs, rhs),
            Expr::Mod(lhs, rhs) => mod_expr(symbols, state, lhs, rhs),
            Expr::Pow(lhs, rhs) => pow_expr(symbols, state, lhs, rhs),
            Expr::Lookup(value, target) => lookup_expr(symbols, state, value, target),
            Expr::Roll(reps, target) => roll_expr(symbols, state, reps, target),
            Expr::Interpol(node) => interpol_expr(symbols, state, node),
            Expr::List(node) => node.eval(symbols, state),
        }
    }
}

fn add_expr(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let left_value = lhs.eval(symbols, state);
    let right_value = rhs.eval(symbols, state);

    match (left_value, right_value) {
        (Ok(SymbolValue::Numeric(l)), Ok(SymbolValue::Numeric(r))) => {
            Ok(SymbolValue::Numeric(l + r))
        }
        (Err(err), Ok(_)) | (Ok(_), Err(err)) => Err(err),
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
    state: &StateTable,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let left_value = lhs.eval(symbols, state);
    let right_value = rhs.eval(symbols, state);

    match (left_value, right_value) {
        (Ok(SymbolValue::Numeric(l)), Ok(SymbolValue::Numeric(r))) => {
            Ok(SymbolValue::Numeric(l - r))
        }
        (Err(err), Ok(_)) | (Ok(_), Err(err)) => Err(err),
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
    state: &StateTable,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let left_value = lhs.eval(symbols, state);
    let right_value = rhs.eval(symbols, state);

    match (left_value, right_value) {
        (Ok(SymbolValue::Numeric(l)), Ok(SymbolValue::Numeric(r))) => {
            Ok(SymbolValue::Numeric(l * r))
        }
        (Err(err), Ok(_)) | (Ok(_), Err(err)) => Err(err),
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
    state: &StateTable,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let left_value = lhs.eval(symbols, state);
    let right_value = rhs.eval(symbols, state);

    match (left_value, right_value) {
        (Ok(SymbolValue::Numeric(l)), Ok(SymbolValue::Numeric(r))) => {
            Ok(SymbolValue::Numeric(l / r))
        }
        (Err(err), Ok(_)) | (Ok(_), Err(err)) => Err(err),
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
    state: &StateTable,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let left_value = lhs.eval(symbols, state);
    let right_value = rhs.eval(symbols, state);

    match (left_value, right_value) {
        (Ok(SymbolValue::Numeric(l)), Ok(SymbolValue::Numeric(r))) => {
            Ok(SymbolValue::Numeric(l % r))
        }
        (Err(err), Ok(_)) | (Ok(_), Err(err)) => Err(err),
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
    state: &StateTable,
    lhs: &RcNode<Expr>,
    rhs: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let left_value = lhs.eval(symbols, state);
    let right_value = rhs.eval(symbols, state);

    match (left_value, right_value) {
        (Ok(SymbolValue::Numeric(l)), Ok(SymbolValue::Numeric(r))) => Ok(SymbolValue::Numeric(
            l.pow(u32::try_from(r).map_err(TaleError::from)?),
        )),
        (Err(err), Ok(_)) | (Ok(_), Err(err)) => Err(err),
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
    state: &StateTable,
    value: &RcNode<Expr>,
    target: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let key = value.eval(symbols, state)?;
    let target_val = target.eval(symbols, state)?;
    match &target_val {
        SymbolValue::Table(table) => table.inner_t().lookup(symbols, state, &key),
        _ => Err(vec![TaleError::evaluator(
            target.source_span(),
            target.position(),
            format!("Cannot lookup on: '{target_val}' (Not a Table or Group name)"),
        )]),
    }
}

fn roll_expr(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    reps: &RcNode<Expr>,
    target: &RcNode<Expr>,
) -> TaleResultVec<SymbolValue> {
    let reps_val = reps.eval(symbols, state)?;
    let target_val = match target.eval(symbols, state)? {
        SymbolValue::String(target_string) => symbols.borrow().get_value(&target_string),
        other => other,
    };
    match (reps_val, target_val.clone()) {
        (SymbolValue::Numeric(x), SymbolValue::Numeric(_)) => match x {
            ..=0 => Ok(SymbolValue::Placeholder),
            1..=1 => Ok(target_val),
            2.. => Ok(SymbolValue::List(
                (0..x)
                    .map(|_| target.eval(symbols, state).unwrap())
                    .collect(),
            )),
        },
        (SymbolValue::Numeric(x), target) => match x {
            ..=0 => Ok(SymbolValue::Placeholder),
            1..=1 => roll_invoke_or_err(symbols, state, target),
            2.. => Ok(SymbolValue::List(
                (0..x)
                    .map(|_| roll_invoke_or_err(symbols, state, target.clone()))
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
    state: &StateTable,
    target: SymbolValue,
) -> TaleResultVec<SymbolValue> {
    match target {
        SymbolValue::Script(script) => script.inner_t().invoke(symbols, state),
        SymbolValue::Table(table) => table.inner_t().roll_on(symbols, state),
        SymbolValue::Numeric(_) => Ok(target),
        _ => Err(vec![TaleError::evaluator(
            0..0,
            (0, 0),
            format!("No such Table or Script: '{target}'"),
        )]),
    }
}

fn interpol_expr(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    node: &RcNode<Vec<RcNode<Expr>>>,
) -> TaleResultVec<SymbolValue> {
    let (values, errors): (Vec<_>, Vec<_>) = node
        .inner_t()
        .iter()
        .map(|item| item.eval(symbols, state))
        .partition(Result::is_ok);
    if errors.is_empty() {
        Ok(values
            .into_iter()
            .fold(SymbolValue::Placeholder, |acc, value| {
                if let SymbolValue::String(acc) = acc {
                    SymbolValue::String(format!("{acc} {}", value.unwrap()))
                } else {
                    SymbolValue::String(value.unwrap().to_string())
                }
            }))
    } else {
        Err(errors.into_iter().flat_map(Result::unwrap_err).collect())
    }
}

impl Eval for Atom {
    fn eval(
        &self,
        symbols: &RefCell<SymbolTable>,
        _state: &StateTable,
    ) -> TaleResultVec<SymbolValue> {
        match self {
            #[allow(clippy::cast_possible_wrap)] // > 2 Billion numerics aren't realistic for TALE
            Atom::Number(n) => Ok(SymbolValue::Numeric(*n as isize)),
            Atom::Dice(x, y) => roll_dice(*x, *y),
            Atom::Str(s) => Ok(SymbolValue::String(s.clone())),
            Atom::Ident(id) => Ok(symbols.borrow().get_value(id)),
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
    #[allow(clippy::cast_possible_wrap)] // > 2 Billion roll total isn't realistic
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
