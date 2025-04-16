use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::{ops::Range, rc::Rc};

use crate::error::{TaleError, TaleResultVec};
use crate::lexer::{Position, Token};
use crate::parsers::Op;
use crate::state::StateTable;
use chumsky::prelude::*;
use chumsky::span::Span;
pub use eval::Eval;

use crate::{state::SimpleParserState, state::SymbolTable, state::SymbolValue};

mod analyzer;
mod eval;

pub use analyzer::Analyze;

pub trait TypedNode {
    fn node_type(&self) -> String;
}

impl<T> TypedNode for Rc<T>
where
    T: TypedNode,
{
    fn node_type(&self) -> String {
        self.as_ref().node_type()
    }
}

impl<T> TypedNode for Vec<T>
where
    T: TypedNode,
{
    fn node_type(&self) -> String {
        match self.first() {
            Some(t) => format!("Vec<{}>", t.node_type()),
            None => "Vec<Unknown>".into(),
        }
    }
}

// This is a one off, maybe there's a better way, but this is quick and easy
impl TypedNode for (bool, Atom) {
    fn node_type(&self) -> String {
        "Show Statement Target".into()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ast {
    nodes: RcNode<Statement>,
}

impl Ast {
    pub fn new(nodes: RcNode<Statement>) -> Self {
        Self { nodes }
    }

    pub fn nodes(&self) -> &RcNode<Statement> {
        &self.nodes
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.nodes)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    // Empty Statement
    Empty,

    // Definition Statements
    Script(RcNode<Script>),
    Table(RcNode<Table>),
    TableGroup(RcNode<TableGroup>),

    // 'Command' Statements
    Assignment(RcNode<Atom>, RcNode<Expr>),
    Clear(RcNode<Duration>, RcNode<Atom>),
    Invoke(RcNode<Atom>),
    Load(RcNode<Atom>),
    Modify(RcNode<Modifier>, RcNode<Atom>),
    Output(RcNode<Expr>),
    Show(RcNode<(bool, Atom)>), // TODO: Better tag support

    // Sequence of Statements
    Sequence(RcNode<Vec<RcNode<Self>>>),

    // Expression Statement
    Expr(RcNode<Expr>),
}

impl Statement {
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }
}

impl Default for Statement {
    fn default() -> Self {
        Self::Empty
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Empty => write!(f, "Empty"),
            Statement::Script(script) => write!(f, "Script: {script}"),
            Statement::Table(table) => write!(f, "Table: {table}"),
            Statement::TableGroup(group) => write!(f, "TableGroup: {group}"),
            Statement::Assignment(ident, expr) => write!(f, "Assignment: {ident} = {expr}"),
            Statement::Clear(duration, expr) => write!(f, "Clear {duration} {expr}"),
            Statement::Invoke(ident) => write!(f, "Invoke: {ident}"),
            Statement::Load(ident) => write!(f, "Load: {ident}"),
            Statement::Modify(modifier, expr) => write!(f, "Modify {modifier} {expr}"),
            Statement::Output(expr) => write!(f, "Output: {expr}"),
            Statement::Show(show) => write!(
                f,
                "Show {}{}",
                if show.inner_t().0 { "Tag " } else { "" },
                show.inner_t().1
            ),
            Statement::Sequence(statements) => {
                let statements = statements
                    .inner_t()
                    .iter()
                    .map(|stmt| format!("{stmt}"))
                    .collect::<Vec<_>>();
                write!(f, "Sequence: [\n\t{}\n]", statements.join(",\n\t"))
            }
            Statement::Expr(expr) => write!(f, "{expr}"),
        }
    }
}

impl TypedNode for Statement {
    fn node_type(&self) -> String {
        match self {
            Statement::Empty => "Empty Statement".into(),
            Statement::Script(_) => "Script Definition".into(),
            Statement::Table(_) => "Table Definition".into(),
            Statement::TableGroup(_) => "TableGroup Definition".into(),
            Statement::Assignment(_, _) => "Assignment Statement".into(),
            Statement::Clear(_, _) => "Clear Statement".into(),
            Statement::Invoke(_) => "Invoke Statement".into(),
            Statement::Load(_) => "Load Statement".into(),
            Statement::Modify(_, _) => "Modify Statement".into(),
            Statement::Output(_) => "Output Statement".into(),
            Statement::Show(_) => "Show Statement".into(),
            Statement::Sequence(_) => "Statement Sequence".into(),
            Statement::Expr(_) => "Expression Statement".into(),
        }
    }
}

impl From<RcNode<Expr>> for Statement {
    fn from(value: RcNode<Expr>) -> Self {
        Self::Expr(value)
    }
}

impl From<RcNode<TableGroup>> for Statement {
    fn from(value: RcNode<TableGroup>) -> Self {
        Statement::TableGroup(value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    // Empty Expression
    Empty,

    // Atom
    Atom(Atom),

    // Unary Ops
    Neg(RcNode<Self>),

    // Binary Ops
    Add(RcNode<Self>, RcNode<Self>),
    Sub(RcNode<Self>, RcNode<Self>),
    Mul(RcNode<Self>, RcNode<Self>),
    Div(RcNode<Self>, RcNode<Self>),
    Mod(RcNode<Self>, RcNode<Self>),
    Pow(RcNode<Self>, RcNode<Self>),

    // Keyword Exprs
    Lookup(RcNode<Self>, RcNode<Self>),
    Roll(RcNode<Self>, RcNode<Self>),

    // Interpolation
    Interpol(RcNode<Vec<RcNode<Self>>>),

    // List
    List(RcNode<Vec<Atom>>),
}

impl Expr {
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }
}

impl Default for Expr {
    fn default() -> Self {
        Self::Empty
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Empty => write!(f, "Empty"),
            Expr::Atom(atom) => write!(f, "{atom}"),
            Expr::Neg(expr) => write!(f, "-{expr}"),
            Expr::Add(lhs, rhs) => write!(f, "({lhs} + {rhs})"),
            Expr::Sub(lhs, rhs) => write!(f, "({lhs} - {rhs})"),
            Expr::Mul(lhs, rhs) => write!(f, "({lhs} * {rhs})"),
            Expr::Div(lhs, rhs) => write!(f, "({lhs} / {rhs})"),
            Expr::Mod(lhs, rhs) => write!(f, "({lhs} % {rhs})"),
            Expr::Pow(lhs, rhs) => write!(f, "({lhs} ^ {rhs})"),
            Expr::Lookup(lhs, rhs) => write!(f, "Lookup {lhs} on {rhs}"),
            Expr::Roll(lhs, rhs) => write!(f, "Roll {lhs}, {rhs}"),
            Expr::Interpol(exprs) => {
                let exprs = exprs
                    .inner_t()
                    .iter()
                    .map(|expr| format!("{expr}"))
                    .collect::<Vec<_>>();
                write!(f, "![{}]!", exprs.join(", "))
            }
            Expr::List(exprs) => {
                let exprs = exprs
                    .inner_t()
                    .iter()
                    .map(|expr| format!("{expr}"))
                    .collect::<Vec<_>>();
                write!(f, "[{}]", exprs.join(", "))
            }
        }
    }
}

impl TypedNode for Expr {
    fn node_type(&self) -> String {
        match self {
            Expr::Empty => "Empty Statement".into(),
            Expr::Atom(_) => "Atom Expression".into(),
            Expr::Neg(_) => "Negation Expression".into(),
            Expr::Add(_, _) => "Add Expression".into(),
            Expr::Sub(_, _) => "Subraction Expression".into(),
            Expr::Mul(_, _) => "Multiplication Expression".into(),
            Expr::Div(_, _) => "Division Expression".into(),
            Expr::Mod(_, _) => "Modulus Expression".into(),
            Expr::Pow(_, _) => "Exponent Expression".into(),
            Expr::Lookup(_, _) => "Lookup Expression".into(),
            Expr::Roll(_, _) => "Roll Expression".into(),
            Expr::Interpol(_) => "Interpolation Expression".into(),
            Expr::List(_) => "List Expression".into(),
        }
    }
}

impl From<Atom> for Expr {
    fn from(value: Atom) -> Self {
        Self::Atom(value)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Atom {
    Number(usize),
    Dice(usize, usize),
    Str(String),
    Ident(String),
    Raw(Token),
}

impl Atom {
    pub fn to_lowercase(&self) -> String {
        match self {
            Self::Number(num) => num.to_string(),
            Self::Dice(num, sides) => format!("{num}d{sides}"),
            Atom::Str(s) | Atom::Ident(s) => s.to_lowercase(),
            Self::Raw(token) => token.to_lowercase(),
        }
    }

    pub fn bare_string(&self) -> String {
        match self {
            Atom::Number(n) => n.to_string(),
            Atom::Dice(n, s) => format!("{n}d{s}"),
            Atom::Str(s) | Atom::Ident(s) => s.to_string(),
            Atom::Raw(token) => token.to_string(),
        }
    }

    pub fn range(&self, other: &Self) -> Vec<Self> {
        (*self.number()..=*other.number())
            .map(Self::Number)
            .collect()
    }

    pub fn number(&self) -> &usize {
        match self {
            Self::Number(inner) => inner,
            _ => unimplemented!(),
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Number(n) => write!(f, "{n}"),
            Atom::Dice(n, s) => write!(f, "{n}d{s}"),
            Atom::Str(s) => write!(f, r#""{s}""#),
            Atom::Ident(s) => write!(f, "`{s}`"),
            Atom::Raw(token) => write!(f, "{token}"),
        }
    }
}

impl TypedNode for Atom {
    fn node_type(&self) -> String {
        match self {
            Atom::Number(_) => "Number Atom".into(),
            Atom::Dice(_, _) => "Dice Atom".into(),
            Atom::Str(_) => "String Atom".into(),
            Atom::Ident(_) => "Identity Atom".into(),
            Atom::Raw(_) => "Raw Token Atom".into(),
        }
    }
}

impl PartialOrd for Atom {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Atom {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Atom::Number(l), Atom::Number(r)) => l.cmp(r),
            (Atom::Dice(ln, ls), Atom::Dice(rn, rs)) => (ln * ls).cmp(&(rn * rs)),
            (Atom::Str(l), Atom::Str(r)) | (Atom::Ident(l), Atom::Ident(r)) => l.cmp(r),
            (l, r) => l.bare_string().cmp(&r.bare_string()),
        }
    }
}

pub type RcNode<T> = Rc<Node<T>>;

pub fn rc_node<T>(value: T) -> RcNode<T>
where
    T: TypedNode,
{
    Rc::new(Node::from(value))
}

pub fn full_rc_node<'src, I, O>(
    value: I,
    extra: &mut chumsky::input::MapExtra<
        'src,
        '_,
        &'src [Token],
        extra::Full<Rich<'src, Token>, SimpleParserState, ()>,
    >,
) -> RcNode<O>
where
    O: From<I> + TypedNode,
    I: std::fmt::Debug,
{
    let span: std::ops::Range<usize> = extra.span().into_range();
    let spanslation = extra.state().spanslate(&span);
    let full_info = (value.into(), span, spanslation);

    Rc::new(Node::from(full_info))
}

pub type SourceInfo = (Range<usize>, Position);

#[derive(Debug, Clone)]
pub struct Node<T>
where
    T: TypedNode,
{
    actual: RefCell<T>,
    meta: MetaData,
    token_span: SpanInfo,
    source_span: SpanInfo,
}

impl<T> Node<T>
where
    T: TypedNode,
{
    pub fn inner_t(&self) -> Ref<T> {
        self.actual.borrow()
    }

    pub fn inner_t_mut(&self) -> RefMut<T> {
        self.actual.borrow_mut()
    }

    pub fn replace_inner_t(&self, new_inner: T) {
        self.actual.replace(new_inner);
    }

    pub fn info(&self) -> (&MetaData, &SpanInfo, &SpanInfo) {
        (&self.meta, &self.token_span, &self.source_span)
    }

    pub fn add_detail(&self, k: String, v: String) {
        self.meta.add_detail(k, v);
    }

    pub fn get_detail(&self, k: &str) -> Option<String> {
        self.meta.get_detail(k)
    }

    pub fn position(&self) -> Position {
        self.meta.position
    }

    pub fn token_span(&self) -> SimpleSpan {
        SimpleSpan::new((), self.token_span.start()..self.token_span.end())
    }

    pub fn source_span(&self) -> Range<usize> {
        self.source_span.start..self.source_span.end
    }

    pub fn merge_source_span(&self, other: &Self) -> Range<usize> {
        self.source_span.start..other.source_span.end
    }
}

impl<T> Hash for Node<T>
where
    T: Hash + TypedNode,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Hash the actual value inside the node
        let actual_value = self.inner_t();
        actual_value.hash(state);
    }
}

impl<T> Eq for Node<T> where T: PartialEq + TypedNode {}

impl<T> PartialEq for Node<T>
where
    T: PartialEq + TypedNode,
{
    fn eq(&self, other: &Self) -> bool {
        // Compare the actual values inside the nodes
        self.actual == other.actual
    }
}

impl<T> Ord for Node<T>
where
    T: Ord + TypedNode,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.actual.cmp(&other.actual)
    }
}

impl<T> PartialOrd for Node<T>
where
    T: PartialOrd + TypedNode,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.actual.partial_cmp(&other.actual)
    }
}

impl<T> Default for Node<T>
where
    T: Default + TypedNode,
{
    fn default() -> Self {
        Self {
            actual: RefCell::new(T::default()),
            meta: MetaData::new((0, 0)), // Default position
            token_span: SpanInfo::new(String::new(), 0, 0),
            source_span: SpanInfo::new(String::new(), 0, 0),
        }
    }
}

impl<T> Display for Node<T>
where
    T: Display + TypedNode,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner_t())
    }
}

impl<T> TypedNode for Node<T>
where
    T: TypedNode,
{
    fn node_type(&self) -> String {
        self.inner_t().node_type()
    }
}

impl<T> From<T> for Node<T>
where
    T: TypedNode,
{
    fn from(value: T) -> Self {
        Self {
            actual: RefCell::new(value),
            meta: MetaData::default(),
            token_span: SpanInfo::default(),
            source_span: SpanInfo::default(),
        }
    }
}

impl<T> From<(T, Range<usize>, SourceInfo)> for Node<T>
where
    T: TypedNode,
{
    fn from(value: (T, Range<usize>, SourceInfo)) -> Self {
        let (actual, range, src_info) = value;
        let (src_span, position) = src_info;
        let node_type = actual.node_type();
        Self {
            actual: RefCell::new(actual),
            meta: MetaData::new(position),
            token_span: SpanInfo::new(node_type.clone(), range.start, range.end),
            source_span: SpanInfo::new(node_type, src_span.start, src_span.end),
        }
    }
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct MetaData {
    position: Position,
    details: RefCell<HashMap<String, String>>,
}

impl MetaData {
    fn new(position: Position) -> Self {
        Self {
            position,
            ..Default::default()
        }
    }

    fn add_detail(&self, k: String, v: String) {
        self.details.borrow_mut().insert(k, v);
    }

    fn get_detail(&self, k: &str) -> Option<String> {
        self.details.borrow().get(k).cloned()
    }
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct SpanInfo {
    context: String,
    start: usize,
    end: usize,
}

impl SpanInfo {
    fn new(context: String, start: usize, end: usize) -> Self {
        Self {
            context,
            start,
            end,
        }
    }
}

impl Span for SpanInfo {
    type Context = String;

    type Offset = usize;

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            context,
            start: range.start(),
            end: range.end(),
        }
    }

    fn context(&self) -> Self::Context {
        self.context.clone()
    }
}

#[derive(Debug, Clone)]
pub struct Script {
    name: RcNode<Atom>,
    statements: Vec<RcNode<Statement>>,
}

impl Script {
    pub fn new(name: RcNode<Atom>, statements: Vec<RcNode<Statement>>) -> Self {
        Self { name, statements }
    }

    pub fn name_only(name: String) -> Self {
        Self {
            name: rc_node(Atom::Ident(name)),
            statements: Vec::new(),
        }
    }

    pub fn name(&self) -> &RcNode<Atom> {
        &self.name
    }

    pub fn invoke(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
    ) -> TaleResultVec<SymbolValue> {
        let stack_happy = symbols.borrow_mut().push_scope();
        match stack_happy {
            Ok(()) => {
                let output = self.statements.eval(symbols, state);
                symbols.borrow_mut().pop_scope();
                output
            }
            Err(()) => Err(vec![TaleError::evaluator(
                0..0,
                (0, 0),
                format!("Recursive Script: {} hit stack guard!", self.name),
            )]),
        }
    }
}

impl Display for Script {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}, {} Statement{}",
            self.name,
            self.statements.len(),
            if self.statements.len() == 1 { "" } else { "s" }
        )
    }
}

impl TypedNode for Script {
    fn node_type(&self) -> String {
        "Script Definition".into()
    }
}

impl Eq for Script {}

impl PartialEq for Script {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Ord for Script {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

impl PartialOrd for Script {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone)]
pub struct Table {
    name: RcNode<Atom>,
    roll: RcNode<Expr>,
    tags: RcNode<Vec<Atom>>,
    modifiers: Vec<Modifier>,
    rows: RcNode<TableRows>,
}

impl Table {
    pub fn new(
        name: RcNode<Atom>,
        roll: RcNode<Expr>,
        tags: RcNode<Vec<Atom>>,
        rows: RcNode<TableRows>,
    ) -> Self {
        Self {
            name,
            roll,
            tags,
            modifiers: Vec::new(),
            rows,
        }
    }

    pub fn name_only(name: String) -> Self {
        Self {
            name: rc_node(Atom::Ident(name)),
            roll: Rc::default(),
            tags: Rc::default(),
            modifiers: Vec::default(),
            rows: Rc::default(),
        }
    }

    pub fn name(&self) -> &RcNode<Atom> {
        &self.name
    }

    pub fn tags(&self) -> &RcNode<Vec<Atom>> {
        &self.tags
    }

    pub fn roll_on(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
    ) -> TaleResultVec<SymbolValue> {
        let roll_value = self.roll.eval(symbols, state)?;
        match &*self.rows.inner_t() {
            TableRows::Empty => Ok(roll_value),
            TableRows::List(atoms) => Ok(list_form_match(
                &self.roll_offset(symbols, state, roll_value)?,
                atoms,
            )),
            TableRows::Flat(nodes) => flat_form_match(
                symbols,
                state,
                &self.roll_offset(symbols, state, roll_value)?,
                nodes,
            ),
            TableRows::Keyed(items) => {
                match self
                    .rows
                    .get_detail("key_type")
                    .expect("Analyzer Bug: Missing table key type!")
                    .as_str()
                {
                    "numeric" => num_keyed_form_match(symbols, state, &roll_value, items),
                    "text" => text_keyed_form_match(
                        symbols,
                        state,
                        &self.roll_offset(symbols, state, roll_value)?,
                        items,
                    ),
                    _ => unreachable!("Analyzer Bug: Invalid table key type!"),
                }
            }
            TableRows::SubTables(nodes) => nodes
                .iter()
                .map(|node| node.inner_t().roll_on(symbols, state))
                .collect(),
        }
    }

    fn roll_offset(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
        val: SymbolValue,
    ) -> TaleResultVec<SymbolValue> {
        let offset = Self::calc_floor(symbols, state, &self.roll)?
            .operation(Op::Sub, &SymbolValue::Numeric(1))?;
        match val {
            SymbolValue::Numeric(_) => Ok(val.operation(Op::Sub, &offset)?),
            other => Ok(other),
        }
    }

    fn calc_floor(
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
        expr: &RcNode<Expr>,
    ) -> TaleResultVec<SymbolValue> {
        match &*expr.inner_t() {
            Expr::Atom(atom) => match atom {
                Atom::Number(_) | Atom::Ident(_) => atom.eval(symbols, state),
                #[allow(clippy::cast_possible_wrap)] // > 2 Billion dice isn't realistic
                Atom::Dice(x, _) => Ok(SymbolValue::Numeric(*x as isize)),
                _ => unimplemented!("calc_floor is only valid for arithmetic expressions!"),
            },
            Expr::Neg(node) => Self::calc_floor(symbols, state, node),
            Expr::Add(lhs, rhs) => Ok(Self::calc_floor(symbols, state, lhs)?
                .operation(Op::Add, &Self::calc_floor(symbols, state, rhs)?)?),
            Expr::Sub(lhs, rhs) => Ok(Self::calc_floor(symbols, state, lhs)?
                .operation(Op::Sub, &Self::calc_floor(symbols, state, rhs)?)?),
            Expr::Mul(lhs, rhs) => Ok(Self::calc_floor(symbols, state, lhs)?
                .operation(Op::Mul, &Self::calc_floor(symbols, state, rhs)?)?),
            Expr::Div(lhs, rhs) => Ok(Self::calc_floor(symbols, state, lhs)?
                .operation(Op::Div, &Self::calc_floor(symbols, state, rhs)?)?),
            Expr::Mod(lhs, rhs) => Ok(Self::calc_floor(symbols, state, lhs)?
                .operation(Op::Mod, &Self::calc_floor(symbols, state, rhs)?)?),
            Expr::Pow(lhs, rhs) => Ok(Self::calc_floor(symbols, state, lhs)?
                .operation(Op::Pow, &Self::calc_floor(symbols, state, rhs)?)?),
            _ => unimplemented!("calc_floor is only valid for arithmetic expressions!"),
        }
    }

    pub fn lookup(
        &self,
        symbols: &RefCell<SymbolTable>,
        state: &StateTable,
        key: &SymbolValue,
    ) -> TaleResultVec<SymbolValue> {
        match &*self.rows.inner_t() {
            TableRows::Empty => Ok(key.clone()),
            TableRows::List(atoms) => Ok(list_form_match(key, atoms)),
            TableRows::Flat(nodes) => flat_form_match(symbols, state, key, nodes),
            TableRows::Keyed(items) => {
                match self
                    .rows
                    .get_detail("key_type")
                    .expect("Analyzer Bug: Missing table key type!")
                    .as_str()
                {
                    "numeric" => num_keyed_form_match(symbols, state, &key, items),
                    "text" => text_keyed_form_match(symbols, state, &key, items),
                    _ => unreachable!("Analyzer Bug: Invalid table key type!"),
                }
            }
            TableRows::SubTables(nodes) => nodes
                .iter()
                .map(|node| node.inner_t().lookup(symbols, state, key))
                .collect(),
        }
    }

    pub fn add_modifier(&mut self, modifier: Modifier) {
        self.modifiers.push(modifier);
    }

    pub fn clear_modifier(&mut self, duration: Duration) {
        match duration {
            Duration::All => self.modifiers.clear(),
            Duration::Next(value) => {
                let amount = value
                    .eval(&RefCell::default(), &StateTable::default())
                    .expect("Error evaluating Clear Duration");
                if let SymbolValue::Numeric(amount) = amount {
                    self.modifiers = self
                        .modifiers
                        .iter_mut()
                        .filter_map(|modifier| {
                            modifier
                                .decrease_duration(amount)
                                .then_some(modifier.clone())
                        })
                        .collect();
                } else {
                    unreachable!(
                        "Duration::Next should only contain numeric values, found: {:?}",
                        value
                    );
                }
            }
        }
    }
}

#[allow(clippy::cast_sign_loss)] // *n < 1 match guard case prevents this
fn list_form_match(key: &SymbolValue, atoms: &[Atom]) -> SymbolValue {
    match key {
        SymbolValue::Numeric(n) if *n < 1 => SymbolValue::String(atoms[0].to_string()),
        SymbolValue::Numeric(n) if *n > 0 && (*n as usize) < atoms.len() => {
            SymbolValue::String(atoms[*n as usize - 1].to_string())
        }
        #[allow(clippy::cast_possible_wrap)] // > 2 Billion rows isn't realistic
        SymbolValue::Numeric(n) if *n >= atoms.len() as isize => {
            SymbolValue::String(atoms.last().unwrap().to_string())
        }
        _ => SymbolValue::Placeholder,
    }
}

#[allow(clippy::cast_sign_loss)] // *n < 1 match guard case prevents this
fn flat_form_match(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    key: &SymbolValue,
    nodes: &[RcNode<Statement>],
) -> TaleResultVec<SymbolValue> {
    match key {
        SymbolValue::Numeric(n) if *n < 1 => nodes[0].eval(symbols, state),
        SymbolValue::Numeric(n) if *n > 0 && (*n as usize) < nodes.len() => {
            nodes[*n as usize - 1].eval(symbols, state)
        }
        #[allow(clippy::cast_possible_wrap)] // > 2 Billion rows isn't realistic
        SymbolValue::Numeric(n) if *n >= nodes.len() as isize => {
            nodes.last().unwrap().eval(symbols, state)
        }
        _ => Ok(SymbolValue::Placeholder),
    }
}

fn num_keyed_form_match(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    key: &SymbolValue,
    items: &[(RcNode<Expr>, RcNode<Statement>)],
) -> TaleResultVec<SymbolValue> {
    match key {
        SymbolValue::Numeric(key) => {
            let mut closest = (usize::MAX, 0usize);
            for (idx, (row_keys, _)) in items.iter().enumerate() {
                match row_keys.eval(symbols, state)? {
                    SymbolValue::List(candidates) => {
                        let closest_this_row = num_key_matcher(*key, candidates);
                        closest = if closest.0 > closest_this_row {
                            (closest_this_row, idx)
                        } else {
                            closest
                        }
                    }
                    other => unreachable!(
                        "Analyzer/Evaluator Bug: table key type mismatch! ({})",
                        other
                    ),
                }
            }
            items[closest.1].1.eval(symbols, state)
        }
        _ => Ok(SymbolValue::Placeholder),
    }
}

fn num_key_matcher(key: isize, candidates: Vec<SymbolValue>) -> usize {
    let mut distance = usize::MAX;
    for candidate in candidates {
        match candidate {
            SymbolValue::Numeric(n) => {
                let new_distance = (n - key).unsigned_abs();
                if new_distance < distance {
                    distance = new_distance;
                }
            }
            other => unreachable!(
                "Analyzer/Evaluator Bug: table key type mismatch! ({})",
                other
            ),
        }
    }
    distance
}

#[allow(clippy::cast_sign_loss)] // *n < 1 match guard case prevents this
fn text_keyed_form_match(
    symbols: &RefCell<SymbolTable>,
    state: &StateTable,
    key: &SymbolValue,
    items: &[(RcNode<Expr>, RcNode<Statement>)],
) -> TaleResultVec<SymbolValue> {
    match key {
        SymbolValue::String(key) => {
            for (row_keys, stmt) in items {
                match row_keys.eval(symbols, state)? {
                    SymbolValue::String(candidate) => {
                        if key == &candidate {
                            return stmt.eval(symbols, state);
                        }
                    }
                    other => unreachable!(
                        "Analyzer/Evaluator Bug: table key type mismatch! ({})",
                        other
                    ),
                }
            }
            Ok(SymbolValue::Placeholder)
        }
        SymbolValue::Numeric(n) if *n < 1 => items[0].1.eval(symbols, state),
        SymbolValue::Numeric(n) if *n > 0 && (*n as usize) < items.len() => {
            items[*n as usize - 1].1.eval(symbols, state)
        }
        #[allow(clippy::cast_possible_wrap)] // > 2 Billion rows isn't realistic
        SymbolValue::Numeric(n) if *n >= items.len() as isize => {
            items.last().unwrap().1.eval(symbols, state)
        }
        _ => Ok(SymbolValue::Placeholder),
    }
}

impl Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}, {}, {} Rows", self.name, self.roll, self.rows)
    }
}

impl TypedNode for Table {
    fn node_type(&self) -> String {
        "Table Definition".into()
    }
}

impl Eq for Table {}

impl PartialEq for Table {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Ord for Table {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

impl PartialOrd for Table {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl From<TableGroup> for Table {
    fn from(value: TableGroup) -> Self {
        let roll = value.sub_tables.first().unwrap().inner_t().roll.clone();
        Self {
            name: value.name,
            roll,
            tags: value.tags,
            modifiers: Vec::default(),
            rows: rc_node(TableRows::SubTables(value.sub_tables)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TableGroup {
    name: RcNode<Atom>,
    tags: RcNode<Vec<Atom>>,
    sub_tables: Vec<RcNode<Table>>,
}

impl TableGroup {
    pub fn new(
        name: RcNode<Atom>,
        tags: RcNode<Vec<Atom>>,
        sub_tables: Vec<RcNode<Table>>,
    ) -> Self {
        Self {
            name,
            tags,
            sub_tables,
        }
    }

    pub fn name(&self) -> &RcNode<Atom> {
        &self.name
    }

    pub fn tags(&self) -> &RcNode<Vec<Atom>> {
        &self.tags
    }
}

impl Display for TableGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.name)?;
        for table in &self.sub_tables {
            writeln!(f, "\t{table}")?;
        }
        Ok(())
    }
}

impl TypedNode for TableGroup {
    fn node_type(&self) -> String {
        "Table Group Definition".into()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TableRows {
    Empty,
    List(Vec<Atom>),
    Flat(Vec<RcNode<Statement>>),
    Keyed(Vec<(RcNode<Expr>, RcNode<Statement>)>),
    SubTables(Vec<RcNode<Table>>), // For TableGroup storage in symbol table
}

impl TableRows {
    pub fn calc_roll(&self) -> Expr {
        match self {
            TableRows::Empty => Expr::Empty,
            TableRows::List(atoms) => Expr::Atom(Atom::Dice(1, atoms.len())),
            TableRows::Flat(nodes) => Expr::Atom(Atom::Dice(1, nodes.len())),
            TableRows::Keyed(rows) => calc_keyed_roll(rows),
            TableRows::SubTables(nodes) => nodes.first().unwrap().inner_t().roll.inner_t().clone(),
        }
    }
}

impl Default for TableRows {
    fn default() -> Self {
        Self::Empty
    }
}

impl Display for TableRows {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let num_rows = match self {
            TableRows::Empty => 0,
            TableRows::List(atoms) => atoms.len(),
            TableRows::Flat(stmts) => stmts.len(),
            TableRows::Keyed(items) => items.len(),
            TableRows::SubTables(nodes) => nodes.len(),
        };
        write!(f, "{num_rows}")
    }
}

impl TypedNode for TableRows {
    fn node_type(&self) -> String {
        match self {
            TableRows::Empty => "Empty Table Rows".into(),
            TableRows::List(_) => "List Form Table Rows".into(),
            TableRows::Flat(_) => "Flat Table Rows".into(),
            TableRows::Keyed(_) => "Keyed Table Rows".into(),
            TableRows::SubTables(_) => "Sub Tables Definition".into(),
        }
    }
}

impl From<Vec<Atom>> for TableRows {
    fn from(value: Vec<Atom>) -> Self {
        TableRows::List(value)
    }
}

fn calc_keyed_roll<T>(rows: &Vec<(RcNode<Expr>, T)>) -> Expr {
    let (mut bottom, mut top) = (usize::MAX, usize::MIN);
    for (key_list, _) in rows {
        let key_list = key_list.inner_t();
        let (low, high) = match &*key_list {
            Expr::List(keys) => (
                *keys
                    .inner_t()
                    .iter()
                    .min()
                    .unwrap_or(&Atom::Number(usize::MAX))
                    .number(),
                *keys
                    .inner_t()
                    .iter()
                    .max()
                    .unwrap_or(&Atom::Number(usize::MIN))
                    .number(),
            ),
            _ => return Expr::Atom(Atom::Dice(1, rows.len())),
        };
        bottom = bottom.min(low);
        top = top.max(high);
    }
    let first = rows.first().unwrap().0.info();
    let last = rows.last().unwrap().0.info();
    let offset = bottom.saturating_sub(1);
    if offset != 0 {
        let die_roll = Rc::new(Node {
            actual: Expr::Atom(Atom::Dice(1, top - offset)).into(),
            meta: MetaData::new(first.0.position),
            token_span: SpanInfo::new(first.1.context.clone(), first.1.start, last.1.end),
            source_span: SpanInfo::new(first.2.context.clone(), first.2.start, last.2.end),
        });
        let modifier = Rc::new(Node {
            actual: Expr::Atom(Atom::Number(offset)).into(),
            meta: MetaData::new(first.0.position),
            token_span: SpanInfo::new(first.1.context.clone(), first.1.start, last.1.end),
            source_span: SpanInfo::new(first.2.context.clone(), first.2.start, last.2.end),
        });
        Expr::Add(die_roll, modifier)
    } else {
        Expr::Atom(Atom::Dice(1, top))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Modifier {
    duration: Duration,
    value: RcNode<Expr>,
}

impl Modifier {
    pub fn new(duration: Duration, value: RcNode<Expr>) -> Self {
        Self { duration, value }
    }

    /// Decreases the duration by the specified amount
    /// Returns true if there is duration remaining
    fn decrease_duration(&mut self, amount: isize) -> bool {
        // Decrease the duration by the specified amount
        self.duration.decrease(amount)
    }
}

impl Display for Modifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = format!("{}", self.value);
        let value = if value.starts_with('-') {
            value
        } else {
            format!("+{value}")
        };
        write!(f, "{value} {}", self.duration)
    }
}

impl TypedNode for Modifier {
    fn node_type(&self) -> String {
        "Modifier".into()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Duration {
    All,
    Next(RcNode<Expr>),
}

impl Duration {
    /// Decreases the duration by the specified amount
    /// Returns true if there is duration remaining
    fn decrease(&mut self, amount: isize) -> bool {
        match self {
            Duration::All => true,
            Duration::Next(node) => {
                let value = node
                    .eval(&RefCell::default(), &StateTable::default())
                    .expect("Error evaluating Duration");
                if let SymbolValue::Numeric(num) = value {
                    let new_value = num - amount;
                    if new_value <= 0 {
                        // Duration is exhausted
                        *self = Duration::All; // Set to All
                        false
                    } else {
                        // Update the node with the new value
                        #[allow(clippy::cast_sign_loss)] // Above conditional guards against this
                        node.replace_inner_t(Expr::Atom(Atom::Number(new_value as usize)));
                        true // Duration remains
                    }
                } else {
                    unreachable!(
                        "Duration::Next should only contain numeric values, found: {:?}",
                        value
                    );
                }
            }
        }
    }
}

impl Display for Duration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Duration::All => write!(f, "All"),
            Duration::Next(expr) => write!(f, "Next({expr})"),
        }
    }
}

impl TypedNode for Duration {
    fn node_type(&self) -> String {
        "Duration".into()
    }
}
