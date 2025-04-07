use std::cell::{Ref, RefCell, RefMut};
use std::fmt::Display;
use std::hash::Hash;
use std::{ops::Range, rc::Rc};

use crate::lexer::{Position, Token};
use chumsky::prelude::*;
use chumsky::span::Span;
use eval::Eval;

use crate::{SimpleStateTable, SymbolTable, SymbolValue};

mod analyzer;
mod eval;

pub use analyzer::Analyze;

#[derive(Debug, PartialEq, Clone)]
pub struct AST {
    nodes: RcNode<Statement>,
}

impl AST {
    pub fn new(nodes: RcNode<Statement>) -> Self {
        Self { nodes }
    }

    pub fn nodes(&self) -> &RcNode<Statement> {
        &self.nodes
    }
}

impl Display for AST {
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
        match self {
            Self::Empty => true,
            _ => false,
        }
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
            Statement::Script(script) => write!(f, "Script: {}", script),
            Statement::Table(table) => write!(f, "Table: {}", table),
            Statement::TableGroup(group) => write!(f, "TableGroup: {}", group),
            Statement::Assignment(ident, expr) => write!(f, "Assignment: {} = {}", ident, expr),
            Statement::Clear(duration, expr) => write!(f, "Clear {} {}", duration, expr),
            Statement::Invoke(ident) => write!(f, "Invoke: {}", ident),
            Statement::Load(ident) => write!(f, "Load: {}", ident),
            Statement::Modify(modifier, expr) => write!(f, "Modify {} {}", modifier, expr),
            Statement::Output(expr) => write!(f, "Output: {}", expr),
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
                    .map(|stmt| format!("{}", stmt))
                    .collect::<Vec<_>>();
                write!(f, "Sequence: [\n\t{}\n]", statements.join(",\n\t"))
            }
            Statement::Expr(expr) => write!(f, "{}", expr),
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
        match self {
            Self::Empty => true,
            _ => false,
        }
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
            Expr::Atom(atom) => write!(f, "{}", atom),
            Expr::Neg(expr) => write!(f, "-{}", expr),
            Expr::Add(lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            Expr::Sub(lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
            Expr::Mul(lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            Expr::Div(lhs, rhs) => write!(f, "({} / {})", lhs, rhs),
            Expr::Mod(lhs, rhs) => write!(f, "({} % {})", lhs, rhs),
            Expr::Pow(lhs, rhs) => write!(f, "({} ^ {})", lhs, rhs),
            Expr::Lookup(lhs, rhs) => write!(f, "Lookup {} on {}", lhs, rhs),
            Expr::Roll(lhs, rhs) => write!(f, "Roll {}, {}", lhs, rhs),
            Expr::Interpol(exprs) => {
                let exprs = exprs
                    .inner_t()
                    .iter()
                    .map(|expr| format!("{}", expr))
                    .collect::<Vec<_>>();
                write!(f, "![{}]!", exprs.join(", "))
            }
            Expr::List(exprs) => {
                let exprs = exprs
                    .inner_t()
                    .iter()
                    .map(|expr| format!("{}", expr))
                    .collect::<Vec<_>>();
                write!(f, "[{}]", exprs.join(", "))
            }
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
            Self::Dice(num, sides) => format!("{}d{}", num, sides),
            Self::Str(s) => s.to_lowercase(),
            Self::Ident(s) => s.to_lowercase(),
            Self::Raw(token) => token.to_lowercase(),
        }
    }

    pub fn range(&self, other: &Self) -> Vec<Self> {
        (*self.number()..=*other.number())
            .map(|val| Self::Number(val))
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
            Atom::Number(n) => write!(f, "{}", n),
            Atom::Dice(n, s) => write!(f, "{}d{}", n, s),
            Atom::Str(s) => write!(f, r#""{}""#, s),
            Atom::Ident(s) => write!(f, "`{}`", s),
            Atom::Raw(token) => write!(f, "{}", token),
        }
    }
}

impl PartialOrd for Atom {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Atom::Number(l), Atom::Number(r)) => l.partial_cmp(r),
            (Atom::Dice(ln, ls), Atom::Dice(rn, rs)) => (ln * ls).partial_cmp(&(rn * rs)),
            (Atom::Str(l), Atom::Str(r)) => l.partial_cmp(r),
            (Atom::Ident(l), Atom::Ident(r)) => l.partial_cmp(r),
            (Atom::Raw(l), Atom::Raw(r)) => l.partial_cmp(r),
            _ => None,
        }
    }
}

impl Ord for Atom {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if let Some(order) = self.partial_cmp(other) {
            order
        } else {
            self.to_string().cmp(&other.to_string())
        }
    }
}

pub type RcNode<T> = Rc<Node<T>>;

pub fn rc_node<T>(value: T) -> RcNode<T> {
    Rc::new(Node::from(value))
}

pub fn full_rc_node<'src, I, O>(
    value: I,
    extra: &mut chumsky::input::MapExtra<
        'src,
        '_,
        &'src [Token],
        extra::Full<Rich<'src, Token>, SimpleStateTable, ()>,
    >,
) -> RcNode<O>
where
    O: From<I>,
    I: std::fmt::Debug,
{
    //println!("full_rc_node: {:?}", value);
    let span: std::ops::Range<usize> = extra.span().into_range();
    let spanslation = extra.state().spanslate(&span);
    let full_info = (value.into(), span, spanslation);

    Rc::new(Node::from(full_info))
}

type SourceInfo = (String, Range<usize>, Position);

#[derive(Debug, Clone)]
pub struct Node<T> {
    actual: RefCell<T>,
    meta: MetaData,
    token_span: SpanInfo,
    source_span: SpanInfo,
}

impl<T> Node<T> {
    pub fn inner_t(&self) -> Ref<T> {
        self.actual.borrow()
    }

    pub fn inner_t_mut(&self) -> RefMut<T> {
        self.actual.borrow_mut()
    }

    pub fn details(&self) -> (&MetaData, &SpanInfo, &SpanInfo) {
        (&self.meta, &self.token_span, &self.source_span)
    }

    pub fn token_span(&self) -> SimpleSpan {
        SimpleSpan::new((), self.token_span.start()..self.token_span.end())
    }

    pub fn source_span(&self) -> Range<usize> {
        self.source_span.start()..self.source_span.end()
    }
}

impl<T> Hash for Node<T>
where
    T: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Hash the actual value inside the node
        let actual_value = self.inner_t();
        actual_value.hash(state);
    }
}

impl<T> Eq for Node<T> where T: PartialEq {}

impl<T> PartialEq for Node<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        // Compare the actual values inside the nodes
        *self.inner_t() == *other.inner_t()
    }
}

impl<T> Default for Node<T>
where
    T: Default,
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
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner_t())
    }
}

impl<T> From<T> for Node<T> {
    fn from(value: T) -> Self {
        Self {
            actual: RefCell::new(value),
            meta: Default::default(),
            token_span: Default::default(),
            source_span: Default::default(),
        }
    }
}

impl<T> From<(T, Range<usize>, SourceInfo)> for Node<T> {
    fn from(value: (T, Range<usize>, SourceInfo)) -> Self {
        let (actual, range, src_info) = value;
        let (src_name, src_span, position) = src_info;
        Self {
            actual: RefCell::new(actual),
            meta: MetaData::new(position),
            token_span: SpanInfo::new(src_name.clone(), range.start, range.end),
            source_span: SpanInfo::new(src_name, src_span.start, src_span.end),
        }
    }
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct MetaData {
    position: Position,
}

impl MetaData {
    fn new(position: Position) -> Self {
        Self { position }
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

#[derive(Debug, PartialEq, Clone)]
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

    pub fn invoke(&self) -> Result<SymbolValue, String> {
        todo!()
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

impl Hash for Script {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Debug, PartialEq, Clone)]
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
            roll: Default::default(),
            tags: Default::default(),
            modifiers: Default::default(),
            rows: Default::default(),
        }
    }

    pub fn name(&self) -> &RcNode<Atom> {
        &self.name
    }

    pub fn tags(&self) -> &RcNode<Vec<Atom>> {
        &self.tags
    }

    pub fn roll(&self) -> Result<SymbolValue, String> {
        todo!()
    }

    pub fn lookup(&self, key: SymbolValue) -> Result<SymbolValue, String> {
        todo!()
    }

    pub fn add_modifier(&mut self, modifier: Modifier) {
        self.modifiers.push(modifier);
    }

    pub fn clear_modifier(&mut self, duration: Duration) {
        match duration {
            Duration::All => self.modifiers.clear(),
            Duration::Next(value) => {
                let amount = value
                    .eval(&RefCell::new(SymbolTable::new()))
                    .expect("Error evaluating Clear Duration");
                if let SymbolValue::Numeric(amount) = amount {
                    self.modifiers = self
                        .modifiers
                        .iter_mut()
                        .filter_map(|modifier| match modifier.decrease_duration(amount) {
                            true => Some(modifier.clone()),
                            false => None,
                        })
                        .collect()
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

impl Hash for Table {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}, {}, {} Rows", self.name, self.roll, self.rows)
    }
}

impl From<TableGroup> for Table {
    fn from(value: TableGroup) -> Self {
        let roll = value.sub_tables.first().unwrap().inner_t().roll.clone();
        Self {
            name: value.name,
            roll,
            tags: value.tags,
            modifiers: Default::default(),
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
            writeln!(f, "\t{}", table)?;
        }
        Ok(())
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
        write!(f, "{}", num_rows)
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
    let first = rows.first().unwrap().0.details();
    let last = rows.last().unwrap().0.details();
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
            format!("+{}", value)
        };
        write!(f, "{} {}", value, self.duration)
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
                    .eval(&RefCell::new(SymbolTable::new()))
                    .expect("Error evaluating Duration");
                if let SymbolValue::Numeric(num) = value {
                    let new_value = num as isize - amount;
                    if new_value <= 0 {
                        // Duration is exhausted
                        *self = Duration::All; // Set to All
                        false
                    } else {
                        // Update the node with the new value
                        *node.inner_t_mut() = Expr::Atom(Atom::Number(new_value as usize));
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
            Duration::Next(expr) => write!(f, "Next({})", expr),
        }
    }
}
