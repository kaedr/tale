use std::fmt::Display;
use std::{ops::Range, rc::Rc};

use chumsky::prelude::*;
use lexer::{Position, Token};

use crate::SimpleStateTable;

#[derive(Debug, PartialEq)]
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
    Clear(RcNode<Duration>, RcNode<Expr>),
    Invoke(RcNode<Atom>),
    Load(RcNode<Atom>),
    Modify(RcNode<Modifier>, RcNode<Expr>),
    Output(RcNode<Expr>),
    Show(RcNode<(bool, String)>),

    // Sequence of Statements
    Sequence(RcNode<Vec<RcNode<Self>>>),

    // Expression Statement
    Expr(RcNode<Expr>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Empty => write!(f, "Empty"),
            Statement::Script(script) => write!(f, "Script({})", script),
            Statement::Table(table) => write!(f, "Table({})", table),
            Statement::TableGroup(group) => write!(f, "TableGroup({})", group),
            Statement::Assignment(ident, expr) => write!(f, "Assignment({} = {})", ident, expr),
            Statement::Clear(duration, expr) => write!(f, "Clear({} {})", duration, expr),
            Statement::Invoke(ident) => write!(f, "Invoke({})", ident),
            Statement::Load(ident) => write!(f, "Load({})", ident),
            Statement::Modify(modifier, expr) => write!(f, "Modify({} {})", modifier, expr),
            Statement::Output(expr) => write!(f, "Output({})", expr),
            Statement::Show(show) => write!(f, "Show({})", show.actual.1),
            Statement::Sequence(statements) => {
                let statements = statements
                    .actual
                    .iter()
                    .map(|stmt| format!("{}", stmt))
                    .collect::<Vec<_>>();
                write!(f, "Sequence({})", statements.join(", "))
            }
            Statement::Expr(expr) => write!(f, "Expr({})", expr),
        }
    }
}

impl From<RcNode<Expr>> for Statement {
    fn from(value: RcNode<Expr>) -> Self {
        Self::Expr(value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
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
    Interpol(RcNode<Vec<RcNode<Statement>>>),

    // List
    List(RcNode<Vec<RcNode<Atom>>>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Atom(atom) => write!(f, "{}", atom),
            Expr::Neg(expr) => write!(f, "Neg({})", expr),
            Expr::Add(lhs, rhs) => write!(f, "Add({} + {})", lhs, rhs),
            Expr::Sub(lhs, rhs) => write!(f, "Sub({} - {})", lhs, rhs),
            Expr::Mul(lhs, rhs) => write!(f, "Mul({} * {})", lhs, rhs),
            Expr::Div(lhs, rhs) => write!(f, "Div({} / {})", lhs, rhs),
            Expr::Mod(lhs, rhs) => write!(f, "Mod({} % {})", lhs, rhs),
            Expr::Pow(lhs, rhs) => write!(f, "Pow({} ^ {})", lhs, rhs),
            Expr::Lookup(lhs, rhs) => write!(f, "Lookup({} on {})", lhs, rhs),
            Expr::Roll(lhs, rhs) => write!(f, "Roll({}, {})", lhs, rhs),
            Expr::Interpol(exprs) => {
                let exprs = exprs
                    .actual
                    .iter()
                    .map(|expr| format!("{}", expr))
                    .collect::<Vec<_>>();
                write!(f, "Interpol({})", exprs.join(", "))
            }
            Expr::List(exprs) => {
                let exprs = exprs
                    .actual
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

// impl InnerAtom for Expr {
//     fn atom(&self) -> &Atom {
//         match self {
//             Expr::Atom(atom) => atom,
//             _ => unimplemented!(),
//         }
//     }
// }

#[derive(Debug, PartialEq, Clone)]
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
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::Number(n) => write!(f, "Atom({})", n),
            Atom::Dice(n, s) => write!(f, "Atom({}d{})", n, s),
            Atom::Str(s) => write!(f, r#"Atom("{}")"#, s),
            Atom::Ident(s) => write!(f, "Atom({})", s),
            Atom::Raw(token) => write!(f, "Atom({})", token),
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
{
    let span: std::ops::Range<usize> = extra.span().into_range();
    let spanslation = extra.state().spanslate(&span);
    let full_info = (value.into(), span, spanslation);

    Rc::new(Node::from(full_info))
}

type SourceInfo = (String, Range<usize>, Position);

#[derive(Debug, PartialEq, Clone)]
pub struct Node<T> {
    actual: T,
    meta: MetaData,
    token_span: SpanInfo,
    source_span: SpanInfo,
}

impl<T> Display for Node<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.actual)
    }
}

impl<T> From<T> for Node<T> {
    fn from(value: T) -> Self {
        Self {
            actual: value,
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
            actual,
            meta: MetaData::new(position),
            token_span: SpanInfo::new(src_name.clone(), range.start, range.end),
            source_span: SpanInfo::new(src_name, src_span.start, src_span.end),
        }
    }
}

impl<T> InnerExpr for Node<T>
where
    T: InnerExpr,
{
    fn expr(&self) -> &Expr {
        self.actual.expr()
    }
}

impl<T> InnerAtom for Node<T>
where
    T: InnerAtom,
{
    fn atom(&self) -> &Atom {
        self.actual.atom()
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

#[derive(Debug, PartialEq, Clone)]
pub struct Script {
    name: String,
    statements: Vec<Statement>,
}

impl Display for Script {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Table {
    name: String,
    roll: Expr,
    tags: Vec<String>,
    modifiers: Vec<Modifier>,
    rows: TableRows,
}

impl Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TableGroup {
    name: String,
    sub_tables: Vec<RcNode<Table>>,
}

impl Display for TableGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TableRows {
    Empty,
    List(Vec<Atom>),
    Flat(Vec<Node<Statement>>),
    Keyed(Vec<(Node<Expr>, Node<Statement>)>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Modifier {
    duration: Duration,
    value: Expr,
}

impl Display for Modifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.duration, self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Duration {
    All,
    Next(Expr),
}

impl Display for Duration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Duration::All => write!(f, "All"),
            Duration::Next(expr) => write!(f, "Next({})", expr),
        }
    }
}

pub trait InnerExpr {
    fn expr(&self) -> &Expr;
}

pub trait InnerAtom {
    fn atom(&self) -> &Atom;
}
