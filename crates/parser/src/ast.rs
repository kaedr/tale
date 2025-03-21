use std::rc::Rc;

use chumsky::Span;

pub struct AST {
    source: String,
    nodes: Node<Statement>
}

pub enum Statement{
    // Definition Statements
    Script(Node<Script>),
    Table(Node<Table>),
    TableGroup(Node<TableGroup>),

    // 'Command' Statements
    Assignment(Node<Atom>, Node<Expr>),
    Clear(Node<Duration>, Node<Expr>),
    Invoke(Node<Atom>),
    Load(Node<Atom>),
    Modify(Node<Modifier>, Node<Expr>),
    Output(Node<Expr>),
    Show(Node<(bool, String)>),
    Sequence(Node<Vec<Self>>),

    // Expression Statement
    Expr(Node<Expr>)
}

pub enum Expr{
    Atom(Node<Atom>),

    // Unary Ops
    Neg(Node<Self>),

    // Binary Ops
    Add(Node<Self>, Node<Self>),
    Sub(Node<Self>, Node<Self>),
    Mul(Node<Self>, Node<Self>),
    Div(Node<Self>, Node<Self>),
    Mod(Node<Self>, Node<Self>),
    Pow(Node<Self>, Node<Self>),

    // Keyword Exprs
    Lookup(Node<Self>, Node<Self>),
    Roll(Node<Self>, Node<Self>),

    // Interpolation
    Interpol(Node<Vec<Statement>>),

    // List
    List(Node<Vec<Atom>>),
}

#[derive(Debug, Clone)]
pub enum Atom {
    Number(usize),
    Dice(usize, usize),
    Str(String),
    Ident(String),
}

pub struct Node<T> {
    actual: Rc<T>,
    meta: MetaData,
    span: SpanInfo,
}

#[derive(Debug, Clone)]
pub struct MetaData {
    position: (usize, usize),
}

#[derive(Debug, Clone)]
pub struct SpanInfo {
    context: Context,
    start: usize,
    end: usize,
}

#[derive(Debug, Clone)]
pub enum Context {
    File(String),
    REPL,
}

impl Span for SpanInfo {
    type Context = Context;

    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            context,
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {
        self.context.clone()
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

pub struct Script{
    name: String,
    statements: Vec<Statement>
}

pub struct Table{
    name: String,
    roll: Expr,
    tags: Vec<String>,
    modifiers: Vec<Modifier>,
    rows: TableRows,
}

pub struct TableGroup{
    name: String,
    sub_tables: Vec<Table>,
}

pub enum TableRows{
    Empty,
    List(Vec<Atom>),
    Flat(Vec<Node<Statement>>),
    Keyed(Vec<(Node<Expr>, Node<Statement>)>),
}

pub struct Modifier{
    duration: Duration,
    value: Expr,
}

pub enum Duration{
    All,
    Next(Expr),
}
