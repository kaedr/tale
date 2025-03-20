use std::rc::Rc;

use chumsky::Span;

pub struct AST<'src> {
    nodes: Vec<Node<'src, Statement<'src>>>
}

pub enum Statement<'src> {
    // Definition Statements
    Script(Node<'src, Script<'src>>),
    Table(Node<'src, Table<'src>>),
    TableGroup(Node<'src, TableGroup<'src>>),

    // 'Command' Statements
    Assignment(Node<'src, Atom<'src>>, Node<'src, Expr<'src>>),
    Clear(Node<'src, Duration<'src>>, Node<'src, Expr<'src>>),
    Invoke(Node<'src, Atom<'src>>),
    Load(Node<'src, Atom<'src>>),
    Modify(Node<'src, Modifier<'src>>, Node<'src, Expr<'src>>),
    Output(Node<'src, Expr<'src>>),
    Show(Node<'src, (bool, String)>),
    Sequence(Node<'src, Vec<Self>>),

    // Expression Statement
    Expr(Node<'src, Expr<'src>>)
}

pub enum Expr<'src> {
    Atom(Node<'src, Atom<'src>>),

    // Unary Ops
    Neg(Node<'src, Self>),

    // Binary Ops
    Add(Node<'src, Self>, Node<'src, Self>),
    Sub(Node<'src, Self>, Node<'src, Self>),
    Mul(Node<'src, Self>, Node<'src, Self>),
    Div(Node<'src, Self>, Node<'src, Self>),
    Mod(Node<'src, Self>, Node<'src, Self>),
    Pow(Node<'src, Self>, Node<'src, Self>),

    // Keyword Exprs
    Lookup(Node<'src, Self>, Node<'src, Self>),
    Roll(Node<'src, Self>, Node<'src, Self>),

    // Interpolation
    Interpol(Node<'src, Vec<Statement<'src>>>),

    // List
    List(Node<'src, Vec<Atom<'src>>>),
}

pub enum Atom<'src> {
    Number(usize),
    Dice(usize, usize),
    Str(&'src str),
    Ident(String),
}

pub struct Node<'src, T> {
    actual: Rc<T>,
    meta: MetaData<'src>,
    span: SpanInfo<'src>,
}

#[derive(Debug, Clone)]
pub struct MetaData<'src> {
    content: &'src str,
    position: (usize, usize),
}

#[derive(Debug, Clone)]
pub struct SpanInfo<'src> {
    context: &'src str,
    start: usize,
    end: usize,
}

impl<'src> Span for SpanInfo<'src> {
    type Context = &'src str;

    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            context,
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {
        self.context
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

pub struct Script<'src> {
    name: String,
    statements: Vec<Statement<'src>>
}

pub struct Table<'src> {
    name: String,
    roll: Expr<'src>,
    tags: Vec<String>,
    modifiers: Vec<Modifier<'src>>,
    rows: TableRows<'src>,
}

pub struct TableGroup<'src> {
    name: String,
    sub_tables: Vec<Table<'src>>,
}

pub enum TableRows<'src> {
    Empty,
    List(Vec<Atom<'src>>),
    Flat(Vec<Node<'src, Statement<'src>>>),
    Keyed(Vec<(Node<'src, Expr<'src>>, Node<'src, Statement<'src>>)>),
}

pub struct Modifier<'src> {
    duration: Duration<'src>,
    value: Expr<'src>,
}

pub enum Duration<'src> {
    All,
    Next(Expr<'src>),
}