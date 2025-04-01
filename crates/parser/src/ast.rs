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
            Statement::Show(show) => write!(
                f,
                "Show{}({})",
                if show.actual.0 { "Tag" } else { "" },
                show.actual.1
            ),
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

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Empty => write!(f, "Empty"),
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
                    .inner()
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

#[derive(Debug, PartialEq, Eq, Clone)]
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
            Atom::Number(n) => write!(f, "Atom({})", n),
            Atom::Dice(n, s) => write!(f, "Atom({}d{})", n, s),
            Atom::Str(s) => write!(f, r#"Atom("{}")"#, s),
            Atom::Ident(s) => write!(f, "Atom({})", s),
            Atom::Raw(token) => write!(f, "Atom({})", token),
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

impl<T> Node<T> {
    pub fn inner(&self) -> &T {
        &self.actual
    }

    pub fn details(&self) -> (&MetaData, &SpanInfo, &SpanInfo) {
        (&self.meta, &self.token_span, &self.source_span)
    }
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
    name: RcNode<Atom>,
    statements: Vec<RcNode<Statement>>,
}

impl Script {
    pub fn new(name: RcNode<Atom>, statements: Vec<RcNode<Statement>>) -> Self {
        Self { name, statements }
    }
}

impl Display for Script {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}, {} Statements", self.name, self.statements.len())
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
}

impl Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}, ({}) {} Rows", self.name, self.roll, self.rows)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TableGroup {
    name: RcNode<Atom>,
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
    Flat(Vec<RcNode<Statement>>),
    Keyed(Vec<(RcNode<Expr>, RcNode<Statement>)>),
}

impl TableRows {
    pub fn calc_roll(&self) -> Expr {
        match self {
            TableRows::Empty => Expr::Empty,
            TableRows::List(atoms) => Expr::Atom(Atom::Dice(1, atoms.len())),
            TableRows::Flat(nodes) => Expr::Atom(Atom::Dice(1, nodes.len())),
            TableRows::Keyed(rows) => calc_keyed_roll(rows),
        }
    }
}

impl Display for TableRows {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let num_rows = match self {
            TableRows::Empty => 0,
            TableRows::List(atoms) => atoms.len(),
            TableRows::Flat(nodes) => nodes.len(),
            TableRows::Keyed(items) => items.len(),
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
        let (low, high) = match key_list.inner() {
            Expr::List(keys) => (
                keys.actual
                    .iter()
                    .min()
                    .unwrap_or(&Atom::Number(usize::MAX))
                    .number(),
                keys.actual
                    .iter()
                    .max()
                    .unwrap_or(&Atom::Number(usize::MIN))
                    .number(),
            ),
            _ => unreachable!(),
        };
        bottom = bottom.min(*low);
        top = top.max(*high);
    }
    let first = rows.first().unwrap().0.details();
    let last = rows.last().unwrap().0.details();
    let offset = bottom.saturating_sub(1);
    println!("{}, {}", top, offset);
    if offset != 0 {
        let die_roll = Rc::new(Node {
            actual: Expr::Atom(Atom::Dice(1, top - offset)),
            meta: MetaData::new(first.0.position),
            token_span: SpanInfo::new(first.1.context.clone(), first.1.start, last.1.end),
            source_span: SpanInfo::new(first.2.context.clone(), first.2.start, last.2.end),
        });
        let modifier = Rc::new(Node {
            actual: Expr::Atom(Atom::Number(offset)),
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
}

impl Display for Modifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.duration, self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Duration {
    All,
    Next(RcNode<Expr>),
}

impl Display for Duration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Duration::All => write!(f, "All"),
            Duration::Next(expr) => write!(f, "Next({})", expr),
        }
    }
}
