use chumsky::Span;

pub struct AST<'src> {
    statements: Vec<Statement<'src>>,
}

enum Statement<'src> {
    ScriptDef(ScriptDef<'src>),
    TableDef(TableDef<'src>),
    TableGroupDef(TableGroupDef<'src>),
    CmdStatement(CommandStatement<'src>),
}

struct ScriptDef<'src> {
    name: String,
    statements: Vec<CommandStatement<'src>>,
}

struct TableDef<'src> {
    name: String,
    roll: Expression<'src>,
    tags: Vec<String>,
    modifiers: Vec<Modifier<'src>>,
    rows: TableRowsDef<'src>,
}

enum TableRowsDef<'src> {
    List(Vec<String>),
    Flat(Vec<CommandStatement<'src>>),
    Keyed(Vec<(RowKey, CommandStatement<'src>)>),
    Empty,
}

enum RowKey {
    Numeric(NumericKey),
    Text(String),
}

struct NumericKey {}

struct TableGroupDef<'src> {
    name: String,
    sub_tables: Vec<TableDef<'src>>,
}

enum CommandStatement<'src> {
    Assignment(String, Expression<'src>),
    Expression(Expression<'src>),
    Clear(Duration<'src>, Expression<'src>),
    Invoke(Expression<'src>),
    Load(Expression<'src>),
    Modify(Duration<'src>, Expression<'src>, Expression<'src>),
    Output(Expression<'src>),
    Show(bool, Expression<'src>),
    Sequence(Vec<CommandStatement<'src>>),
}

enum Expression<'src> {
    Atom(Atom<'src>),
    Neg(Box<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    Mod(Box<Expression<'src>>, Box<Expression<'src>>),
    Pow(Box<Expression<'src>>, Box<Expression<'src>>),
    Lookup(Box<Expression<'src>>, Box<Expression<'src>>),
    Roll(
        Box<Expression<'src>>,
        Box<Expression<'src>>,
        Box<Expression<'src>>,
    ),
}

enum Atom<'src> {
    String(StringAtom<'src>),
    Number(usize),
    Ident(String),
    DieRoll(usize, usize),
    Interpolation(InterpolationSequence<'src>),
}

struct StringAtom<'src>(&'src str, MetaData<'src>);

struct NumberAtom<'src>(usize, MetaData<'src>);

struct IdentAtom<'src>(String, MetaData<'src>);

struct DieRollAtom<'src>(usize, usize, MetaData<'src>);

struct InterpolationAtom<'src>(InterpolationSequence<'src>, MetaData<'src>);

enum Duration<'src> {
    All,
    Next(Expression<'src>),
}

struct InterpolationSequence<'src> {
    base: String,
    replacements: Vec<CommandStatement<'src>>,
    meta: MetaData<'src>,
}

struct Modifier<'src> {
    duration: Duration<'src>,
    value: Expression<'src>,
}

#[derive(Debug, Clone)]
struct MetaData<'src> {
    context: &'src str,
    content: &'src str,
    start: usize,
    end: usize,
}

impl<'src> Span for MetaData<'src> {
    type Context = &'src str;

    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        MetaData {
            context,
            content: "",
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
