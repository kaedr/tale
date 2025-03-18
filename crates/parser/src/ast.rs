
pub struct AST {
    statements: Vec<Statement>,

}

enum Statement {
    ScriptDef(ScriptDef),
    TableDef(TableDef),
    TableGroupDef(TableGroupDef),
    CmdStatement(CommandStatement),
}

enum CommandStatement {
    Assignment(String, Expression),
    Expression(Expression),
    Clear(Duration, Expression),
    Invoke(Expression),
    Load(Expression),
    Modify(Duration, Expression, Expression),
    Output(Expression),
    Show(bool, Expression),
    Sequence(Vec<CommandStatement>),
}

enum Expression {
    Atom(Atom),
    Neg(Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Mod(Box<Expression>, Box<Expression>),
    Pow(Box<Expression>, Box<Expression>),
    Interpolation(InterpolationSequence),
    Lookup(Box<Expression>, Box<Expression>),
    Roll(Box<Expression>, Box<Expression>, Box<Expression>),
}

enum Atom {
    String(String),
    Number(usize),
    Ident(String),
    DieRoll(usize, usize),
}

enum Duration {
    All,
    Next(Expression),
}

struct InterpolationSequence {
    base: String,
    replacements: Vec<CommandStatement>,
}

struct Modifier {
    duration: Duration,
    value: Expression
}

struct ScriptDef {
    name: String,
    statements: Vec<CommandStatement>
}

enum RowKey {
    Numeric(Vec<isize>),
    Text(String),
}

enum TableRowsDef {
    List(Vec<String>),
    Flat(Vec<CommandStatement>),
    Keyed(Vec<(RowKey, CommandStatement)>),
    Empty,
}

struct TableDef {
    name: String,
    roll: Expression,
    tags: Vec<String>,
    modifiers: Vec<Modifier>,
    rows: TableRowsDef,
}

struct TableGroupDef {
    name: String,
    sub_tables: Vec<TableDef>,
}


