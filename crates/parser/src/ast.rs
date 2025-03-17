use std::rc::Rc;

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
    Neg(Rc<Expression>),
    Add(Rc<Expression>, Rc<Expression>),
    Sub(Rc<Expression>, Rc<Expression>),
    Mul(Rc<Expression>, Rc<Expression>),
    Div(Rc<Expression>, Rc<Expression>),
    Mod(Rc<Expression>, Rc<Expression>),
    Pow(Rc<Expression>, Rc<Expression>),
    Interpolation(InterpolationSequence),
    Lookup(Rc<Expression>, Rc<Expression>),
    Roll(Rc<Expression>, Rc<Expression>, Rc<Expression>),
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
    List(Vec<CommandStatement>),
    Flat(Vec<CommandStatement>),
    Keyed(Vec<(RowKey, CommandStatement)>),
}

struct TableDef {
    name: String,
    roll: Expression,
    tags: Vec<String>,
    modifiers: Vec<Modifier>,

}

struct TableGroupDef {
    name: String,
    sub_tables: Vec<String>,
}


