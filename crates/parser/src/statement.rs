use std::rc::Rc;

use crate::{expression::Expression, types::*};

pub enum Statement {
    ScriptDef(Script),
    TableDef(Table),
    TableGroupDef(TableGroup),
    CmdStatement(CommandStatement),
    CmdSequence(CommandSequence),
}

pub enum CommandStatement {
    AssignmentStatement,
    ExpressionStatement(Rc<Expression>),
    ClearStatement(Duration, String),
    InvokeStatement(String),
    LoadStatement(String),
    LookupStatement(String, String),
    ModifyStatement(Duration, String, isize),
    OutputStatement,
    RollStatement,
    ShowStatement,
}

pub type CommandSequence = Vec<CommandStatement>;
