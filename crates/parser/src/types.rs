use crate::statement::CommandSequence;

type Modifier = (Duration, isize);

pub enum Duration {
    All,
    Next(usize),
}

pub struct InterpolationSequence {
    base: String,
    replacements: Vec<CommandSequence>,
}

pub struct Script {}

pub struct Table {
    name: String,
    roll: (usize, usize),
    tags: Vec<String>,
    modifiers: Vec<Modifier>,
}

pub struct TableGroup {}
