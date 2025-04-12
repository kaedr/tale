//! Contains include_str! of all the samples files
//! These can then be used easily for testing or elsewhere

pub const TABLE_MINIMAL: &str = include_str!("01_table_minimal.tale");
pub const TABLE_ROLL_DEF: &str = include_str!("02_table_roll_def.tale");
pub const TABLE_LIST: &str = include_str!("03_table_list.tale");
pub const TABLE_KEYED_NUMERIC: &str = include_str!("04_table_keyed_numeric.tale");
pub const TABLE_KEYED_WORD: &str = include_str!("05_table_keyed_word.tale");
pub const TABLE_GROUP: &str = include_str!("06_table_group.tale");

pub const STATEMENT_EXPRESSION: &str = include_str!("10_statement_expression.tale");
pub const STATEMENT_ASSIGNMENT: &str = include_str!("11_statement_assignment.tale");
pub const STATEMENT_CLEAR: &str = include_str!("12_statement_clear.tale");
pub const STATEMENT_INVOKE: &str = include_str!("13_statement_invoke.tale");
pub const STATEMENT_LOAD: &str = include_str!("14_statement_load.tale");
pub const STATEMENT_LOOKUP: &str = include_str!("15_statement_lookup.tale");
pub const STATEMENT_MODIFY: &str = include_str!("16_statement_modify.tale");
pub const STATEMENT_OUTPUT: &str = include_str!("17_statement_output.tale");
pub const STATEMENT_ROLL: &str = include_str!("18_statement_roll.tale");
pub const STATEMENT_SHOW: &str = include_str!("19_statement_show.tale");

pub const SCRIPT: &str = include_str!("21_script.tale");
pub const STRINGS: &str = include_str!("91_strings");
pub const SUPPORTING_DEFS: &str = include_str!("92_supporting_defs.tale");
pub const SCOPING: &str = include_str!("93_scoping.tale");
pub const DISTRIBUTIONS: &str = include_str!("94_distributions.tale");
pub const RECURSION: &str = include_str!("95_recursion.tale");
