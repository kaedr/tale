//! Contains `include_str!` of all the examples files
//! These can then be used easily for help functions

pub const HELP_GENERAL: &str = include_str!("h01_general.help");
pub const HELP_TOPICS: &str = include_str!("h02_topics.help");
pub const HELP_ADVANCED: &str = include_str!("h03_advanced.help");

pub const EXAMPLE_TABLE_BASICS: &str = include_str!("ex01_table_basics.tale");
pub const EXAMPLE_TABLE_LIST: &str = include_str!("ex02_table_list.tale");
pub const EXAMPLE_TABLE_PROBABILITIES: &str = include_str!("ex03_table_probabilities.tale");
pub const EXAMPLE_TABLE_CSV_KEYS: &str = include_str!("ex04_table_csv_keys.tale");
pub const EXAMPLE_TABLE_LOOKUP: &str = include_str!("ex05_table_lookup.tale");
pub const EXAMPLE_TABLE_TAGS: &str = include_str!("ex06_table_tags.tale");
pub const EXAMPLE_TABLE_STATEMENTS: &str = include_str!("ex07_table_statements.tale");
pub const EXAMPLE_TABLE_BLOCKS: &str = include_str!("ex08_table_blocks.tale");

pub const EXAMPLE_TABLE_GROUP_BASICS: &str = include_str!("ex11_table_group_basics.tale");
pub const EXAMPLE_TABLE_GROUP_STATEMENTS: &str = include_str!("ex12_table_group_statements.tale");

pub const EXAMPLE_SCRIPT_BASICS: &str = include_str!("ex21_script_basics.tale");
pub const EXAMPLE_SCRIPT_SCOPES: &str = include_str!("ex22_script_scopes.tale");

pub const EXAMPLE_EXPRESSIONS: &str = include_str!("ex31_expressions.tale");
pub const EXAMPLE_ROLLS: &str = include_str!("ex32_rolls.tale");
pub const EXAMPLE_LOOKUP: &str = include_str!("ex33_lookup.tale");

pub const EXAMPLE_LOAD: &str = include_str!("ex41_load.tale");

pub const EXAMPLE_SHOW: &str = include_str!("ex51_show.tale");
pub const EXAMPLE_MODIFY: &str = include_str!("ex52_modify.tale");
pub const EXAMPLE_CLEAR: &str = include_str!("ex53_clear.tale");
pub const EXAMPLE_OUTPUT: &str = include_str!("ex54_output_interpolation.tale");
pub const EXAMPLE_ASSIGNMENT: &str = include_str!("ex55_assignment.tale");
