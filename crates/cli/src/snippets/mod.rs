//! Contains include_str! of all the examples files
//! These can then be used easily for help functions

pub const HELP_GENERAL: &str = include_str!("h01_general.help");
pub const HELP_TOPICS: &str = include_str!("h02_topics.help");

pub const EXAMPLE_TABLE_BASICS: &str = include_str!("ex01_table_basics.tale");
pub const EXAMPLE_TABLE_LIST: &str = include_str!("ex02_table_list.tale");
pub const EXAMPLE_TABLE_PROBABILITIES: &str = include_str!("ex03_table_probabilities.tale");
pub const EXAMPLE_TABLE_CSV_KEYS: &str = include_str!("ex04_table_csv_keys.tale");
pub const EXAMPLE_TABLE_LOOKUP: &str = include_str!("ex05_table_lookup.tale");
pub const EXAMPLE_TABLE_TAGS: &str = include_str!("ex06_table_tags.tale");

pub const EXAMPLE_TABLE_GROUP_BASICS: &str = include_str!("ex11_table_group_basics.tale");