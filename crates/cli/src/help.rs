//! Contains functions related to displaying help

use std::str::SplitWhitespace;

use crate::{print_arrowed, print_sidebarred, snippets::{EXAMPLE_EXPRESSIONS, EXAMPLE_ROLLS, EXAMPLE_SCRIPT_BASICS, EXAMPLE_SCRIPT_SCOPES, EXAMPLE_TABLE_BASICS, EXAMPLE_TABLE_BLOCKS, EXAMPLE_TABLE_CSV_KEYS, EXAMPLE_TABLE_GROUP_BASICS, EXAMPLE_TABLE_GROUP_STATEMENTS, EXAMPLE_TABLE_LIST, EXAMPLE_TABLE_LOOKUP, EXAMPLE_TABLE_PROBABILITIES, EXAMPLE_TABLE_STATEMENTS, EXAMPLE_TABLE_TAGS, HELP_GENERAL, HELP_TOPICS}};

pub fn help(lc_input: &str) {
    let mut help_strs = lc_input.split_whitespace();
    help_strs.next();
    if let Some(topic) = help_strs.next() {
        match Topic::from(topic) {
            // All the .help files have a trailing newline, so this is print! instead of println!
            Topic::Table => table_help(&mut help_strs),
            Topic::TableGroup => table_group_help(&mut help_strs),
            Topic::Script => script_help(&mut help_strs),
            Topic::Expression => flanked_example("Arithmetic Expressions", EXAMPLE_EXPRESSIONS),
            Topic::Roll => flanked_example("Roll Syntax", EXAMPLE_ROLLS),
            Topic::Topics => print!("{HELP_TOPICS}"),
            Topic::Unknown(topic) => {
                print_sidebarred(&format!("Uknown help topic: {topic}"));
                print_sidebarred("Type: 'help topics' for a list of valid topics");
            }
        }
    } else {
        print!("{HELP_GENERAL}");
    }
}

fn table_help(help_strs: &mut SplitWhitespace) {
    let maybe_sub_topic = help_strs.next();
    match maybe_sub_topic {
        Some("list") => flanked_example("Table List Form", EXAMPLE_TABLE_LIST),
        Some("probabilities") => {
            flanked_example("Table w/ Probabilities", EXAMPLE_TABLE_PROBABILITIES);
        }
        Some("keys") => flanked_example("Table w/ Key Variants", EXAMPLE_TABLE_CSV_KEYS),
        Some("lookup") => flanked_example("Table w/ Textual Keys", EXAMPLE_TABLE_LOOKUP),
        Some("tags") => flanked_example("Table w/ Tags", EXAMPLE_TABLE_TAGS),
        Some("statements") => flanked_example("Table w/ Statements", EXAMPLE_TABLE_STATEMENTS),
        Some("blocks") => flanked_example("Table w/ Blocks", EXAMPLE_TABLE_BLOCKS),
        Some("group") => table_group_help(help_strs),
        _ => {
            flanked_example("Table Basics", EXAMPLE_TABLE_BASICS);
            print_sidebarred("Type: 'help table list', 'help table probabilities',");
            print_sidebarred("      'help table keys', 'help table lookup', or 'help table tags'");
            print_sidebarred("      for more information.");
        }
    }
}

fn table_group_help(help_strs: &mut SplitWhitespace) {
    let maybe_sub_topic = help_strs.next();
    if let Some("statements") = maybe_sub_topic {
        flanked_example("Table Group w/ Statements", EXAMPLE_TABLE_GROUP_STATEMENTS);
    } else {
        flanked_example("Table Group Basics", EXAMPLE_TABLE_GROUP_BASICS);
        print_sidebarred("Type: 'help table group statements' for more information.");
    }
}

fn script_help(help_strs: &mut SplitWhitespace) {
    let maybe_sub_topic = help_strs.next();
    if let Some("scopes") = maybe_sub_topic { flanked_example("Script Value Scopes", EXAMPLE_SCRIPT_BASICS) } else {
        flanked_example("Script Basics", EXAMPLE_SCRIPT_SCOPES);
        print_sidebarred("Type: 'help script scopes' for more information.");
    }
}

enum Topic<'a> {
    Table,
    TableGroup,
    Script,
    Expression,
    Roll,
    Topics,
    Unknown(&'a str),
}

impl<'a> From<&'a str> for Topic<'a> {
    fn from(value: &'a str) -> Self {
        match value {
            "table" | "tables" => Topic::Table,
            "tablegroup" | "tablegroups" => Topic::TableGroup,
            "script" | "scripts" => Topic::Script,
            "expression" | "expressions" => Topic::Expression,
            "roll" | "rolls" => Topic::Roll,
            "topics" => Topic::Topics,
            other => Self::Unknown(other),
        }
    }
}

fn flanked_example(name: &str, example: &str) {
    print_arrowed(name);
    println!();
    println!("{example}");
    print_arrowed("End of Example");
}
