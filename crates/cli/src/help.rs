//! Contains functions related to displaying help

use std::str::SplitWhitespace;

#[allow(clippy::wildcard_imports)] // Everything in snippets is a Verbosely named const
use crate::{print_arrowed, print_sidebarred, snippets::*};

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
            Topic::Lookup => flanked_example("Lookup Syntax", EXAMPLE_LOOKUP),
            Topic::Load => flanked_example("Load Syntax", EXAMPLE_LOAD),
            Topic::Show => flanked_example("Show Syntax", EXAMPLE_SHOW),
            Topic::Modify => flanked_example("Modify Syntax", EXAMPLE_MODIFY),
            Topic::Clear => flanked_example("Clear Syntax", EXAMPLE_CLEAR),
            Topic::Output => flanked_example("Output Syntax", EXAMPLE_OUTPUT),
            Topic::Assignment => flanked_example("Assignment Syntax", EXAMPLE_ASSIGNMENT),
            Topic::Topics => print!("{HELP_TOPICS}"),
            Topic::Advanced => print!("{HELP_ADVANCED}"),
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
    if let Some("scopes") = maybe_sub_topic {
        flanked_example("Script Value Scopes", EXAMPLE_SCRIPT_BASICS);
    } else {
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
    Lookup,
    Load,
    Show,
    Modify,
    Clear,
    Output,
    Assignment,
    Topics,
    Advanced,
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
            "lookup" | "lookups" => Topic::Lookup,
            "load" => Topic::Load,
            "show" => Topic::Show,
            "modify" => Topic::Modify,
            "clear" => Topic::Clear,
            "output" | "interpolation" => Topic::Output,
            "assignment" => Topic::Assignment,
            "topics" => Topic::Topics,
            "advanced" => Topic::Advanced,
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
