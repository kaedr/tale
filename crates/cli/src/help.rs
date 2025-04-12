//! Contains functions related to displaying help

use crate::{print_arrowed, print_sidebarred, snippets::{EXAMPLE_TABLE_BASICS, HELP_GENERAL, HELP_TOPICS}};

pub fn help(lc_input: &str, prefix: &str) {
    let mut help_strs = lc_input.split_whitespace();
    help_strs.next();
    if let Some(topic) = help_strs.next() {
        let maybe_sub_topic = help_strs.next();
        match Topic::from(topic) {
            // All the .help files have a trailing newline, so this is print! instead of println!
            Topic::Table => table_help(maybe_sub_topic),
            Topic::TableGroup => todo!(),
            Topic::Topics => print!("{}", HELP_TOPICS),
            Topic::Unknown(topic) => {
                println!("{}Uknown help topic: {}", prefix, topic);
                println!("{}Type: 'help topics' for a list of valid topics", prefix)
            },
        }
    } else {
        print!("{}", HELP_GENERAL);
    }
}

fn table_help(maybe_sub_topic: Option<&str>) {
    match maybe_sub_topic {
        Some(sub_topic) if sub_topic == "list" => todo!(),
        Some(sub_topic) if sub_topic == "probabilities" => todo!(),
        Some(sub_topic) if sub_topic == "keys" => todo!(),
        Some(sub_topic) if sub_topic == "lookup" => todo!(),
        Some(sub_topic) if sub_topic == "tags" => todo!(),
        _ => {
            flanked_example("Table Basics", EXAMPLE_TABLE_BASICS);
            print_sidebarred("Type: 'help table list', 'help table probabilities',");
            print_sidebarred("      'help table keys', 'help table lookup', or 'help table tags'");
            print_sidebarred("      for more information.");
        }
    }
}

enum Topic<'a> {
    Table,
    TableGroup,
    Topics,
    Unknown(&'a str),
}

impl<'a> From<&'a str> for Topic<'a> {
    fn from(value: &'a str) -> Self {
        match value {
            "table" | "tables" => Topic::Table,
            "tablegroup" | "tablegroups" => Topic::TableGroup,
            "topics" => Topic::Topics,
            other => Self::Unknown(other),
        }
    }
}

fn flanked_example(name: &str, example: &str) {
    print_arrowed(name);
    println!();
    println!("{}", example);
    print_arrowed("End of Example");
}
