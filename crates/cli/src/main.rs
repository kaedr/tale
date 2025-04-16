use std::io;

use clap::Parser;
use help::help;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

use tale_lib::prelude::*;

mod help;
mod snippets;

#[derive(Parser)]
#[command(version, about)]
struct Cli {
    files: Vec<String>,
}

const LOAD_LINE: &str = "TALE +-  Loading:";
const WELCOME: &str = "Welcome to TALE!";
const TIP: &str =
    "Type a command to get started, 'help' if you're unsure what to do, or CTRL+C to exit.";
const PROMPT: &str = "TALE +-> ";
const SIDEBAR: &str = "     |   ";

fn print_arrowed(banner: &str) {
    let padded = format!(" {banner} ");
    println!("-----+-{padded:-<21}->");
}

fn print_sidebarred(text: &str) {
    println!("{SIDEBAR}{text}");
}

fn process_input(engine: &mut Interpreter, input: String) -> Result<()> {
    let lc_input = input.to_lowercase();
    if lc_input.starts_with("help") {
        help(lc_input.as_str());
    } else {
        engine
            .execute(SIDEBAR, &input)
            .map_err(|err| io::Error::other(format!("{err:?}")))?
            .render(SIDEBAR);
    }
    Ok(())
}

fn repl(mut engine: Interpreter) -> Result<()> {
    let mut rl = DefaultEditor::new()?;
    #[cfg(feature = "with-file-history")]
    if rl.load_history(".tale_history").is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline(PROMPT);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                process_input(&mut engine, line)?;
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL+C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL+D");
                break;
            }
            Err(err) => {
                println!("Error: {err:?}");
                break;
            }
        }
    }

    #[cfg(feature = "with-file-history")]
    if let Err(err) = rl.save_history(".tale_history") {
        eprintln!("Failed to save history file:");
        eprintln!("{err}");
    };

    Ok(())
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match Interpreter::new_with_files(SIDEBAR, &cli.files) {
        Ok(engine) => {
            for file in &cli.files {
                println!("{LOAD_LINE} {file}");
                engine
                    .render_output_of(SIDEBAR, file)
                    .map_err(|err| io::Error::other(format!("{err:?}")))?
                    .render(SIDEBAR);
            }
            print_arrowed(WELCOME);
            print_sidebarred(TIP);
            Ok(repl(engine)?)
        }
        Err(err) => Err(err.into()),
    }
}
