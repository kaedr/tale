use std::io;

use clap::Parser;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

use tale_lib::prelude::*;

#[derive(Parser)]
#[command(version, about)]
struct CLI {
    files: Vec<String>,
}

const LOAD_LINE: &str = "TALE +- Loading:";
const PROMPT: &str = "TALE +-> ";
const SIDEBAR: &str = "     |   ";

fn process_input(engine: &mut Interpreter, input: String) -> Result<()> {
    match input.to_lowercase().as_str() {
        "help" => println!("{}TODO: Help messages!", SIDEBAR),
        _ => engine
            .execute(SIDEBAR, input)
            .map_err(|err| io::Error::other(format!("-{:?}", err)))?
            .render(SIDEBAR),
    };
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
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    #[cfg(feature = "with-file-history")]
    rl.save_history(".tale_history");

    Ok(())
}

fn main() -> Result<()> {
    let cli = CLI::parse();

    match Interpreter::new_with_files(&cli.files) {
        Ok(engine) => {
            for file in &cli.files {
                println!("{LOAD_LINE} {file}");
                engine
                    .render_output_of(SIDEBAR, file)
                    .map_err(|err| io::Error::other(format!("--{:?}", err)))?
                    .render(SIDEBAR);
            }
            Ok(repl(engine)?)
        }
        Err(err) => Err(err.into()),
    }
}
