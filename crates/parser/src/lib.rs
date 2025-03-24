use std::{collections::HashMap, ops::Range};

use ast::rc_node;
use chumsky::{Parser, extra::SimpleState};
use lexer::{Lexicon, Position, Token, tokenize};
use parsers::parser;

mod ast;
mod parsers;

pub use ast::AST;

type SimpleStateTable<'src> = SimpleState<&'src mut StateTable>;

pub struct StateTable {
    current: String,
    sources: HashMap<String, String>,
    tokens: HashMap<String, Lexicon>,
    asts: HashMap<String, ast::AST>,
}

impl StateTable {
    pub fn new() -> Self {
        Self {
            current: String::new(),
            sources: HashMap::new(),
            tokens: HashMap::new(),
            asts: HashMap::new(),
        }
    }

    pub fn add_source(&mut self, name: String, source: String) {
        self.sources.insert(name, source);
    }

    pub fn lex_source(&mut self, name: String) {
        let source = self.sources.get(&name).unwrap();
        let lexicon = tokenize(source);
        // println!("{:?}", lexicon);
        self.tokens.insert(name, lexicon);
    }

    pub fn parse_source(&mut self, name: String) {
        self.current = name.clone();
        let output = {
            let tokens = self.get_tokens(&name);
            let mut parse_state = SimpleState::from(&mut *self);
            let parse_result = parser().parse_with_state(&tokens, &mut parse_state);
            match parse_result.into_output_errors() {
                (Some(output), errs) => {
                    for err in errs {
                        eprintln!("{}", err);
                    }
                    output
                }
                (None, errs) => {
                    for err in errs {
                        eprintln!("{}", err);
                    }
                    rc_node(ast::Statement::Empty)
                }
            }
        };
        // println!("{}", output);
        self.asts.insert(name, AST::new(output));
    }

    fn get_tokens(&self, name: &str) -> Vec<Token> {
        let lexicon = self.tokens.get(name).unwrap();
        let tokens: Vec<_> = lexicon.iter().map(|(token, _, _)| token.clone()).collect();
        tokens
    }

    fn spanslate(&self, span: &Range<usize>) -> (String, Range<usize>, Position) {
        // println!("{}", self.current);
        // println!("{:?}", self.tokens);
        let tokens = self.tokens.get(&self.current).unwrap();
        (
            self.current.clone(),
            tokens[span.start].1.start..tokens[span.end - 1].1.end,
            tokens[span.start].2,
        )
    }
}

impl Default for StateTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut table = StateTable::new();
        table.add_source("test".into(), "1d20 * 10 ^ 3".into());
        table.lex_source("test".into());
        table.parse_source("test".into());
        let ast = table.asts.get("test").unwrap();
        println!("{:#?}", ast);
        println!("{}", ast);
        assert!(false);
        assert_eq!(
            ast,
            &AST::new(rc_node(ast::Statement::Expr(rc_node(ast::Expr::Atom(
                ast::Atom::Dice(1, 20)
            )))))
        );
    }
}
