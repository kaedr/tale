use std::{collections::HashMap, fs::read_to_string, ops::Range};

use ast::{RcNode, Script, Table, rc_node};
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
    symbols: SymbolTable,
}

impl StateTable {
    pub fn new() -> Self {
        Self {
            current: String::new(),
            sources: HashMap::new(),
            tokens: HashMap::new(),
            asts: HashMap::new(),
            symbols: SymbolTable::new(),
        }
    }

    pub fn add_source(&mut self, name: String, source: String) {
        self.sources.insert(name, source);
    }

    pub fn add_source_file(&mut self, name: String) {
        let source = read_to_string(&name).unwrap();
        self.add_source(name, source);
    }

    pub fn lex_source(&mut self, name: String) {
        let source = self.sources.get(&name).unwrap();
        let lexicon = tokenize(source);
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
        self.asts.insert(name, AST::new(output));
    }

    fn get_tokens(&self, name: &str) -> Vec<Token> {
        let lexicon = self.tokens.get(name).unwrap();
        let tokens: Vec<_> = lexicon.iter().map(|(token, _, _)| token.clone()).collect();
        tokens
    }

    fn get_source_span(&self, span: &Range<usize>) -> Range<usize> {
        if let Some(tokens) = self.tokens.get(&self.current) {
            tokens[span.start].1.start..tokens[span.end.saturating_sub(1)].1.end
        } else {
            0..0
        }
    }

    fn get_source_slice(&self, span: &Range<usize>) -> &str {
        let source_span = self.get_source_span(span);
        if let Some(source) = self.sources.get(&self.current) {
            &source[source_span]
        } else {
            eprintln!("NO SOURCE FOUND!");
            "NO SOURCE FOUND!"
        }
    }

    fn spanslate(&self, span: &Range<usize>) -> (String, Range<usize>, Position) {
        if let Some(tokens) = self.tokens.get(&self.current) {
            (
                self.current.clone(),
                tokens[span.start].1.start..tokens[span.end.saturating_sub(1)].1.end,
                tokens[span.start].2,
            )
        } else {
            (self.current.clone(), 0..0, (0, 0))
        }
    }
}

impl Default for StateTable {
    fn default() -> Self {
        Self::new()
    }
}

struct SymbolTable {
    numerics: HashMap<String, isize>,
    strings: HashMap<String, String>,
    scripts: HashMap<String, RcNode<Script>>,
    tables: HashMap<String, RcNode<Table>>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            numerics: HashMap::new(),
            strings: HashMap::new(),
            scripts: HashMap::new(),
            tables: HashMap::new(),
        }
    }

    /// Inserts a value into the appropriate symbol table, returns true if
    /// overwriting a previously stored value.
    fn insert(&mut self, name: String, value: SymbolValue) -> bool {
        match value {
            SymbolValue::Numeric(v) => self.numerics.insert(name, v).is_some(),
            SymbolValue::String(v) => self.strings.insert(name, v).is_some(),
            SymbolValue::Script(node) => self.scripts.insert(name, node).is_some(),
            SymbolValue::Table(node) => self.tables.insert(name, node).is_some(),
        }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

enum SymbolValue {
    Numeric(isize),
    String(String),
    Script(RcNode<Script>),
    Table(RcNode<Table>),
}

#[cfg(test)]
mod tests {
    use chumsky::{error::Simple, extra};

    use super::*;

    pub(crate) fn stubbed_parser<'src, T>(
        table: &'src mut StateTable,
        tokens: &'src [Token],
        parser: impl Parser<
            'src,
            &'src [Token],
            T,
            extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>,
        > + Clone,
    ) -> String
    where
        T: std::fmt::Display,
    {
        let mut state = SimpleState::from(table);
        let output = parser.parse_with_state(&tokens, &mut state).unwrap();
        format!("{}", output)
    }

    #[test]
    fn it_works() {}
}
