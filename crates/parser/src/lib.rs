use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::min,
    collections::{HashMap, HashSet},
    fmt::Display,
    fs::read_to_string,
    ops::Range,
};

use ariadne::{Color, Label, Report, ReportKind, Source};

use ast::{Analyze, RcNode, Script, SemErrors, Table, rc_node};
use chumsky::{Parser, extra::SimpleState};
use lexer::{Lexicon, Position, Token, tokenize};
use parsers::parser;

pub mod ast;
mod parsers;

pub use ast::AST;

type SimpleStateTable<'src> = SimpleState<&'src mut StateTable>;

pub struct StateTable {
    current: String,
    sources: HashMap<String, String>,
    tokens: HashMap<String, Lexicon>,
    asts: HashMap<String, ast::AST>,
    //errs: HashMap<String, Vec<Vec<Rich<'src, Token>>>>,
    symbols: RefCell<SymbolTable>,
}

impl StateTable {
    pub fn new() -> Self {
        Self {
            current: String::new(),
            sources: HashMap::new(),
            tokens: HashMap::new(),
            asts: HashMap::new(),
            //errs: HashMap::new(),
            symbols: RefCell::new(SymbolTable::new()),
        }
    }

    pub fn add_source(&mut self, name: String, source: String) {
        self.current = name.clone();
        self.sources.insert(name, source);
    }

    pub fn add_source_file(&mut self, name: String) {
        self.current = name.clone();
        let source = read_to_string(&name).unwrap();
        self.add_source(name, source);
    }

    pub fn lex_current(&mut self) {
        let source = self.sources.get(&self.current).unwrap();
        let lexicon = tokenize(source);
        self.tokens.insert(self.current.clone(), lexicon);
    }

    pub fn lex_source(&mut self, name: String) {
        self.current = name;
        self.lex_current();
    }

    pub fn parse_current(&mut self) -> String {
        let the_errs;
        let tokens = self.get_tokens(&self.current);
        let output = {
            let mut parse_state = SimpleState::from(&mut *self);
            let parse_result = parser().parse_with_state(&tokens, &mut parse_state);
            match parse_result.into_output_errors() {
                (Some(output), errs) => {
                    the_errs = errs;
                    output
                }
                (None, errs) => {
                    the_errs = errs;
                    rc_node(ast::Statement::Empty)
                }
            }
        };
        let err_string = format!("{:?}", the_errs);
        let err_breakout = the_errs
            .iter()
            .map(|err| (err.to_string(), err.span().into_range()))
            .collect::<Vec<_>>();
        for (msg, span) in err_breakout {
            let src_span = self.get_source_span(&span);
            Report::build(ReportKind::Error, (&self.current, src_span.clone()))
                .with_message(msg)
                .with_label(
                    Label::new((&self.current, src_span))
                        .with_message("Problem here")
                        .with_color(Color::Red),
                )
                .finish()
                .eprint((
                    &self.current,
                    Source::from(self.sources.get(&self.current).unwrap()),
                ))
                .unwrap();
        }
        self.asts.insert(self.current.clone(), AST::new(output));
        err_string
    }

    pub fn parse_source(&mut self, name: String) -> String {
        self.current = name.clone();
        self.parse_current()
    }

    pub fn analyze_current(&mut self) -> Result<(), SemErrors> {
        let ast = self.asts.get_mut(&self.current).unwrap();
        ast.analyze(&self.symbols)
    }

    pub fn analyze_source(&mut self, name: String) -> Result<(), SemErrors> {
        self.current = name.clone();
        self.analyze_current()
    }

    fn get_tokens(&self, name: &str) -> Vec<Token> {
        let lexicon = self.tokens.get(name).unwrap();
        let tokens: Vec<_> = lexicon.iter().map(|(token, _, _)| token.clone()).collect();
        tokens
    }

    fn get_source_span(&self, span: &Range<usize>) -> Range<usize> {
        if let Some(tokens) = self.tokens.get(&self.current) {
            let start = min(span.start, tokens.len().saturating_sub(1)); // Avoid out of bounds
            tokens[start].1.start..tokens[span.end.saturating_sub(1)].1.end
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
            if tokens.len() > 0 {
                // TODO: Clean up once https://github.com/rust-lang/rust/issues/53667
                // is stabilized
                return (
                    self.current.clone(),
                    tokens[span.start].1.start..tokens[span.end.saturating_sub(1)].1.end,
                    tokens[span.start].2,
                );
            }
        }
        (self.current.clone(), 0..0, (0, 0))
    }

    pub fn symbols(&self) -> Ref<SymbolTable> {
        self.symbols.borrow()
    }

    pub fn symbols_mut(&self) -> RefMut<SymbolTable> {
        self.symbols.borrow_mut()
    }
}

impl Default for StateTable {
    fn default() -> Self {
        Self::new()
    }
}

pub struct SymbolTable {
    names: HashSet<String>,
    numerics: HashMap<String, isize>,
    strings: HashMap<String, String>,
    scripts: HashSet<RcNode<Script>>,
    tables: HashSet<RcNode<Table>>,
    tags: HashMap<String, Vec<String>>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            names: HashSet::new(),
            numerics: HashMap::new(),
            strings: HashMap::new(),
            scripts: HashSet::new(),
            tables: HashSet::new(),
            tags: HashMap::new(),
        }
    }

    /// Inserts a value into the appropriate symbol table, returns true if
    /// overwriting a previously stored value.
    fn insert(&mut self, name: String, value: SymbolValue) -> bool {
        match value {
            SymbolValue::Placeholder => self.names.insert(name),
            SymbolValue::Numeric(v) => self.numerics.insert(name, v).is_some(),
            SymbolValue::String(v) => self.strings.insert(name, v).is_some(),
            SymbolValue::Script(node) => self.scripts.insert(node),
            SymbolValue::Table(node) => self.tables.insert(node),
            SymbolValue::List(_) => todo!(),
        }
    }

    fn register(&mut self, name: String) -> bool {
        self.insert(name, SymbolValue::Placeholder)
    }

    fn is_def(&self, name: &str) -> bool {
        self.names.contains(name)
    }

    pub fn push_tags(&mut self, tags: Vec<String>, table_name: String) {
        for tag in tags.into_iter() {
            self.tags
                .entry(tag)
                .and_modify(|v| v.push(table_name.clone()))
                .or_insert_with(|| vec![table_name.clone()]);
        }
    }

    pub fn get_value(&self, name: &str) -> Option<SymbolValue> {
        if self.names.contains(name) {
            // If the name exists, return the corresponding value if it exists
            if let Some(v) = self.numerics.get(name) {
                return Some(SymbolValue::Numeric(*v));
            }
            if let Some(v) = self.strings.get(name) {
                return Some(SymbolValue::String(v.clone()));
            }
            Some(SymbolValue::String(name.to_string()))
        } else {
            None
        }
    }

    pub fn get_table(&self, name: String) -> Option<&RcNode<Table>> {
        let value = rc_node(Table::name_only(name));
        self.tables.get(&value)
    }

    pub fn get_script(&self, name: String) -> Option<&RcNode<Script>> {
        let value = rc_node(Script::name_only(name));
        self.scripts.get(&value)
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

pub enum SymbolValue {
    Placeholder,
    Numeric(isize),
    String(String),
    Script(RcNode<Script>),
    Table(RcNode<Table>),
    List(Vec<SymbolValue>),
}

impl Display for SymbolValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolValue::Placeholder => write!(f, ""),
            SymbolValue::Numeric(n) => write!(f, "{}", n),
            SymbolValue::String(s) => write!(f, "{}", s),
            SymbolValue::Script(s) => write!(f, "{}", s),
            SymbolValue::Table(t) => write!(f, "{}", t),
            SymbolValue::List(symbol_values) => write!(
                f,
                "[{}]",
                symbol_values
                    .iter()
                    .map(SymbolValue::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use chumsky::prelude::*;

    use super::*;

    pub(crate) fn stubbed_parser<'src, T>(
        table: &'src mut StateTable,
        tokens: &'src [Token],
        parser: impl Parser<
            'src,
            &'src [Token],
            T,
            extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
        > + Clone,
    ) -> String
    where
        T: std::fmt::Display,
    {
        let mut state = SimpleState::from(table);
        match parser.parse_with_state(&tokens, &mut state).into_result() {
            Ok(output) => format!("{}", output),
            Err(err) => format!("{:?}", err),
        }
    }

    pub(crate) fn grubbed_parser<'src, T>(
        table: &'src mut StateTable,
        tokens: &'src [Token],
        parser: impl Parser<
            'src,
            &'src [Token],
            T,
            extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
        > + Clone,
    ) -> String
    where
        T: std::fmt::Debug,
    {
        let mut state = SimpleState::from(table);
        match parser.parse_with_state(&tokens, &mut state).into_result() {
            Ok(output) => format!("{:?}", output),
            Err(err) => format!("{:?}", err),
        }
    }

    #[test]
    fn it_works() {}
}
