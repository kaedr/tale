use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::min,
    collections::{HashMap, HashSet},
    fmt::Display,
    fs::read_to_string,
    ops::Range,
};

use ast::{Analyze, RcNode, Script, Table, rc_node};
use chumsky::{Parser, extra::SimpleState};
use error::{TaleError, TaleResult, TaleResultVec};
use lexer::{Lexicon, Position, Token, tokenize};
use parsers::parser;

mod ast;
mod lexer;
mod parsers;
mod utils;

pub use ast::AST;

type SimpleStateTable<'src> = SimpleState<&'src mut StateTable>;

pub mod error;

pub struct StateTable {
    current: String,
    sources: HashMap<String, String>,
    tokens: HashMap<String, Lexicon>,
    asts: HashMap<String, ast::AST>,
    //errs: HashMap<String, Vec<TaleError>>,
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

    pub fn add_source(&mut self, name: String, source: String) -> TaleResult<()> {
        self.current = name.clone();
        match self.sources.insert(name, source) {
            Some(overwritten) => Err(TaleError::system(format!(
                "Overwriting previous source: {}\nWith: {}",
                &self.current,
                overwritten.chars().take(50).collect::<Box<str>>()
            ))),
            None => Ok(()),
        }
    }

    pub fn add_source_file(&mut self, name: String) -> TaleResult<()> {
        self.current = name.clone();
        let source = read_to_string(&name)?;
        self.add_source(name, source)
    }

    pub fn lex_current(&mut self) -> TaleResultVec<()> {
        let source = self
            .sources
            .get(&self.current)
            .ok_or_else(|| TaleError::lexer(0..0, format!("No source named: {}", &self.current)))?;
        let lexicon = tokenize(source)?;
        match self.tokens.insert(self.current.clone(), lexicon) {
            Some(_) => Err(TaleError::lexer(
                0..0,
                format!("Overwriting previous lexicon of: {}", &self.current),
            )
            .into()),
            None => Ok(()),
        }
    }

    pub fn lex_source(&mut self, name: String) -> TaleResultVec<()> {
        self.current = name;
        self.lex_current()
    }

    pub fn parse_current(&mut self) -> TaleResultVec<()> {
        let the_errs;
        let tokens = self.get_tokens(&self.current)?;
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
        if !the_errs.is_empty() {
            let mut err_breakout = the_errs
                .iter()
                .map(|err| TaleError::parser(err.span().into_range(), err.to_string()))
                .collect::<Vec<_>>();
            err_breakout
                .iter_mut()
                .for_each(|err| err.span = self.get_source_span(&err.span));
            Err(err_breakout)
        } else {
            match self.asts.insert(self.current.clone(), AST::new(output)) {
                Some(_) => Err(TaleError::parser(
                    0..0,
                    format!("Overwriting previous AST of: {}", &self.current),
                )
                .into()),
                None => Ok(()),
            }
        }
    }

    pub fn parse_source(&mut self, name: String) -> TaleResultVec<()> {
        self.current = name.clone();
        self.parse_current()
    }

    pub fn analyze_current(&mut self) -> TaleResultVec<()> {
        let ast = self.asts.get_mut(&self.current).ok_or_else(|| {
            TaleError::analyzer(0..0, format!("No source named: {}", &self.current))
        })?;
        ast.analyze(&self.symbols)
    }

    pub fn analyze_source(&mut self, name: String) -> TaleResultVec<()> {
        self.current = name.clone();
        self.analyze_current()
    }

    fn get_tokens(&self, name: &str) -> TaleResult<Vec<Token>> {
        let lexicon = self.tokens.get(name).unwrap();
        let tokens: Vec<_> = lexicon.iter().map(|(token, _, _)| token.clone()).collect();
        Ok(tokens)
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

    pub fn get_tags(&self, tags: Vec<&str>) -> SymbolValue {
        let mut matched_tables = tags.iter().map(|tag| self.tags.get(*tag));
        let mut intersection = match matched_tables.next() {
            Some(Some(t)) => t.clone(),
            _ => {
                // No tags found, return empty list
                return SymbolValue::List(Vec::new());
            }
        };
        for tables in matched_tables {
            match tables {
                Some(t) => {
                    // Intersect the current intersection with the new set of tables
                    intersection = intersection
                        .iter()
                        .filter(|x| t.contains(x))
                        .cloned()
                        .collect();
                }
                None => {
                    // If any tag was not found, return empty list
                    return SymbolValue::List(Vec::new());
                }
            }
        }
        SymbolValue::List(
            intersection
                .into_iter()
                .map(|t| SymbolValue::String(self.get_table(t).unwrap().to_string()))
                .collect(),
        )
    }

    pub fn get_value(&self, name: &str) -> Result<SymbolValue, String> {
        if self.names.contains(name) {
            // If the name exists, return the corresponding value if it exists
            if let Some(v) = self.numerics.get(name) {
                return Ok(SymbolValue::Numeric(*v));
            }
            if let Some(v) = self.strings.get(name) {
                return Ok(SymbolValue::String(v.clone()));
            }
            Ok(SymbolValue::String(name.to_string()))
        } else {
            Err(format!(
                "Identifier '{}' is not defined in the current symbol table.",
                name
            ))
        }
    }

    pub fn show_value(&self, name: &str) -> Result<SymbolValue, String> {
        if self.names.contains(name) {
            // If the name exists, return the corresponding value if it exists
            if let Some(v) = self.numerics.get(name) {
                Ok(SymbolValue::Numeric(*v))
            } else if let Some(v) = self.strings.get(name) {
                Ok(SymbolValue::String(v.clone()))
            } else if let Some(v) = self.scripts.get(&rc_node(Script::name_only(name.into()))) {
                Ok(SymbolValue::String(v.to_string()))
            } else if let Some(v) = self.tables.get(&rc_node(Table::name_only(name.into()))) {
                Ok(SymbolValue::String(v.to_string()))
            } else {
                Err(format!("No value found for: {}", name))
            }
        } else {
            Err(format!("No value found for: {}", name))
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

    pub fn list_names(&self) -> Result<SymbolValue, String> {
        if self.names.is_empty() {
            Err("No identifiers are defined in the symbol table.".to_string())
        } else {
            let names_list = self
                .names
                .iter()
                .map(|n| SymbolValue::String(n.clone()))
                .collect();
            Ok(SymbolValue::List(names_list))
        }
    }

    pub fn list_scripts(&self) -> Result<SymbolValue, String> {
        if self.scripts.is_empty() {
            Err("No Tables are defined!".to_string())
        } else {
            let scripts_list = self
                .scripts
                .iter()
                .map(|t| SymbolValue::String(t.inner_t().name().to_string()))
                .collect();
            Ok(SymbolValue::List(scripts_list))
        }
    }

    pub fn list_tables(&self) -> Result<SymbolValue, String> {
        if self.tables.is_empty() {
            Err("No Tables are defined!".to_string())
        } else {
            let tables_list = self
                .tables
                .iter()
                .map(|t| SymbolValue::String(t.inner_t().name().to_string()))
                .collect();
            Ok(SymbolValue::List(tables_list))
        }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
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
#[allow(unused_must_use)]
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
