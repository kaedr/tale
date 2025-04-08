use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::min,
    collections::{HashMap, HashSet},
    fmt::Display,
    fs::read_to_string,
    ops::Range,
};

use ast::{Analyze, Eval as _, RcNode, Script, Table, rc_node};
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
        let source = self.sources.get(&self.current).ok_or_else(|| {
            TaleError::lexer(0..0, (0, 0), format!("No source named: {}", &self.current))
        })?;
        let lexicon = tokenize(source)?;
        match self.tokens.insert(self.current.clone(), lexicon) {
            Some(_) => Err(TaleError::lexer(
                0..0,
                (0, 0),
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
                .map(|err| TaleError::parser(err.span().into_range(), (0, 0), err.to_string()))
                .collect::<Vec<_>>();
            err_breakout.iter_mut().for_each(|err| {
                err.update_span(self.get_source_span(&err.span()));
                err.update_position(self.get_source_position(&err.span()));
            });
            Err(err_breakout)
        } else {
            match self.asts.insert(self.current.clone(), AST::new(output)) {
                Some(_) => Err(TaleError::parser(
                    0..0,
                    (0, 0),
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
            TaleError::analyzer(0..0, (0, 0), format!("No source named: {}", &self.current))
        })?;
        ast.analyze(&self.symbols)
    }

    pub fn analyze_source(&mut self, name: String) -> TaleResultVec<()> {
        self.current = name.clone();
        self.analyze_current()
    }

    pub fn evaluate_current(&mut self) -> TaleResultVec<SymbolValue> {
        let ast = self.asts.get_mut(&self.current).ok_or_else(|| {
            TaleError::evaluator(0..0, (0, 0), format!("No source named: {}", &self.current))
        })?;
        ast.eval(&self.symbols)
    }

    pub fn evaluate_source(&mut self, name: String) -> TaleResultVec<SymbolValue> {
        self.current = name.clone();
        self.evaluate_current()
    }

    pub fn pipeline(&mut self, name: String, source: String) -> TaleResultVec<SymbolValue> {
        self.add_source(name, source)?;
        self.lex_current()?;
        self.parse_current()?;
        self.analyze_current()?;
        self.evaluate_current()
    }

    pub fn pipeline_file(&mut self, name: String) -> TaleResultVec<SymbolValue> {
        self.add_source_file(name)?;
        self.lex_current()?;
        self.parse_current()?;
        self.analyze_current()?;
        self.evaluate_current()
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

    fn get_source_position(&self, span: &Range<usize>) -> Position {
        if let Some(tokens) = self.tokens.get(&self.current) {
            let start = min(span.start, tokens.len().saturating_sub(1)); // Avoid out of bounds
            tokens[start].2
        } else {
            (0, 0)
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

pub struct Scopes {
    numerics: Vec<HashMap<String, isize>>,
    strings: Vec<HashMap<String, String>>,
}

impl Scopes {
    fn new() -> Self {
        Self {
            numerics: vec![HashMap::new()],
            strings: vec![HashMap::new()],
        }
    }

    fn insert(&mut self, name: String, value: SymbolValue) -> bool {
        match value {
            SymbolValue::Numeric(v) => self
                .numerics
                .last_mut()
                .expect("There must always be at least one valid scope")
                .insert(name, v)
                .is_none(),
            SymbolValue::String(v) => self
                .strings
                .last_mut()
                .expect("There must always be at least one valid scope")
                .insert(name, v)
                .is_none(),
            _ => unreachable!(
                "Attempted to insert Non Numeric/String value into Scope: {}",
                self.len()
            ),
        }
    }

    fn resolve(&self, name: &str) -> Option<SymbolValue> {
        self.numerics
            .iter()
            .zip(self.strings.iter())
            .rev()
            .find_map(|(nums, strs)| {
                if let Some(n) = nums.get(name) {
                    Some(SymbolValue::Numeric(*n))
                } else {
                    strs.get(name).map(|s| SymbolValue::String(s.clone()))
                }
            })
    }

    fn push(&mut self) {
        self.numerics.push(HashMap::new());
        self.strings.push(HashMap::new());
    }

    fn pop(&mut self) {
        if self.len() > 1 {
            self.numerics.pop();
            self.strings.pop();
        } else {
            unreachable!("Attempted to pop base scope!");
        }
    }

    fn len(&self) -> usize {
        // If the internal Vecs are somehow a different length, we should probably break
        debug_assert_eq!(self.numerics.len(), self.strings.len());
        self.numerics.len()
    }
}

pub struct SymbolTable {
    names: HashSet<String>,
    scopes: Scopes,
    scripts: HashMap<String, RcNode<Script>>,
    tables: HashMap<String, RcNode<Table>>,
    tags: HashMap<String, Vec<String>>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            names: HashSet::new(),
            scripts: HashMap::new(),
            scopes: Scopes::new(),
            tables: HashMap::new(),
            tags: HashMap::new(),
        }
    }

    /// Inserts a value into the appropriate symbol table, returns false if
    /// overwriting a previously stored value.
    fn insert(&mut self, name: String, value: SymbolValue) -> bool {
        match value {
            SymbolValue::Placeholder => self.names.insert(name),
            SymbolValue::Numeric(_) => self.scopes.insert(name, value),
            SymbolValue::String(_) => self.scopes.insert(name, value),
            SymbolValue::Script(node) => self.scripts.insert(name, node).is_none(),
            SymbolValue::Table(node) => self.tables.insert(name, node).is_none(),
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
                .map(|t| SymbolValue::String(self.get_table(&t).unwrap().to_string()))
                .collect(),
        )
    }

    pub fn get_value(&self, name: &str) -> TaleResultVec<SymbolValue> {
        if self.names.contains(name) {
            // If the name exists, return the corresponding value if it exists
            if let Some(v) = self.scopes.resolve(name) {
                Ok(v)
            } else {
                Ok(SymbolValue::String(name.to_string()))
            }
        } else {
            Err(vec![TaleError::evaluator(
                0..0,
                (0, 0),
                format!("Identifier '{name}' is not defined in the current symbol table."),
            )])
        }
    }

    pub fn show_value(&self, name: &str) -> TaleResultVec<SymbolValue> {
        if self.names.contains(name) {
            // If the name exists, return the corresponding value if it exists
            if let Some(v) = self.scopes.resolve(name) {
                Ok(v)
            } else if let Some(v) = self.scripts.get(name) {
                Ok(SymbolValue::String(v.to_string()))
            } else if let Some(v) = self.tables.get(name) {
                Ok(SymbolValue::String(v.to_string()))
            } else {
                Err(vec![TaleError::evaluator(
                    0..0,
                    (0, 0),
                    format!("No value found for: {name}"),
                )])
            }
        } else {
            Err(vec![TaleError::evaluator(
                0..0,
                (0, 0),
                format!("No value found for: {name}"),
            )])
        }
    }

    pub fn get_table(&self, name: &str) -> Option<&RcNode<Table>> {
        self.tables.get(name)
    }

    pub fn get_script(&self, name: &str) -> Option<&RcNode<Script>> {
        self.scripts.get(name)
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
                .keys()
                .map(|t| SymbolValue::String(t.clone()))
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
                .keys()
                .map(|t| SymbolValue::String(t.clone()))
                .collect();
            Ok(SymbolValue::List(tables_list))
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push();
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
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

    use crate::utils::tests::sample_path;

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
            Ok(output) => format!("{output}"),
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
    fn pipeline_full_01() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("01_table_minimal.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        assert_eq!("Ok(List([Placeholder]))", output);
    }

    #[test]
    fn pipeline_full_02() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("02_table_roll_def.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        assert_eq!("Ok(List([Placeholder]))", output);
    }

    #[test]
    fn pipeline_full_03() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("03_table_list.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        assert_eq!("Ok(List([Placeholder]))", output);
    }

    #[test]
    fn pipeline_full_04() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("04_table_keyed_numeric.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        assert_eq!("Ok(List([Placeholder]))", output);
    }

    #[test]
    fn pipeline_full_05() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("05_table_keyed_word.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        assert_eq!("Ok(List([Placeholder]))", output);
    }

    #[test]
    fn pipeline_full_06() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("06_table_group.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!(
            "{}",
            table
                .asts
                .get("/home/hcorse/Code/tale/samples/06_table_group.tale")
                .unwrap()
        );
        assert_eq!(
            "Ok(List([List([Placeholder, Placeholder, Placeholder])]))",
            output
        );
    }

    #[test]
    fn pipeline_full_10_expr() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("10_statement_expression.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert!(output.starts_with("Ok(List([Numeric("));
        assert!(output.ends_with(")]))"));
        assert_eq!(2, output.matches("Numeric(").count())
    }

    #[test]
    fn pipeline_full_11_assign() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("11_statement_assignment.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Eval"));
        assert!(output.ends_with("}])"));
        assert_eq!(3, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_11_assign_with_deps() {
        let mut table = StateTable::new();
        table.pipeline_file(
            sample_path("92_supporting_defs.tale")
                .to_string_lossy()
                .to_string(),
        );
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("11_statement_assignment.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(1, output.matches("Overwriting previous value").count());
    }

    #[test]
    fn pipeline_full_12_clear() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("12_statement_clear.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Eval"));
        assert!(output.ends_with("}])"));
        assert_eq!(4, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_12_clear_with_deps() {
        let mut table = StateTable::new();
        table.pipeline_file(
            sample_path("92_supporting_defs.tale")
                .to_string_lossy()
                .to_string(),
        );
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("12_statement_clear.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(4, output.matches("Placeholder").count());
    }

    #[test]
    fn pipeline_full_13_invoke() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("13_statement_invoke.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Eval"));
        assert!(output.ends_with("}])"));
        assert_eq!(2, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_13_invoke_with_deps() {
        let mut table = StateTable::new();
        table.pipeline_file(
            sample_path("92_supporting_defs.tale")
                .to_string_lossy()
                .to_string(),
        );
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("13_statement_invoke.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert!(output.starts_with("Ok(List([List([Placeholder"));
        assert!(output.ends_with(")]))"));
        assert_eq!(1, output.matches("Placeholder").count());
        assert_eq!(3, output.matches("List").count());
    }

    #[test]
    fn pipeline_full_14_load() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("14_statement_load.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert_eq!("", output)
    }

    #[test]
    fn pipeline_full_15_lookup() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("15_statement_lookup.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert_eq!("", output)
    }

    #[test]
    fn pipeline_full_15_lookup_with_deps() {
        let mut table = StateTable::new();
        table.pipeline_file(
            sample_path("92_supporting_defs.tale")
                .to_string_lossy()
                .to_string(),
        );
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("15_statement_lookup.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert_eq!("", output)
    }

    #[test]
    fn pipeline_full_16_modify() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("16_statement_modify.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert_eq!("", output)
    }

    #[test]
    fn pipeline_full_16_modify_with_deps() {
        let mut table = StateTable::new();
        table.pipeline_file(
            sample_path("92_supporting_defs.tale")
                .to_string_lossy()
                .to_string(),
        );
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("16_statement_modify.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert_eq!("", output)
    }

    #[test]
    fn pipeline_full_17_output() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("17_statement_output.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert_eq!("", output)
    }

    #[test]
    fn pipeline_full_18_roll() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("18_statement_roll.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert_eq!("", output)
    }

    #[test]
    fn pipeline_full_18_roll_with_deps() {
        let mut table = StateTable::new();
        table.pipeline_file(
            sample_path("92_supporting_defs.tale")
                .to_string_lossy()
                .to_string(),
        );
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("18_statement_roll.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert_eq!("", output)
    }

    #[test]
    fn pipeline_full_19_show() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("19_statement_show.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert_eq!("", output)
    }

    #[test]
    fn pipeline_full_19_show_with_deps() {
        let mut table = StateTable::new();
        table.pipeline_file(
            sample_path("92_supporting_defs.tale")
                .to_string_lossy()
                .to_string(),
        );
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("19_statement_show.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert_eq!("", output)
    }

    #[test]
    fn pipeline_full_21_scripts() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(sample_path("21_script.tale").to_string_lossy().to_string())
        );
        println!("{}", output);
        assert_eq!("", output)
    }

    #[test]
    fn pipeline_full_92_supporting_defs() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(
                sample_path("92_supporting_defs.tale")
                    .to_string_lossy()
                    .to_string()
            )
        );
        println!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(11, output.matches("Placeholder").count());
    }

    #[test]
    fn pipeline_full_93_scopes() {
        let mut table = StateTable::new();
        let output = format!(
            "{:?}",
            table.pipeline_file(sample_path("93_scoping.tale").to_string_lossy().to_string())
        );
        println!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(11, output.matches("Placeholder").count());
    }
}
