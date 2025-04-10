use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::min,
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
    io::Write,
    ops::Range,
};

use chumsky::{Parser, extra::SimpleState};

use crate::{
    ast::{RcNode, Script, Statement, Table, rc_node},
    lexer::{Lexicon, Position, Token, tokenize},
    parsers::parser,
};

use crate::error::TaleResultVec;

use crate::error::TaleError;

use crate::error::TaleResult;

use crate::ast::{AST, Analyze, Eval as _};

pub type SimpleStateTable<'src> = SimpleState<&'src mut StateTable>;

#[derive(Debug)]
pub struct StateTable {
    current: String,
    sources: HashMap<String, String>,
    tokens: HashMap<String, Lexicon>,
    asts: HashMap<String, AST>,
    outputs: HashMap<String, TaleResultVec<SymbolValue>>,
    symbols: RefCell<SymbolTable>,
}

impl StateTable {
    pub fn new() -> Self {
        Self {
            current: String::new(),
            sources: HashMap::new(),
            tokens: HashMap::new(),
            asts: HashMap::new(),
            outputs: HashMap::new(),
            symbols: RefCell::new(SymbolTable::new()),
        }
    }

    pub fn current(&self) -> &str {
        &self.current
    }

    pub fn current_output(&self) -> Option<&TaleResultVec<SymbolValue>> {
        self.outputs.get(&self.current)
    }

    pub fn symbols(&self) -> Ref<SymbolTable> {
        self.symbols.borrow()
    }

    pub fn symbols_mut(&self) -> RefMut<SymbolTable> {
        self.symbols.borrow_mut()
    }

    pub fn asts(&self) -> &HashMap<String, AST> {
        &self.asts
    }

    pub fn add_source(&mut self, name: String, source: String) -> TaleResult<()> {
        self.current = name.clone();
        match self.sources.insert(name, source) {
            Some(overwritten) => Err(TaleError::system(format!(
                "Attempted to overwrite previous source: {}\nWith: {}",
                &self.current,
                overwritten.chars().take(50).collect::<Box<str>>()
            ))),
            None => Ok(()),
        }
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
                format!(
                    "Attempted to overwrite previous lexicon of: {}",
                    &self.current
                ),
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
                    rc_node(Statement::Empty)
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
                    format!("Attempted to overwrite previous AST of: {}", &self.current),
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

    pub fn captured_pipeline(&mut self, name: String, source: String) {
        let output = self.pipeline(name, source);
        self.outputs.insert(self.current.clone(), output);
    }

    pub fn get_tokens(&self, name: &str) -> TaleResult<Vec<Token>> {
        let lexicon = self.tokens.get(name).unwrap();
        let tokens: Vec<_> = lexicon.iter().map(|(token, _, _)| token.clone()).collect();
        Ok(tokens)
    }

    pub fn get_source_span(&self, span: &Range<usize>) -> Range<usize> {
        if let Some(tokens) = self.tokens.get(&self.current) {
            let start = min(span.start, tokens.len().saturating_sub(1)); // Avoid out of bounds
            tokens[start].1.start..tokens[span.end.saturating_sub(1)].1.end
        } else {
            0..0
        }
    }

    pub fn get_source_position(&self, span: &Range<usize>) -> Position {
        if let Some(tokens) = self.tokens.get(&self.current) {
            let start = min(span.start, tokens.len().saturating_sub(1)); // Avoid out of bounds
            tokens[start].2
        } else {
            (0, 0)
        }
    }

    pub fn get_source_slice(&self, span: &Range<usize>) -> &str {
        let source_span = self.get_source_span(span);
        if let Some(source) = self.sources.get(&self.current) {
            &source[source_span]
        } else {
            eprintln!("NO SOURCE FOUND!");
            "NO SOURCE FOUND!"
        }
    }

    pub fn spanslate(&self, span: &Range<usize>) -> (String, Range<usize>, Position) {
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
}

impl Default for StateTable {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    pub names: BTreeSet<String>,
    pub scopes: Scopes,
    pub scripts: BTreeMap<String, RcNode<Script>>,
    pub tables: BTreeMap<String, RcNode<Table>>,
    pub tags: BTreeMap<String, Vec<String>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            names: BTreeSet::new(),
            scopes: Scopes::new(),
            scripts: BTreeMap::new(),
            tables: BTreeMap::new(),
            tags: BTreeMap::new(),
        }
    }

    /// Inserts a value into the appropriate symbol table, returns false if
    /// overwriting a previously stored value.
    pub fn insert(&mut self, name: String, value: SymbolValue) -> bool {
        match value {
            SymbolValue::Placeholder => self.names.insert(name),
            SymbolValue::Numeric(_) => self.scopes.insert(name, value),
            SymbolValue::String(_) => self.scopes.insert(name, value),
            SymbolValue::Script(node) => self.scripts.insert(name, node).is_none(),
            SymbolValue::Table(node) => self.tables.insert(name, node).is_none(),
            SymbolValue::List(_) => todo!(),
        }
    }

    pub fn register(&mut self, name: String) -> bool {
        self.insert(name, SymbolValue::Placeholder)
    }

    pub fn is_def(&self, name: &str) -> bool {
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
            } else if let Some(v) = self.scripts.get(name) {
                Ok(SymbolValue::Script(v.clone()))
            } else if let Some(v) = self.tables.get(name) {
                Ok(SymbolValue::Table(v.clone()))
            } else {
                Ok(SymbolValue::String(name.to_string()))
            }
        } else {
            let result = match name {
                "tables" | "table" => self.list_tables(),
                "scripts" | "script" => self.list_scripts(),
                "values" | "variables" | "names" | "identifiers" => self.list_names(),
                other => Err(format!(
                    "Identifier '{other}' is not defined in the current symbol table."
                )),
            };
            result.map_err(|err| vec![TaleError::evaluator(0..0, (0, 0), err)])
        }
    }

    pub fn get_table(&self, name: &str) -> Option<&RcNode<Table>> {
        self.tables.get(name)
    }

    pub fn get_script(&self, name: &str) -> Option<&RcNode<Script>> {
        self.scripts.get(name)
    }

    pub fn list_names(&self) -> Result<SymbolValue, String> {
        let mut names_list = vec![SymbolValue::String("Defined Identifiers:".into())];
        names_list.extend(self.names.iter().map(|n| SymbolValue::String(n.clone())));
        Ok(SymbolValue::List(names_list))
    }

    pub fn list_scripts(&self) -> Result<SymbolValue, String> {
        let mut scripts_list = vec![SymbolValue::String("Defined Scripts:".into())];
        scripts_list.extend(
            self.scripts
                .values()
                .map(|n| SymbolValue::String(n.to_string())),
        );
        Ok(SymbolValue::List(scripts_list))
    }

    pub fn list_tables(&self) -> Result<SymbolValue, String> {
        let mut tables_list = vec![SymbolValue::String("Defined Tables:".into())];
        tables_list.extend(
            self.tables
                .values()
                .map(|n| SymbolValue::String(n.to_string())),
        );
        Ok(SymbolValue::List(tables_list))
    }

    pub fn push_scope(&mut self) -> Result<(), ()> {
        if self.scopes.len() < 128 {
            self.scopes.push();
            Ok(())
        } else {
            Err(())
        }
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

impl FromIterator<SymbolValue> for SymbolValue {
    fn from_iter<T: IntoIterator<Item = SymbolValue>>(iter: T) -> Self {

        SymbolValue::List(iter.into_iter().collect())
    }
}

#[derive(Debug)]
pub struct Scopes {
    pub numerics: Vec<HashMap<String, isize>>,
    pub strings: Vec<HashMap<String, String>>,
}

impl Scopes {
    pub fn new() -> Self {
        Self {
            numerics: vec![HashMap::new()],
            strings: vec![HashMap::new()],
        }
    }

    pub fn insert(&mut self, name: String, value: SymbolValue) -> bool {
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

    pub fn resolve(&self, name: &str) -> Option<SymbolValue> {
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

    pub fn push(&mut self) {
        self.numerics.push(HashMap::new());
        self.strings.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        if self.len() > 1 {
            self.numerics.pop();
            self.strings.pop();
        } else {
            unreachable!("Attempted to pop base scope!");
        }
    }

    pub fn len(&self) -> usize {
        // If the internal Vecs are somehow a different length, we should probably break
        debug_assert_eq!(self.numerics.len(), self.strings.len());
        self.numerics.len()
    }
}
