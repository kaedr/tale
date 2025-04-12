use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::min,
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
    ops::Range,
    rc::Rc,
};

use chumsky::{Parser, extra::SimpleState};

use crate::{
    ast::{RcNode, Script, SourceInfo, Statement, Table, rc_node},
    lexer::{Lexicon, Position, Token, tokenize},
    parsers::{Op, parser},
};

use crate::error::TaleResultVec;

use crate::error::TaleError;

use crate::error::TaleResult;

use crate::ast::{AST, Analyze, Eval as _};

pub type SimpleParserState<'src> = SimpleState<&'src mut ParserState>;

pub type StateCellMap<T> = Rc<RefCell<HashMap<String, T>>>;

pub struct ParserState {
    source: String,
    lexicon: Lexicon,
}

impl ParserState {
    fn new(source: String, lexicon: Lexicon) -> Self {
        Self { source, lexicon }
    }

    pub fn from_source(source: String) -> Self {
        let lexicon = tokenize(&source).unwrap();
        Self { source, lexicon }
    }

    pub fn tokens(&self) -> Vec<Token> {
        self.lexicon.iter().map(|(token, _, _)| token.clone()).collect()
    }

    pub fn get_source_span(&self, span: &Range<usize>) -> Range<usize> {
        let start = min(span.start, self.lexicon.len().saturating_sub(1)); // Avoid out of bounds
        self.lexicon[start].1.start..self.lexicon[span.end.saturating_sub(1)].1.end
    }

    pub fn get_source_position(&self, span: &Range<usize>) -> Position {
        let start = min(span.start, self.lexicon.len().saturating_sub(1)); // Avoid out of bounds
        self.lexicon[start].2
    }

    pub fn get_source_slice(&self, span: &Range<usize>) -> String {
        let source_span = self.get_source_span(span);
        self.source[source_span].into()
    }

    pub fn spanslate(&self, span: &Range<usize>) -> SourceInfo {
        if self.lexicon.len() > 0 {
            (
                self.lexicon[span.start].1.start..self.lexicon[span.end.saturating_sub(1)].1.end,
                self.lexicon[span.start].2,
            )
        } else {
            (0..0, (0, 0))
        }
    }
}

#[derive(Debug)]
pub struct StateTable {
    current: RefCell<String>,
    sources: StateCellMap<String>,
    tokens: StateCellMap<Lexicon>,
    asts: StateCellMap<AST>,
    outputs: StateCellMap<TaleResultVec<SymbolValue>>,
    symbols: RefCell<SymbolTable>,
}

impl StateTable {
    pub fn new() -> Self {
        Self {
            current: Default::default(),
            sources: Default::default(),
            tokens: Default::default(),
            asts: Default::default(),
            outputs: Default::default(),
            symbols: Default::default(),
        }
    }

    pub fn current(&self) -> Ref<String> {
        self.current.borrow()
    }

    pub fn current_clone(&self) -> String {
        self.current.borrow().clone()
    }

    pub fn current_output(&self) -> TaleResultVec<SymbolValue> {
        self.output_of(&*self.current())
    }

    pub fn output_of(&self, name: &str) -> TaleResultVec<SymbolValue> {
        if let Some(output) = self.outputs.borrow().get(name) {
            output.clone()
        } else {
            Err(
                TaleError::system(format!("No output for source: {}", self.current()))
                    .into(),
            )
        }
    }

    pub fn source_of(&self, name: &str) -> TaleResultVec<SymbolValue> {
        if let Some(source) = self.sources.borrow().get(name) {
            Ok(SymbolValue::String(source.clone()))
        } else {
            Err(
                TaleError::system(format!("No source named: {}", self.current()))
                    .into(),
            )
        }
    }

    pub fn symbols(&self) -> Ref<SymbolTable> {
        self.symbols.borrow()
    }

    pub fn symbols_mut(&self) -> RefMut<SymbolTable> {
        self.symbols.borrow_mut()
    }

    pub fn asts(&self) -> StateCellMap<AST> {
        self.asts.clone()
    }

    pub fn add_source(&self, name: String, source: String) -> TaleResult<()> {
        *self.current.borrow_mut() = name.clone();
        match self.sources.borrow_mut().insert(name, source) {
            Some(overwritten) => Err(TaleError::system(format!(
                "Attempted to overwrite previous source: {}\nWith: {}",
                self.current(),
                overwritten.chars().take(50).collect::<Box<str>>()
            ))),
            None => Ok(()),
        }
    }

    pub fn lex_current(&self) -> TaleResultVec<()> {
        let lexicon = if let Some(source) = self.sources.borrow().get(&*self.current()) {
            tokenize(source)?
        } else {
            return Err(TaleError::lexer(
                0..0,
                (0, 0),
                format!("No source named: {}", self.current()),
            )
            .into());
        };
        match self
            .tokens
            .borrow_mut()
            .insert(self.current_clone(), lexicon)
        {
            Some(_) => Err(TaleError::lexer(
                0..0,
                (0, 0),
                format!(
                    "Attempted to overwrite previous lexicon of: {}",
                    self.current()
                ),
            )
            .into()),
            None => Ok(()),
        }
    }

    pub fn parse_current(&self) -> TaleResultVec<()> {
        let the_errs;
        let tokens = self.get_tokens(&*self.current())?;
        let source = self
            .sources
            .borrow()
            .get(&*self.current())
            .ok_or(TaleError::system(format!(
                "No source for: {}",
                self.current()
            )))?
            .clone();
        let lexicon = self
            .tokens
            .borrow()
            .get(&*self.current())
            .ok_or(TaleError::system(format!(
                "No source for: {}",
                self.current()
            )))?
            .clone();
        let mut state_inner = ParserState::new(source, lexicon);
        let output = {
            let mut parse_state = SimpleState::from(&mut state_inner);
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
                err.update_span(state_inner.get_source_span(&err.span()));
                err.update_position(state_inner.get_source_position(&err.span()));
            });
            Err(err_breakout)
        } else {
            match self
                .asts
                .borrow_mut()
                .insert(self.current_clone(), AST::new(output))
            {
                Some(_) => Err(TaleError::parser(
                    0..0,
                    (0, 0),
                    format!("Attempted to overwrite previous AST of: {}", self.current()),
                )
                .into()),
                None => Ok(()),
            }
        }
    }

    pub fn analyze_current(&self) -> TaleResultVec<()> {
        if let Some(ast) = self.asts.borrow_mut().get_mut(&*self.current()) {
            ast.analyze(&self.symbols)
        } else {
            Err(
                TaleError::analyzer(0..0, (0, 0), format!("No AST named: {}", self.current()))
                    .into(),
            )
        }
    }

    pub fn evaluate_current(&self) -> TaleResultVec<SymbolValue> {
        if let Some(ast) = self.asts.borrow_mut().get_mut(&*self.current()) {
            ast.eval(&self.symbols)
        } else {
            Err(
                TaleError::analyzer(0..0, (0, 0), format!("No AST named: {}", self.current()))
                    .into(),
            )
        }
    }

    pub fn pipeline(&self, name: String, source: String) -> TaleResultVec<SymbolValue> {
        self.add_source(name, source)?;
        self.lex_current()?;
        self.parse_current()?;
        self.analyze_current()?;
        self.evaluate_current()
    }

    pub fn captured_pipeline(&self, name: String, source: String) {
        let output = self.pipeline(name, source);
        self.outputs
            .borrow_mut()
            .insert(self.current_clone(), output);
    }

    pub fn nested_pipeline(&self, name: String, source: String) -> TaleResultVec<SymbolValue> {
        let outer_name = self.current.clone();
        match self.symbols.borrow_mut().push_scope() {
            Ok(_) => todo!(),
            Err(_) => Err(vec![TaleError::system(format!(
                "Hit stack guard while loading: {}",
                name
            ))]),
        }
    }

    pub fn get_tokens(&self, name: &str) -> TaleResult<Vec<Token>> {
        if let Some(lexicon) = self.tokens.borrow().get(name) {
            Ok(lexicon.iter().map(|(token, _, _)| token.clone()).collect())
        } else {
            Err(TaleError::system(format!("No Lexicon for source: {}", name)).into())
        }
    }
}

impl Default for StateTable {
    fn default() -> Self {
        Self::new()
    }
}

const BUILTIN_IDS: [&str; 8] = ["tables", "table", "scripts", "script", "values", "variables", "names", "identifiers"];

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
        BUILTIN_IDS.contains(&name) || self.names.contains(name)
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
            Ok(
            match name {
                "tables" | "table" => self.list_tables(),
                "scripts" | "script" => self.list_scripts(),
                "values" | "variables" | "names" | "identifiers" => self.list_names(),
                other => SymbolValue::String(other.to_string()),
            }
        )
        }
    }

    pub fn get_table(&self, name: &str) -> Option<&RcNode<Table>> {
        self.tables.get(name)
    }

    pub fn get_script(&self, name: &str) -> Option<&RcNode<Script>> {
        self.scripts.get(name)
    }

    pub fn list_names(&self) -> SymbolValue {
        let mut names_list = vec![SymbolValue::String("Defined Identifiers:".into())];
        names_list.extend(self.names.iter().map(|n| SymbolValue::String(n.clone())));
        SymbolValue::List(names_list)
    }

    pub fn list_scripts(&self) -> SymbolValue {
        let mut scripts_list = vec![SymbolValue::String("Defined Scripts:".into())];
        scripts_list.extend(
            self.scripts
                .values()
                .map(|n| SymbolValue::String(n.to_string())),
        );
        SymbolValue::List(scripts_list)
    }

    pub fn list_tables(&self) -> SymbolValue {
        let mut tables_list = vec![SymbolValue::String("Defined Tables:".into())];
        tables_list.extend(
            self.tables
                .values()
                .map(|n| SymbolValue::String(n.to_string())),
        );
        SymbolValue::List(tables_list)
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

impl SymbolValue {
    pub fn operation(&self, op: Op, other: &Self) -> TaleResultVec<SymbolValue> {
        match (self, other) {
            (SymbolValue::Numeric(lhs), SymbolValue::Numeric(rhs)) => match op {
                Op::Add => Ok(SymbolValue::Numeric(lhs + rhs)),
                Op::Sub => Ok(SymbolValue::Numeric(lhs - rhs)),
                Op::Mul => Ok(SymbolValue::Numeric(lhs * rhs)),
                Op::Div => Ok(SymbolValue::Numeric(lhs / rhs)),
                Op::Mod => Ok(SymbolValue::Numeric(lhs % rhs)),
                Op::Pow => Ok(SymbolValue::Numeric(lhs.pow(*rhs as u32))),
            },
            _ => unimplemented!("operations are only valid for numeric values!"),
        }
    }

    pub fn render(&self, prefix: &str) {
        match self {
            SymbolValue::Placeholder => (),
            SymbolValue::Numeric(n) => println!("{prefix}{n}"),
            SymbolValue::String(s) => println!("{prefix}{s}"),
            SymbolValue::Script(script) => println!("{prefix}{script}"),
            SymbolValue::Table(table) => println!("{prefix}{table}"),
            SymbolValue::List(symbol_values) => {
                //println!("[");
                for value in symbol_values {
                    value.render(&format!("{prefix}"));
                }
                //println!("]");
            },
        }
    }
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
