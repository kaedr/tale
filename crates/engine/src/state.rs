use std::{
    cell::{Ref, RefCell},
    cmp::min,
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
    ops::Range,
    rc::Rc,
};

use chumsky::{Parser, extra::SimpleState};

use crate::{
    ast::{Analyze as _, Ast, Eval as _, RcNode, Script, SourceInfo, Statement, Table, rc_node},
    error::{TaleError, TaleResult, TaleResultVec, render_tale_error_vec},
    lexer::{Lexicon, Position, Token, tokenize},
    parsers::{Op, parser},
};

pub type SimpleParserState<'src> = SimpleState<&'src mut ParserState>;

pub type StateCellMap<T> = Rc<RefCell<HashMap<String, T>>>;

#[derive(Debug, Clone)]
pub struct ParserState {
    source: Rc<String>,
    lexicon: Lexicon,
}

impl ParserState {
    fn new(source: Rc<String>, lexicon: Lexicon) -> Self {
        Self { source, lexicon }
    }

    pub fn get_source_span(&self, span: &Range<usize>) -> Range<usize> {
        if self.lexicon.is_empty() {
            0..0
        } else {
            let start = min(span.start, self.lexicon.len().saturating_sub(1)); // Avoid out of bounds
            let end = min(
                span.end.saturating_sub(1),
                self.lexicon.len().saturating_sub(1),
            );
            self.lexicon[start].1.start..self.lexicon[end].1.end
        }
    }

    pub fn get_source_position(&self, span: &Range<usize>) -> Position {
        if self.lexicon.is_empty() {
            (0, 0)
        } else {
            let start = min(span.start, self.lexicon.len().saturating_sub(1)); // Avoid out of bounds
            self.lexicon[start].2
        }
    }

    pub fn get_source_slice(&self, span: &Range<usize>) -> String {
        let source_span = self.get_source_span(span);
        self.source[source_span].into()
    }

    pub fn spanslate(&self, span: &Range<usize>) -> SourceInfo {
        if self.lexicon.is_empty() {
            (0..0, (0, 0))
        } else {
            (self.get_source_span(span), self.get_source_position(span))
        }
    }
}

#[derive(Debug)]
pub struct StateTable {
    current: RefCell<String>,
    sources: StateCellMap<Rc<String>>,
    tokens: StateCellMap<Lexicon>,
    asts: StateCellMap<Rc<Ast>>,
    outputs: StateCellMap<TaleResultVec<SymbolValue>>,
}

impl StateTable {
    pub fn new() -> Self {
        Self {
            current: RefCell::default(),
            sources: Rc::default(),
            tokens: Rc::default(),
            asts: Rc::default(),
            outputs: Rc::default(),
        }
    }

    pub fn current(&self) -> Ref<String> {
        self.current.borrow()
    }

    pub fn current_clone(&self) -> String {
        self.current.borrow().clone()
    }

    pub fn current_output(&self) -> TaleResultVec<SymbolValue> {
        self.output_of(&self.current())
    }

    pub fn output_of(&self, name: &str) -> TaleResultVec<SymbolValue> {
        if let Some(output) = self.outputs.borrow().get(name) {
            output.clone()
        } else {
            Err(TaleError::system(format!("No output for source: {}", self.current())).into())
        }
    }

    pub fn source_of(&self, name: &str) -> Option<Rc<String>> {
        self.sources.borrow().get(name).cloned()
    }

    pub fn asts(&self) -> StateCellMap<Rc<Ast>> {
        self.asts.clone()
    }

    pub fn add_source(&self, name: String, source: String) {
        self.current.replace(name.clone());
        self.sources.borrow_mut().insert(name, source.into());
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
        self.tokens
            .borrow_mut()
            .insert(self.current_clone(), lexicon);
        Ok(())
    }

    pub fn parse_current(&self) -> TaleResultVec<()> {
        let current = self.current().clone();
        let the_errs;
        let tokens = self.get_tokens(&current)?;
        let source = self
            .sources
            .borrow()
            .get(&current)
            .ok_or(TaleError::system(format!("No source for: {}", &current)))?
            .clone();
        let lexicon = self
            .tokens
            .borrow()
            .get(&current)
            .ok_or(TaleError::system(format!("No source for: {}", &current)))?
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
        if the_errs.is_empty() {
            self.asts
                .borrow_mut()
                .insert(self.current_clone(), Ast::new(output).into());
            Ok(())
        } else {
            // This has to happen in two parts because the source is borrowed by the
            // Parser errors
            let mapped_errs = TaleError::from_parser_vec(the_errs);
            Err(TaleError::update_parser_vec_with_state(
                mapped_errs,
                &state_inner,
            ))
        }
    }

    pub fn analyze_current(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<()> {
        let maybe_ast = self.asts.borrow_mut().get(&*self.current()).cloned();
        if let Some(ast) = maybe_ast {
            ast.analyze(symbols)
        } else {
            Err(
                TaleError::analyzer(0..0, (0, 0), format!("No AST named: {}", self.current()))
                    .into(),
            )
        }
    }

    pub fn evaluate_current(&self, symbols: &RefCell<SymbolTable>) -> TaleResultVec<SymbolValue> {
        let maybe_ast = self.asts.borrow_mut().get(&*self.current()).cloned();
        if let Some(ast) = maybe_ast {
            ast.eval(symbols, self)
        } else {
            Err(
                TaleError::analyzer(0..0, (0, 0), format!("No AST named: {}", self.current()))
                    .into(),
            )
        }
    }

    pub fn pipeline(
        &self,
        symbols: &RefCell<SymbolTable>,
        name: String,
        source: String,
    ) -> TaleResultVec<SymbolValue> {
        self.add_source(name, source);
        self.lex_current()?;
        self.parse_current()?;
        self.analyze_current(symbols)?;
        self.evaluate_current(symbols)
    }

    pub fn captured_pipeline(&self, symbols: &RefCell<SymbolTable>, name: String, source: String) {
        let output = self.pipeline(symbols, name, source);
        self.outputs
            .borrow_mut()
            .insert(self.current_clone(), output);
    }

    pub fn nested_pipeline(
        &self,
        symbols: &RefCell<SymbolTable>,
        name: &str,
        source: &str,
    ) -> TaleResultVec<SymbolValue> {
        let outer_name = self.current_clone();
        let pushed_ok = symbols.borrow_mut().push_scope();
        if let Ok(()) = pushed_ok {
            let tale_result_vec = self.pipeline(symbols, name.to_string(), source.to_string());
            symbols.borrow_mut().pop_scope();
            self.current.replace(outer_name);
            match tale_result_vec {
                Ok(_) => tale_result_vec,
                Err(tev) => render_tale_error_vec(tev, name, source),
            }
        } else {
            symbols.borrow_mut().pop_scope();
            Err(TaleError::system(format!("Hit stack guard while loading: {name}")).into())
        }
    }

    pub fn get_tokens(&self, name: &str) -> TaleResult<Vec<Token>> {
        if let Some(lexicon) = self.tokens.borrow().get(name) {
            Ok(lexicon.iter().map(|(token, _, _)| token.clone()).collect())
        } else {
            Err(TaleError::system(format!("No Lexicon for source: {name}")))
        }
    }
}

impl Default for StateTable {
    fn default() -> Self {
        Self::new()
    }
}

const BUILTIN_IDS: [&str; 8] = [
    "tables",
    "table",
    "scripts",
    "script",
    "values",
    "variables",
    "names",
    "identifiers",
];

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
            SymbolValue::Numeric(_) | SymbolValue::String(_) => self.scopes.insert(name, value),
            SymbolValue::Script(node) => self.scripts.insert(name, node).is_none(),
            SymbolValue::Table(node) => self.tables.insert(name, node).is_none(),
            SymbolValue::List(_) | SymbolValue::KeyValue(_, _) => todo!(),
        }
    }

    pub fn register(&mut self, name: String) -> bool {
        self.insert(name, SymbolValue::Placeholder)
    }

    pub fn is_def(&self, name: &str) -> bool {
        BUILTIN_IDS.contains(&name) || self.names.contains(name)
    }

    pub fn push_tags(&mut self, tags: Vec<String>, table_name: &str) {
        for tag in tags {
            self.tags
                .entry(tag)
                .and_modify(|v| v.push(table_name.to_string()))
                .or_insert_with(|| vec![table_name.to_string()]);
        }
    }

    pub fn get_tags(&self, tags: &[&str]) -> SymbolValue {
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

    pub fn get_value(&self, name: &str) -> Option<SymbolValue> {
        if self.names.contains(name) {
            // If the name exists, return the corresponding value if it exists
            if let Some(v) = self.scopes.resolve(name) {
                Some(v)
            } else if let Some(v) = self.scripts.get(name) {
                Some(SymbolValue::Script(v.clone()))
            } else if let Some(v) = self.tables.get(name) {
                Some(SymbolValue::Table(v.clone()))
            } else {
                Some(SymbolValue::String(name.to_string()))
            }
        } else {
            match name {
                "tables" | "table" => Some(self.list_tables()),
                "scripts" | "script" => Some(self.list_scripts()),
                "values" | "variables" | "names" | "identifiers" => Some(self.list_names()),
                _ => None,
            }
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

    pub fn number_of_scripts(&self) -> usize {
        self.scripts.len()
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

    pub fn number_of_tables(&self) -> usize {
        self.tables.len()
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

impl Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Names: {}",
            self.names.iter().cloned().collect::<Vec<_>>().join(", ")
        )?;
        write!(f, "{}", self.scopes)?;
        writeln!(
            f,
            "Scripts: {}",
            self.scripts.keys().cloned().collect::<Vec<_>>().join(", ")
        )?;
        writeln!(
            f,
            "Tables: {}",
            self.tables.keys().cloned().collect::<Vec<_>>().join(", ")
        )?;
        write!(
            f,
            "Tags: {}",
            self.tags.keys().cloned().collect::<Vec<_>>().join(", ")
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[non_exhaustive]
pub enum SymbolValue {
    Placeholder,
    Numeric(isize),
    String(String),
    KeyValue(Box<SymbolValue>, Box<SymbolValue>),
    Script(RcNode<Script>),
    Table(RcNode<Table>),
    List(Vec<SymbolValue>),
}

impl SymbolValue {
    pub fn operation(&self, op: Op, other: &Self) -> TaleResult<SymbolValue> {
        match (self, other) {
            (Self::Numeric(lhs), Self::Numeric(rhs)) => match op {
                Op::Add => Ok(Self::Numeric(lhs + rhs)),
                Op::Sub => Ok(Self::Numeric(lhs - rhs)),
                Op::Mul => Ok(Self::Numeric(lhs * rhs)),
                Op::Div => Ok(Self::Numeric(lhs / rhs)),
                Op::Mod => Ok(Self::Numeric(lhs % rhs)),
                Op::Pow => Ok(Self::Numeric(lhs.pow(u32::try_from(*rhs)?))),
            },
            _ => unimplemented!("operations are only valid for numeric values!"),
        }
    }

    pub fn render(&self, prefix: &str) {
        match self {
            Self::Placeholder => (),
            Self::Numeric(n) => println!("{prefix}{n}"),
            Self::String(s) => println!("{prefix}{s}"),
            Self::KeyValue(key, value) => {
                if let Self::List(_) = value.as_ref() {
                    println!("{prefix}{key} =>");
                    value.render(prefix);
                } else {
                    println!("{prefix}{key} => {value}");
                }
            }
            Self::Script(script) => println!("{prefix}{script}"),
            Self::Table(table) => println!("{prefix}{table}"),
            Self::List(symbol_values) => {
                for value in symbol_values {
                    value.render(&format!("{prefix}  "));
                }
            }
        }
    }
}

impl Display for SymbolValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Placeholder => write!(f, ""),
            Self::Numeric(n) => write!(f, "{n}"),
            Self::String(s) => write!(f, "{s}"),
            Self::KeyValue(key, value) => write!(f, "{key} => {value}"),
            Self::Script(s) => write!(f, "{s}"),
            Self::Table(t) => write!(f, "{t}"),
            Self::List(symbol_values) => write!(
                f,
                "[{}]",
                symbol_values
                    .iter()
                    .map(Self::to_string)
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

#[derive(Debug, Default)]
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

impl Display for Scopes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Numerics:")?;
        for (level, scope) in self.numerics.iter().enumerate() {
            writeln!(
                f,
                "\t{level}: {}",
                scope.keys().cloned().collect::<Vec<_>>().join(", ")
            )?;
        }
        writeln!(f, "Strings:")?;
        for (level, scope) in self.strings.iter().enumerate() {
            writeln!(
                f,
                "\t{level}: {}",
                scope.keys().cloned().collect::<Vec<_>>().join(", ")
            )?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::ParserState;
    use crate::lexer::{Token, tokenize};

    impl ParserState {
        pub fn from_source(source: String) -> Self {
            let lexicon = tokenize(&source).unwrap();
            Self {
                source: source.into(),
                lexicon,
            }
        }

        pub fn tokens(&self) -> Vec<Token> {
            self.lexicon
                .iter()
                .map(|(token, _, _)| token.clone())
                .collect()
        }
    }
}
