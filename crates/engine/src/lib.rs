mod ast;
mod lexer;
mod parsers;
mod utils;

use std::{cell::RefCell, fs::read_to_string, io, path::Path, rc::Rc};

use error::{TaleError, TaleResultVec, render_tale_result_vec};
use state::{StateTable, SymbolTable, SymbolValue};

mod error;
mod samples;
mod state;

pub mod prelude {
    pub use crate::Interpreter;
    pub use crate::error::{TaleError, TaleResultVec};
    pub use crate::state::SymbolValue;
}

pub struct Interpreter {
    state: StateTable,
    symbols: RefCell<SymbolTable>,
    repl_count: usize,
}

impl Interpreter {
    #[must_use]
    pub fn new(prefix: &'static str) -> Self {
        Self {
            state: StateTable::new(prefix),
            symbols: RefCell::default(),
            repl_count: 0,
        }
    }

    #[must_use]
    pub fn new_with_source_string(prefix: &'static str, source: String) -> Self {
        let state = StateTable::new(prefix);
        let symbols = RefCell::default();
        state.captured_pipeline(&symbols, "InitialInput".into(), source);
        Self {
            state,
            symbols,
            repl_count: 0,
        }
    }

    pub fn new_with_files<P>(prefix: &'static str, file_names: &[P]) -> io::Result<Self>
    where
        P: AsRef<Path> + ToString,
    {
        let state = StateTable::new(prefix);
        let symbols = RefCell::default();
        for file_name in file_names {
            let source = read_to_string(file_name)?;
            state.captured_pipeline(&symbols, file_name.to_string(), source);
        }
        Ok(Self {
            state,
            symbols,
            repl_count: 0,
        })
    }

    pub fn new_with_file<P>(prefix: &'static str, file_name: P) -> io::Result<Self>
    where
        P: AsRef<Path> + ToString,
    {
        Self::new_with_files(prefix, &[file_name])
    }

    pub fn current_output(&self) -> TaleResultVec<SymbolValue> {
        self.state.current_output()
    }

    pub fn output_of(&self, name: &str) -> TaleResultVec<SymbolValue> {
        self.state.output_of(name)
    }

    pub fn source_of(&self, name: &str) -> Option<Rc<String>> {
        self.state.source_of(name)
    }

    pub fn render_output_of(&self, prefix: &str, name: &str) -> TaleResultVec<SymbolValue> {
        let source = self
            .source_of(name)
            .ok_or_else(|| TaleError::system(format!("No source found for: {name}")))?;
        let tale_result_vec = self.output_of(name);
        render_tale_result_vec(prefix, name, &source, tale_result_vec)
    }

    pub fn execute(&mut self, prefix: &str, source: &str) -> TaleResultVec<SymbolValue> {
        self.repl_count += 1;
        let source_name = format!("REPL({})", self.repl_count);
        let tale_result_vec =
            self.state
                .pipeline(&self.symbols, source_name.clone(), source.to_string());
        render_tale_result_vec(prefix, &source_name, source, tale_result_vec)
    }

    pub fn execute_captured(&mut self, source: &str) {
        self.repl_count += 1;
        let source_name = format!("REPL({})", self.repl_count);
        self.state
            .captured_pipeline(&self.symbols, source_name.clone(), source.to_string());
    }
}

#[cfg(test)]
#[allow(unused_must_use)]
mod tests {
    use chumsky::{extra::SimpleState, prelude::*};

    use crate::{
        error::TaleError, lexer::Token, samples::*, state::SimpleParserState,
        utils::tests::sample_path,
    };

    use super::*;

    pub fn stubbed_parser<'src, T>(
        state: &'src mut state::ParserState,
        tokens: &'src [Token],
        parser: impl Parser<
            'src,
            &'src [Token],
            T,
            extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
        > + Clone,
    ) -> String
    where
        T: std::fmt::Display,
    {
        let mut state = SimpleState::from(state);
        match parser.parse_with_state(tokens, &mut state).into_result() {
            Ok(output) => format!("{output}"),
            Err(the_errs) => {
                let mapped_errs = TaleError::from_parser_vec(the_errs);
                format!(
                    "{:?}",
                    TaleError::update_parser_vec_with_state(mapped_errs, &state,)
                )
            }
        }
    }

    pub fn grubbed_parser<'src, T>(
        state: &'src mut state::ParserState,
        tokens: &'src [Token],
        parser: impl Parser<
            'src,
            &'src [Token],
            T,
            extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
        > + Clone,
    ) -> String
    where
        T: std::fmt::Debug,
    {
        let mut state = SimpleState::from(state);
        match parser.parse_with_state(tokens, &mut state).into_result() {
            Ok(output) => format!("{output:?}"),
            Err(the_errs) => {
                let mapped_errs = TaleError::from_parser_vec(the_errs);
                format!(
                    "{:?}",
                    TaleError::update_parser_vec_with_state(mapped_errs, &state,)
                )
            }
        }
    }

    fn streamline(source: &str) -> String {
        let terp = Interpreter::new_with_source_string("", source.to_string());
        format!("{:?}", terp.current_output())
    }

    fn streamlinest(file_names: &[&str]) -> Interpreter {
        let transform = file_names.iter().map(sample_path);
        let file_names = transform
            .map(|f| f.to_string_lossy().into_owned())
            .collect::<Vec<_>>();
        Interpreter::new_with_files("", &file_names).unwrap()
    }

    #[test]
    fn pipeline_full_01() {
        let output = streamline(TABLE_MINIMAL);
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([Table(Node"));
        assert!(output.ends_with("})]))"));
        assert_eq!(1, output.matches("minimalism").count());
        assert_eq!(1, output.matches("Flat([Node").count());
        assert_eq!(2, output.matches("less").count());
        assert_eq!(2, output.matches("more").count());
    }

    #[test]
    fn pipeline_full_02() {
        let output = streamline(TABLE_ROLL_DEF);
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([Table(Node"));
        assert!(output.ends_with("})]))"));
        assert_eq!(1, output.matches("value: Empty").count());
        assert_eq!(1, output.matches("melee").count());
        assert_eq!(1, output.matches("Dice").count());
    }

    #[test]
    fn pipeline_full_03() {
        let output = streamline(TABLE_LIST);
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([Table(Node"));
        assert!(output.ends_with("})]))"));
        assert_eq!(1, output.matches("groceries").count());
        assert_eq!(1, output.matches("value: List([Str(").count());
        assert_eq!(1, output.matches("eggs").count());
        assert_eq!(1, output.matches("milk").count());
        assert_eq!(1, output.matches("bread").count());
    }

    #[test]
    fn pipeline_full_04() {
        let output = streamline(TABLE_KEYED_NUMERIC);
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([Table(Node"));
        assert!(output.ends_with("})]))"));
        assert_eq!(1, output.matches("numkeyed").count());
        // Includes the words_only meta
        assert_eq!(2, output.matches("Can be as").count());
        assert_eq!(2, output.matches("CSV").count());
        assert_eq!(2, output.matches("loneliest").count());
    }

    #[test]
    fn pipeline_full_05() {
        let output = streamline(TABLE_KEYED_WORD);
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([Table(Node"));
        assert!(output.ends_with("})]))"));
        assert_eq!(1, output.matches("textkeys").count());
        assert_eq!(2, output.matches("upon").count());
        assert_eq!(1, output.matches("time").count());
    }

    #[test]
    fn pipeline_full_06() {
        let output = streamline(TABLE_GROUP);
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([List([Table(Node"));
        assert!(output.ends_with("})])]))"));
        assert!(output.contains("treasure hoard"));
        assert!(output.contains("magic"));
    }

    #[test]
    fn pipeline_full_07() {
        let output = streamline(TABLE_BLOCKS);
        eprintln!("{output}");
        assert!(output.starts_with("Err([TaleError { kind: Analysis"));
        assert!(output.ends_with("}])"));
        assert!(output.contains("size"));
        assert!(output.contains("crime"));
        assert_eq!(2, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_07_with_deps() {
        let mut terp = streamlinest(&["92_supporting_defs.tale", "07_table_blocks.tale"]);
        terp.execute_captured("show tables");
        let output = format!("{:?}", terp.current_output());
        eprintln!("{output}");
        assert!(output.contains("Defined Tables:"));
        assert!(output.contains("`trading post visitor traffic`, 1d20, 5 Rows"));
        assert!(output.contains("`wanted outcomes`, 1d14, 14 Rows"));
    }

    #[test]
    fn pipeline_full_10_expr() {
        let output = streamline(STATEMENT_EXPRESSION);
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([Numeric("));
        assert!(output.ends_with(")]))"));
        assert_eq!(2, output.matches("Numeric(").count());
    }

    #[test]
    fn pipeline_full_11_assign() {
        let output = streamline(STATEMENT_ASSIGNMENT);
        eprintln!("{output}");
        assert!(output.starts_with("Err([TaleError { kind: Eval"));
        assert!(output.ends_with("}])"));
        assert_eq!(1, output.matches("non-numeric").count());
    }

    #[test]
    fn pipeline_full_11_assign_with_deps() {
        let terp = streamlinest(&["92_supporting_defs.tale", "11_statement_assignment.tale"]);
        let output = format!("{:?}", terp.current_output());
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(1, output.matches("Overwriting previous value").count());
    }

    #[test]
    fn pipeline_full_12_clear() {
        let output = streamline(STATEMENT_CLEAR);
        eprintln!("{output}");
        assert!(output.starts_with("Err([TaleError { kind: Analysis"));
        assert!(output.ends_with("}])"));
        assert_eq!(4, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_12_clear_with_deps() {
        let terp = streamlinest(&["92_supporting_defs.tale", "12_statement_clear.tale"]);
        let output = format!("{:?}", terp.current_output());
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(4, output.matches("Placeholder").count());
    }

    #[test]
    fn pipeline_full_13_invoke() {
        let output = streamline(STATEMENT_INVOKE);
        eprintln!("{output}");
        assert!(output.starts_with("Err([TaleError { kind: Analysis"));
        assert!(output.ends_with("}])"));
        assert_eq!(2, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_13_invoke_with_deps() {
        let terp = streamlinest(&["92_supporting_defs.tale", "13_statement_invoke.tale"]);
        let output = format!("{:?}", terp.current_output());
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([List([Placeholder"));
        assert!(output.ends_with(")]))"));
        assert_eq!(1, output.matches("Placeholder").count());
        assert_eq!(3, output.matches("List").count());
    }

    #[test]
    fn pipeline_full_14_load() {
        let output = streamline(STATEMENT_LOAD);
        eprintln!("{output}");
        assert!(output.starts_with("Err([TaleError { kind: System"));
        assert!(output.ends_with("}])"));
        assert_eq!(3, output.matches("No such file or directory").count());
    }

    #[test]
    fn pipeline_full_15_lookup() {
        let output = streamline(STATEMENT_LOOKUP);
        eprintln!("{output}");
        assert!(output.starts_with("Err([TaleError { kind: Evaluation"));
        assert!(output.ends_with("}])"));
        assert_eq!(4, output.matches("Not a Table or Group name").count());
    }

    #[test]
    fn pipeline_full_15_lookup_with_deps() {
        let terp = streamlinest(&["92_supporting_defs.tale", "15_statement_lookup.tale"]);
        let output = format!("{:?}", terp.current_output());
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([Numeric(1), String("));
        assert!(output.ends_with(")]))"));
        assert_eq!(3, output.matches("String").count());
        assert!(0 < output.matches("3 => c").count());
    }

    #[test]
    fn pipeline_full_16_modify() {
        let output = streamline(STATEMENT_MODIFY);
        eprintln!("{output}");
        assert!(output.starts_with("Err([TaleError { kind: Analysis"));
        assert!(output.ends_with("}])"));
        assert_eq!(4, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_16_modify_with_deps() {
        let terp = streamlinest(&["92_supporting_defs.tale", "16_statement_modify.tale"]);
        let output = format!("{:?}", terp.current_output());
        eprintln!("{output}");
        assert_eq!(
            "Ok(List([Placeholder, Placeholder, Placeholder, Placeholder]))",
            output
        );
    }

    #[test]
    fn pipeline_full_17_output() {
        let output = streamline(STATEMENT_OUTPUT);
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([String(\"There are "));
        assert!(output.ends_with(
            "lights illuminated out of a total of 5.\"), String(\"A lovely string\")]))"
        ));
    }

    #[test]
    fn pipeline_full_18_roll() {
        let output = streamline(STATEMENT_ROLL);
        eprintln!("{output}");
        assert!(output.starts_with("Err([TaleError { kind: Analysis"));
        assert!(output.ends_with("msg: \"Roll: neither 'farm' nor 'animals' are defined\" }])"));
    }

    #[test]
    fn pipeline_full_18_roll_with_deps() {
        let terp = streamlinest(&["92_supporting_defs.tale", "18_statement_roll.tale"]);
        let output = format!("{:?}", terp.current_output());
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List(["));
        assert!(output.ends_with(")]))"));
        eprintln!("{}", output.matches("Numeric(").count());
        assert!(11 <= output.matches("Numeric(").count());
        assert!(17 >= output.matches("Numeric(").count());
        eprintln!("{}", output.matches("Literally just a wand").count());
        assert!(4 <= output.matches("Literally just a wand").count());
        assert!(20 >= output.matches("Literally just a wand").count());
        eprintln!("{}", output.matches("List(").count());
        assert!(10 >= output.matches("List(").count());
        assert!(4 <= output.matches("List(").count());
    }

    #[test]
    fn pipeline_full_19_show() {
        let output = streamline(STATEMENT_SHOW);
        eprintln!("{output}");
        assert!(output.starts_with("Err([TaleError { kind: Analysis"));
        assert!(output.ends_with("}])"));
        assert_eq!(2, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_19_show_with_deps() {
        let terp = streamlinest(&["92_supporting_defs.tale", "19_statement_show.tale"]);
        let output = format!("{:?}", terp.current_output());
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List(["));
        assert!(output.ends_with(")]))"));
        assert_eq!(3, output.matches("minimalism").count());
        assert_eq!(1, output.matches("midnight").count());
        assert_eq!(1, output.matches("Numeric(12)").count());
        assert_eq!(2, output.matches("quality").count());
        assert_eq!(2, output.matches("some kind of bizarre ritual").count());
        assert_eq!(2, output.matches("last rites").count());
        assert_eq!(3, output.matches("textkeys").count());
        assert_eq!(2, output.matches("numkeyed").count());
        assert_eq!(2, output.matches("magic item table a").count());
        assert_eq!(2, output.matches("farm animals").count());
        assert_eq!(
            1,
            output
                .matches("`some kind of bizarre ritual`, 1 Statement")
                .count()
        );
        assert_eq!(1, output.matches("`last rites`, 0 Statements").count());
        assert_eq!(1, output.matches("`farm animals`, 1d4, 4 Rows").count());
        assert_eq!(
            1,
            output.matches("`magic item table a`, 1d1, 1 Rows").count()
        );
        assert_eq!(1, output.matches("`minimalism`, 1d20, 0 Rows").count());
        assert_eq!(1, output.matches("`quality`, 1d3, 3 Rows").count());
        assert_eq!(1, output.matches("`numkeyed`, 1d3, 3 Rows").count());
        assert_eq!(2, output.matches("`textkeys`, 1d1, 1 Rows").count());
    }

    #[test]
    fn pipeline_full_21_scripts() {
        let output = streamline(SCRIPT);
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List(["));
        assert!(output.ends_with("]))"));
        assert_eq!(3, output.matches("Script(Node").count());
    }

    #[test]
    fn pipeline_full_92_supporting_defs() {
        let output = streamline(SUPPORTING_DEFS);
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(3, output.matches("Placeholder").count());
        assert_eq!(9, output.matches("Table(Node").count());
        assert_eq!(2, output.matches("Script(Node").count());
    }

    #[test]
    fn pipeline_full_93_scopes() {
        let output = streamline(SCOPING);
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List(["));
        assert!(output.ends_with("]))"));
        assert_eq!(3, output.matches("Placeholder").count());
        assert_eq!(2, output.matches("Overwriting previous value").count());
    }

    #[test]
    #[allow(clippy::cast_precision_loss)] // Since we know how large our terms will be here.
    fn pipeline_full_94_distributions() {
        let output = streamline(DISTRIBUTIONS);
        //eprintln!("{}", output);
        assert!(output.starts_with("Ok(List(["));
        assert!(output.ends_with("]))"));

        let flat_terms = &[
            "one tenth",
            "two tenths",
            "three tenths",
            "four tenths",
            "five tenths",
            "six tenths",
            "seven tenths",
            "eight tenths",
            "nine tenths",
            "one whole",
        ];
        for term in flat_terms {
            let current_count = output.matches(term).count();
            eprintln!("{term}: {current_count}");
            assert!(1100 > current_count);
            assert!(900 < current_count);
        }

        let curve_terms = &[
            ("lower zero point four six", 46.0),
            ("lower one point three nine", 139.0),
            ("lower two point seven eight", 278.0),
            ("lower four point six three", 463.0),
            ("lower six point nine four", 694.0),
            ("lower nine point seven two", 972.0),
            ("lower eleven point five seven", 1157.0),
            ("lower twelve point five", 1250.0),
            ("upper twelve point five", 1250.0),
            ("upper eleven point five seven", 1157.0),
            ("upper nine point seven two", 972.0),
            ("upper six point nine four", 694.0),
            ("upper four point six three", 463.0),
            ("upper two point seven eight", 278.0),
            ("upper one point three nine", 139.0),
            ("upper zero point four six", 46.0),
        ];
        //eprintln!("{}", output);
        let tolerance = 0.15;
        for (term, target) in curve_terms {
            let current_count = output.matches(term).count();
            let target = target * 10.0;
            eprintln!(
                "{}: {} < {}",
                term,
                current_count,
                target * (1.0 + tolerance)
            );
            assert!(target * (1.0 + tolerance) > current_count as f64);
            eprintln!(
                "{}: {} > {}",
                term,
                current_count,
                target * (1.0 - tolerance)
            );

            assert!(target * (1.0 - tolerance) < current_count as f64);
        }
    }

    #[test]
    fn pipeline_full_95_recursion() {
        let output = streamline(RECURSION);
        eprintln!("{output}");
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(2, output.matches("Placeholder").count());
        assert_eq!(126, output.matches("=> [").count());
        assert_eq!(1, output.matches("Numeric").count());
    }
}
