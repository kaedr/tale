mod ast;
mod lexer;
mod parsers;
mod utils;

use std::{cell::RefCell, fs::read_to_string, io, path::Path};

use ariadne::{Color, Label, Report, Source};

use error::{TaleError, TaleResultVec};
use state::{StateTable, SymbolTable, SymbolValue};

mod error;
mod samples;
mod state;

pub mod prelude {
    pub use crate::Interpreter;
    pub use crate::error::TaleResultVec;
    pub use crate::state::SymbolValue;
}

pub struct Interpreter {
    state: StateTable,
    symbols: RefCell<SymbolTable>,
    repl_count: usize,
}

impl Interpreter {
    pub fn new(prefix: &'static str) -> Self {
        Self {
            state: StateTable::new(prefix),
            symbols: Default::default(),
            repl_count: 0,
        }
    }

    pub fn new_with_source_string(prefix: &'static str, source: String) -> Self {
        let state = StateTable::new(prefix);
        let symbols = Default::default();
        state.captured_pipeline(&symbols, "InitialInput".into(), source);
        Self {
            state,
            symbols,
            repl_count: 0,
        }
    }

    pub fn new_with_files<P>(prefix: &'static str, file_names: &Vec<P>) -> io::Result<Self>
    where
        P: AsRef<Path> + ToString,
    {
        let state = StateTable::new(prefix);
        let symbols = Default::default();
        for file_name in file_names {
            let source = read_to_string(&file_name)?;
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
        Self::new_with_files(prefix, &vec![file_name])
    }

    pub fn current_output(&self) -> TaleResultVec<SymbolValue> {
        self.state.current_output()
    }

    pub fn output_of(&self, name: &str) -> TaleResultVec<SymbolValue> {
        self.state.output_of(name)
    }

    pub fn source_of(&self, name: &str) -> TaleResultVec<SymbolValue> {
        self.state.source_of(name)
    }

    pub fn render_output_of(&self, prefix: &str, name: &str) -> TaleResultVec<SymbolValue> {
        let source = self.source_of(name)?.to_string();
        let trv = self.output_of(name);
        render_tale_result_vec(prefix, name, source, trv)
    }

    pub fn execute(&mut self, prefix: &str, source: String) -> TaleResultVec<SymbolValue> {
        self.repl_count += 1;
        let source_name = format!("REPL({})", self.repl_count);
        let trv = self
            .state
            .pipeline(&self.symbols, source_name.clone(), source.clone());
        render_tale_result_vec(prefix, &source_name, source, trv)
    }
}

pub fn render_tale_result_vec(
    prefix: &str,
    source_name: &str,
    source: String,
    trv: TaleResultVec<SymbolValue>,
) -> TaleResultVec<SymbolValue> {
    match trv {
        Ok(value) => {
            value.render(prefix);
            Ok(SymbolValue::Placeholder)
        }
        Err(tev) => render_tale_error_vec(tev, source_name, source),
    }
}

pub fn render_tale_error_vec(
    tev: Vec<TaleError>,
    source_name: &str,
    source: String,
) -> TaleResultVec<SymbolValue> {
    for error in tev {
        Report::build(ariadne::ReportKind::Error, (source_name, error.span()))
            .with_message(format!("{:?} Error: {}", error.kind(), error.msg()))
            .with_label(
                Label::new((source_name, error.span()))
                    .with_message("Problem occurred here.")
                    .with_color(Color::Red),
            )
            .finish()
            .eprint((source_name, Source::from(&source)))
            .map_err(|err| TaleError::from(err))?;
    }
    Ok(SymbolValue::Placeholder)
}

#[cfg(test)]
#[allow(unused_must_use)]
mod tests {
    use chumsky::{extra::SimpleState, prelude::*};

    use crate::{lexer::Token, samples::*, state::SimpleParserState, utils::tests::sample_path};

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
        match parser.parse_with_state(&tokens, &mut state).into_result() {
            Ok(output) => format!("{output}"),
            Err(the_errs) => {
                let mut err_breakout: Vec<TaleError> = the_errs
                    .iter()
                    .map(|err| TaleError::parser(err.span().into_range(), (0, 0), err.to_string()))
                    .collect();
                err_breakout.iter_mut().for_each(|err| {
                    err.update_span(state.get_source_span(&err.span()));
                    err.update_position(state.get_source_position(&err.span()));
                });
                format!("{:?}", err_breakout)
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
        match parser.parse_with_state(&tokens, &mut state).into_result() {
            Ok(output) => format!("{:?}", output),
            Err(the_errs) => {
                let mut err_breakout: Vec<TaleError> = the_errs
                    .iter()
                    .map(|err| TaleError::parser(err.span().into_range(), (0, 0), err.to_string()))
                    .collect();
                err_breakout.iter_mut().for_each(|err| {
                    err.update_span(state.get_source_span(&err.span()));
                    err.update_position(state.get_source_position(&err.span()));
                });
                format!("{:?}", err_breakout)
            }
        }
    }

    fn streamline(source: &str) -> String {
        let terp = Interpreter::new_with_source_string("", source.to_string());
        format!("{:?}", terp.current_output())
    }

    fn streamlinest(file_names: Vec<&str>) -> String {
        let transform = file_names.iter().map(|f| sample_path(f));
        let file_names = transform
            .map(|f| f.to_string_lossy().into_owned())
            .collect::<Vec<_>>();
        let terp = Interpreter::new_with_files("", &file_names).unwrap();
        format!("{:?}", terp.current_output())
    }

    #[test]
    fn pipeline_full_01() {
        let output = streamline(TABLE_MINIMAL);
        eprintln!("{}", output);
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
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List([Table(Node"));
        assert!(output.ends_with("})]))"));
        assert_eq!(1, output.matches("value: Empty").count());
        assert_eq!(1, output.matches("melee").count());
        assert_eq!(1, output.matches("Dice").count());
    }

    #[test]
    fn pipeline_full_03() {
        let output = streamline(TABLE_LIST);
        eprintln!("{}", output);
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
        eprintln!("{}", output);
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
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List([Table(Node"));
        assert!(output.ends_with("})]))"));
        assert_eq!(1, output.matches("textkeys").count());
        assert_eq!(2, output.matches("upon").count());
        assert_eq!(1, output.matches("time").count());
    }

    #[test]
    fn pipeline_full_06() {
        let output = streamline(TABLE_GROUP);
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List([List([Table(Node"));
        assert!(output.ends_with("})])]))"));
        assert!(output.contains("treasure hoard"));
        assert!(output.contains("magic"));
    }

    #[test]
    fn pipeline_full_10_expr() {
        let output = streamline(STATEMENT_EXPRESSION);
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List([Numeric("));
        assert!(output.ends_with(")]))"));
        assert_eq!(2, output.matches("Numeric(").count())
    }

    #[test]
    fn pipeline_full_11_assign() {
        let output = streamline(STATEMENT_ASSIGNMENT);
        eprintln!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Eval"));
        assert!(output.ends_with("}])"));
        assert_eq!(1, output.matches("non-numeric").count());
    }

    #[test]
    fn pipeline_full_11_assign_with_deps() {
        let output = streamlinest(vec![
            "92_supporting_defs.tale",
            "11_statement_assignment.tale",
        ]);
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(1, output.matches("Overwriting previous value").count());
    }

    #[test]
    fn pipeline_full_12_clear() {
        let output = streamline(STATEMENT_CLEAR);
        eprintln!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Analysis"));
        assert!(output.ends_with("}])"));
        assert_eq!(4, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_12_clear_with_deps() {
        let output = streamlinest(vec!["92_supporting_defs.tale", "12_statement_clear.tale"]);
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(4, output.matches("Placeholder").count());
    }

    #[test]
    fn pipeline_full_13_invoke() {
        let output = streamline(STATEMENT_INVOKE);
        eprintln!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Analysis"));
        assert!(output.ends_with("}])"));
        assert_eq!(2, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_13_invoke_with_deps() {
        let output = streamlinest(vec!["92_supporting_defs.tale", "13_statement_invoke.tale"]);
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List([List([Placeholder"));
        assert!(output.ends_with(")]))"));
        assert_eq!(1, output.matches("Placeholder").count());
        assert_eq!(3, output.matches("List").count());
    }

    #[test]
    fn pipeline_full_14_load() {
        let output = streamline(STATEMENT_LOAD);
        eprintln!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: System"));
        assert!(output.ends_with("}])"));
        assert_eq!(3, output.matches("No such file or directory").count());
    }

    #[test]
    fn pipeline_full_15_lookup() {
        let output = streamline(STATEMENT_LOOKUP);
        eprintln!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Evaluation"));
        assert!(output.ends_with("}])"));
        assert_eq!(4, output.matches("Not a Table or Group name").count());
    }

    #[test]
    fn pipeline_full_15_lookup_with_deps() {
        let output = streamlinest(vec!["92_supporting_defs.tale", "15_statement_lookup.tale"]);
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List([Numeric(1), String(\"c\"), "));
        assert!(output.ends_with(")]))"));
        assert_eq!(3, output.matches("String").count());
        assert!(0 < output.matches("(\"c\")").count());
    }

    #[test]
    fn pipeline_full_16_modify() {
        let output = streamline(STATEMENT_MODIFY);
        eprintln!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Analysis"));
        assert!(output.ends_with("}])"));
        assert_eq!(4, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_16_modify_with_deps() {
        let output = streamlinest(vec!["92_supporting_defs.tale", "16_statement_modify.tale"]);
        eprintln!("{}", output);
        assert_eq!(
            "Ok(List([Placeholder, Placeholder, Placeholder, Placeholder]))",
            output
        )
    }

    #[test]
    fn pipeline_full_17_output() {
        let output = streamline(STATEMENT_OUTPUT);
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List([String(\"There are "));
        assert!(output.ends_with(
            "lights illuminated out of a total of 5.\"), String(\"A lovely string\")]))"
        ));
    }

    #[test]
    fn pipeline_full_18_roll() {
        let output = streamline(STATEMENT_ROLL);
        eprintln!("{}", output);
        assert_eq!(
            "Err([TaleError { kind: Analysis, span: 264..268, position: (10, 5), msg: \"Roll: neither 'farm' nor 'animals' are defined\" }])",
            output
        )
    }

    #[test]
    fn pipeline_full_18_roll_with_deps() {
        let output = streamlinest(vec!["92_supporting_defs.tale", "18_statement_roll.tale"]);
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List(["));
        assert!(output.ends_with(")]))"));
        eprintln!("{}", output.matches("Numeric(").count());
        assert!(11 <= output.matches("Numeric(").count());
        assert!(17 >= output.matches("Numeric(").count());
        eprintln!(
            "{}",
            output.matches("String(\"Literally just a wand\"").count()
        );
        assert!(4 <= output.matches("String(\"Literally just a wand\"").count());
        assert!(20 >= output.matches("String(\"Literally just a wand\"").count());
        eprintln!("{}", output.matches("List(").count());
        assert!(10 >= output.matches("List(").count());
        assert!(4 <= output.matches("List(").count());
    }

    #[test]
    fn pipeline_full_19_show() {
        let output = streamline(STATEMENT_SHOW);
        eprintln!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Analysis"));
        assert!(output.ends_with("}])"));
        assert_eq!(2, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_19_show_with_deps() {
        let output = streamlinest(vec!["92_supporting_defs.tale", "19_statement_show.tale"]);
        eprintln!("{}", output);
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
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List(["));
        assert!(output.ends_with("]))"));
        assert_eq!(3, output.matches("Script(Node").count());
    }

    #[test]
    fn pipeline_full_92_supporting_defs() {
        let output = streamline(SUPPORTING_DEFS);
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(3, output.matches("Placeholder").count());
        assert_eq!(6, output.matches("Table(Node").count());
        assert_eq!(2, output.matches("Script(Node").count());
    }

    #[test]
    fn pipeline_full_93_scopes() {
        let output = streamline(SCOPING);
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List(["));
        assert!(output.ends_with("]))"));
        assert_eq!(3, output.matches("Placeholder").count());
        assert_eq!(2, output.matches("Overwriting previous value").count());
    }

    #[test]
    fn pipeline_full_94_distributions() {
        let output = streamline(DISTRIBUTIONS);
        //eprintln!("{}", output);
        assert!(output.starts_with("Ok(List(["));
        assert!(output.ends_with("]))"));

        let flat_terms = vec![
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
            eprintln!("{}: {}", term, current_count);
            assert!(1100 > current_count);
            assert!(900 < current_count);
        }

        let curve_terms = vec![
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
        eprintln!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(128, output.matches("Placeholder").count());
        assert_eq!(127, output.matches("Numeric").count());
    }
}
