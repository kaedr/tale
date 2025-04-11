mod ast;
mod lexer;
mod parsers;
mod utils;

use std::{fs::read_to_string, io, path::Path};

pub use ast::AST;
use error::TaleResultVec;
use state::{StateTable, SymbolValue};

pub mod error;

mod state;

pub struct Interpreter {
    state: StateTable,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            state: StateTable::new(),
        }
    }

    pub fn new_with_source_string(source: String) -> Self {
        let state = StateTable::new();
        state.captured_pipeline("InitialInput".into(), source);
        Self { state }
    }

    pub fn new_with_files<P>(file_names: Vec<P>) -> io::Result<Self>
    where
        P: AsRef<Path> + ToString,
    {
        let state = StateTable::new();
        for file_name in file_names {
            let source = read_to_string(&file_name)?;
            state.captured_pipeline(file_name.to_string(), source);
        }
        Ok(Self { state })
    }

    pub fn new_with_file<P>(file_name: P) -> io::Result<Self>
    where
        P: AsRef<Path> + ToString,
    {
        Self::new_with_files(vec![file_name])
    }

    pub fn current_output(&self) -> TaleResultVec<SymbolValue> {
        self.state.current_output()
    }
}

#[cfg(test)]
#[allow(unused_must_use)]
mod tests {
    use chumsky::{extra::SimpleState, prelude::*};

    use crate::{lexer::Token, state::SimpleParserState, utils::tests::sample_path};

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
            Err(err) => format!("{:?}", err),
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
            Err(err) => format!("{:?}", err),
        }
    }

    fn streamline(file_name: &str) -> String {
        let file_name = sample_path(file_name);
        let terp = Interpreter::new_with_file(file_name.to_str().unwrap()).unwrap();
        format!("{:?}", terp.current_output())
    }

    fn streamlinest(file_names: Vec<&str>) -> String {
        let transform = file_names.iter().map(|f| sample_path(f));
        let file_names = transform
            .map(|f| f.to_string_lossy().into_owned())
            .collect::<Vec<_>>();
        let terp = Interpreter::new_with_files(file_names).unwrap();
        format!("{:?}", terp.current_output())
    }

    #[test]
    fn pipeline_full_01() {
        let output = streamline("01_table_minimal.tale");
        assert_eq!("Ok(List([Placeholder]))", output);
    }

    #[test]
    fn pipeline_full_02() {
        let output = streamline("02_table_roll_def.tale");
        assert_eq!("Ok(List([Placeholder]))", output);
    }

    #[test]
    fn pipeline_full_03() {
        let output = streamline("03_table_list.tale");
        assert_eq!("Ok(List([Placeholder]))", output);
    }

    #[test]
    fn pipeline_full_04() {
        let output = streamline("04_table_keyed_numeric.tale");
        assert_eq!("Ok(List([Placeholder]))", output);
    }

    #[test]
    fn pipeline_full_05() {
        let output = streamline("05_table_keyed_word.tale");
        assert_eq!("Ok(List([Placeholder]))", output);
    }

    #[test]
    fn pipeline_full_06() {
        let output = streamline("06_table_group.tale");
        assert_eq!(
            "Ok(List([List([Placeholder, Placeholder, Placeholder])]))",
            output
        );
    }

    #[test]
    fn pipeline_full_10_expr() {
        let output = streamline("10_statement_expression.tale");
        println!("{}", output);
        assert!(output.starts_with("Ok(List([Numeric("));
        assert!(output.ends_with(")]))"));
        assert_eq!(2, output.matches("Numeric(").count())
    }

    #[test]
    fn pipeline_full_11_assign() {
        let output = streamline("11_statement_assignment.tale");
        println!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Eval"));
        assert!(output.ends_with("}])"));
        assert_eq!(3, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_11_assign_with_deps() {
        let output = streamlinest(vec![
            "92_supporting_defs.tale",
            "11_statement_assignment.tale",
        ]);
        println!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(1, output.matches("Overwriting previous value").count());
    }

    #[test]
    fn pipeline_full_12_clear() {
        let output = streamline("12_statement_clear.tale");
        println!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Eval"));
        assert!(output.ends_with("}])"));
        assert_eq!(4, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_12_clear_with_deps() {
        let output = streamlinest(vec!["92_supporting_defs.tale", "12_statement_clear.tale"]);
        println!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(4, output.matches("Placeholder").count());
    }

    #[test]
    fn pipeline_full_13_invoke() {
        let output = streamline("13_statement_invoke.tale");
        println!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Eval"));
        assert!(output.ends_with("}])"));
        assert_eq!(2, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_13_invoke_with_deps() {
        let output = streamlinest(vec!["92_supporting_defs.tale", "13_statement_invoke.tale"]);
        println!("{}", output);
        assert!(output.starts_with("Ok(List([List([Placeholder"));
        assert!(output.ends_with(")]))"));
        assert_eq!(1, output.matches("Placeholder").count());
        assert_eq!(3, output.matches("List").count());
    }

    #[test]
    fn pipeline_full_14_load() {
        let output = streamline("14_statement_load.tale");
        println!("{}", output);
        assert_eq!("", output)
    }

    #[test]
    fn pipeline_full_15_lookup() {
        let output = streamline("15_statement_lookup.tale");
        println!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Eval"));
        assert!(output.ends_with("}])"));
        assert_eq!(4, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_15_lookup_with_deps() {
        let output = streamlinest(vec!["92_supporting_defs.tale", "15_statement_lookup.tale"]);
        println!("{}", output);
        assert!(output.starts_with("Ok(List([Numeric(1), String(\"c\"), "));
        assert!(output.ends_with(")]))"));
        assert_eq!(3, output.matches("String").count());
        assert!(0 < output.matches("(\"c\")").count());
    }

    #[test]
    fn pipeline_full_16_modify() {
        let output = streamline("16_statement_modify.tale");
        println!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Eval"));
        assert!(output.ends_with("}])"));
        assert_eq!(4, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_16_modify_with_deps() {
        let output = streamlinest(vec!["92_supporting_defs.tale", "16_statement_modify.tale"]);
        println!("{}", output);
        assert_eq!(
            "Ok(List([Placeholder, Placeholder, Placeholder, Placeholder]))",
            output
        )
    }

    #[test]
    fn pipeline_full_17_output() {
        let output = streamline("17_statement_output.tale");
        println!("{}", output);
        assert!(output.starts_with("Ok(List([String(\"There are "));
        assert!(output.ends_with(
            "lights illuminated out of a total of 5.\"), String(\"A lovely string\")]))"
        ));
    }

    #[test]
    fn pipeline_full_18_roll() {
        let output = streamline("18_statement_roll.tale");
        println!("{}", output);
        assert_eq!(
            "Err([TaleError { kind: Analysis, span: 262..266, position: (10, 5), msg: \"Roll: neither 'farm' nor 'animals' are defined\" }])",
            output
        )
    }

    #[test]
    fn pipeline_full_18_roll_with_deps() {
        let output = streamlinest(vec!["92_supporting_defs.tale", "18_statement_roll.tale"]);
        println!("{}", output);
        assert!(output.starts_with("Ok(List(["));
        assert!(output.ends_with(")]))"));
        println!("{}", output.matches("Numeric(").count());
        assert!(11 <= output.matches("Numeric(").count());
        assert!(17 >= output.matches("Numeric(").count());
        println!(
            "{}",
            output.matches("String(\"Literally just a wand\"").count()
        );
        assert!(4 <= output.matches("String(\"Literally just a wand\"").count());
        assert!(20 >= output.matches("String(\"Literally just a wand\"").count());
        println!("{}", output.matches("List(").count());
        assert!(10 >= output.matches("List(").count());
        assert!(4 <= output.matches("List(").count());
    }

    #[test]
    fn pipeline_full_19_show() {
        let output = streamline("19_statement_show.tale");
        println!("{}", output);
        assert!(output.starts_with("Err([TaleError { kind: Eval"));
        assert!(output.ends_with("}])"));
        assert_eq!(2, output.matches("is not defined").count());
    }

    #[test]
    fn pipeline_full_19_show_with_deps() {
        let output = streamlinest(vec!["92_supporting_defs.tale", "19_statement_show.tale"]);
        println!("{}", output);
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
        let output = streamline("21_script.tale");
        println!("{}", output);
        assert_eq!("Ok(List([Placeholder, Placeholder, Placeholder]))", output)
    }

    #[test]
    fn pipeline_full_92_supporting_defs() {
        let output = streamline("92_supporting_defs.tale");
        println!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(11, output.matches("Placeholder").count());
    }

    #[test]
    fn pipeline_full_93_scopes() {
        let output = streamline("93_scoping.tale");
        println!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(5, output.matches("Placeholder").count());
        assert_eq!(2, output.matches("Overwriting previous value").count());
    }

    #[test]
    fn pipeline_full_94_distributions() {
        let output = streamline("94_distributions.tale");
        //println!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
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
            println!("{}: {}", term, current_count);
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
        //println!("{}", output);
        let tolerance = 0.1;
        for (term, target) in curve_terms {
            let current_count = output.matches(term).count();
            let target = target * 10.0;
            println!(
                "{}: {} < {}",
                term,
                current_count,
                target * (1.0 + tolerance)
            );
            assert!(target * (1.0 + tolerance) > current_count as f64);
            println!(
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
        let output = streamline("95_recursion.tale");
        println!("{}", output);
        assert!(output.starts_with("Ok(List([Placeholder"));
        assert!(output.ends_with("]))"));
        assert_eq!(130, output.matches("Placeholder").count());
        assert_eq!(127, output.matches("Numeric").count());
    }
}
