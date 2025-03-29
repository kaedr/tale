use chumsky::prelude::*;
use lexer::Token;

use crate::{
    SimpleStateTable,
    ast::{RcNode, Statement},
};

mod atoms;
mod definitions;
mod expressions;
mod statements;

pub fn parser<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    todo()
}

// fn atom<'src>() -> impl Parser<
//     'src,
//     &'src [Token],
//     RcNode<Statement>,
//     extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
// > + Clone {
//     let qstring = qstring();
//     let ident = ident();

//     qstring.or(ident).map(Statement::from).map(rc_node)
// }

#[cfg(test)]
mod tests {
    use lexer::utils::read_sample_file_to_string;

    use crate::StateTable;

    #[test]
    fn parse_full_01() {
        let name = "01_table_minimal.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_02() {
        let name = "02_table_roll_def.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_03() {
        let name = "03_table_list.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_04() {
        let name = "04_table_keyed_numeric.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_05() {
        let name = "05_table_keyed_word.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_06() {
        let name = "06_table_group.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_11() {
        let name = "11_statement_assignment_expression.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_12() {
        let name = "12_statement_clear.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_13() {
        let name = "13_statement_invoke.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_14() {
        let name = "14_statement_load.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_15() {
        let name = "15_statement_lookup.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_16() {
        let name = "16_statement_modify.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_17() {
        let name = "17_statement_output.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_18() {
        let name = "18_statement_roll.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_19() {
        let name = "19_statement_show.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }

    #[test]
    fn parse_full_21() {
        let name = "21_script.tale";
        let source = read_sample_file_to_string(name);
        let mut table = StateTable::new();
        table.add_source(name.to_string(), source);
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(errors, "");
    }
}
