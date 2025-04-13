use crate::lexer::Token;
use chumsky::prelude::*;
use definitions::{script, table, table_group};
use statements::seq_or_statement;

use crate::{
    ast::{RcNode, Statement, full_rc_node},
    state::SimpleParserState,
};

pub use atoms::Op;

mod atoms;
mod definitions;
mod expressions;
mod statements;

pub fn parser<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    table()
        .or(table_group())
        .or(script())
        .or(seq_or_statement().then_ignore(just(Token::NewLines).ignored().or(end())))
        .or(just(Token::Tabs)
            .or_not()
            .then(just(Token::NewLines))
            .ignored()
            .map_with(|_, extra| full_rc_node(Statement::Empty, extra)))
        .repeated()
        .collect::<Vec<_>>()
        .map_with(|items, extra| {
            // Combine all parsed statements into a single vec
            if items.is_empty() {
                full_rc_node(Statement::Empty, extra) // Handle empty case
            } else {
                full_rc_node(Statement::Sequence(full_rc_node(items, extra)), extra)
            }
        })
}

#[cfg(test)]
#[allow(unused_must_use)]
mod tests {
    use crate::samples::*;

    use crate::state::StateTable;

    #[test]
    fn parse_full_01() {
        let name = "01_table_minimal.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), TABLE_MINIMAL.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_02() {
        let name = "02_table_roll_def.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), TABLE_ROLL_DEF.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_03() {
        let name = "03_table_list.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), TABLE_LIST.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_04() {
        let name = "04_table_keyed_numeric.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), TABLE_KEYED_NUMERIC.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_05() {
        let name = "05_table_keyed_word.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), TABLE_KEYED_WORD.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_06() {
        let name = "06_table_group.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), TABLE_GROUP.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_10() {
        let name = "10_statement_expression.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), STATEMENT_EXPRESSION.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_11() {
        let name = "11_statement_assignment.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), STATEMENT_ASSIGNMENT.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_12() {
        let name = "12_statement_clear.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), STATEMENT_CLEAR.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_13() {
        let name = "13_statement_invoke.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), STATEMENT_INVOKE.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_14() {
        let name = "14_statement_load.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), STATEMENT_LOAD.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_15() {
        let name = "15_statement_lookup.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), STATEMENT_LOOKUP.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_16() {
        let name = "16_statement_modify.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), STATEMENT_MODIFY.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_17() {
        let name = "17_statement_output.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), STATEMENT_OUTPUT.to_string());
        table.lex_current();
        let errors = table.parse_current();
        println!(
            "{}",
            table
                .asts()
                .borrow()
                .get("17_statement_output.tale")
                .unwrap()
        );
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_18() {
        let name = "18_statement_roll.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), STATEMENT_ROLL.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_19() {
        let name = "19_statement_show.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), STATEMENT_SHOW.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }

    #[test]
    fn parse_full_21() {
        let name = "21_script.tale";

        let table = StateTable::default();
        table.add_source(name.to_string(), SCRIPT.to_string());
        table.lex_current();
        let errors = table.parse_current();
        assert_eq!(format!("{:?}", errors), "Ok(())");
    }
}
