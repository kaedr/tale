use chumsky::prelude::*;

use super::{
    TaleExtra,
    atoms::{
        COMMA_OR_RBRACKET, DELIMITING_WHITESPACE, RBRACKET, ident, ident_maybe_sub, number,
        qstring, terminator, value_name, words,
    },
    expressions::{any_expr, arithmetic, implied_roll_expr, interpolation},
};
use crate::{
    ast::{Duration, Expr, Modifier, RcNode, Statement, full_rc_node},
    lexer::Token,
};

pub fn seq_or_statement<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone {
    any_statement(end_tokens)
        .or(statement_sequence())
        .boxed()
        .labelled("Sequence or Statement")
}

pub fn any_statement<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone {
    nonce(end_tokens)
        .or(load())
        .or(output())
        .or(show())
        .or(chainable_statement(end_tokens))
        .and_is(
            just(Token::End)
                .then(one_of([Token::Script, Token::Table]))
                .not(),
        )
        .boxed()
        .labelled("Statement")
}

fn nonce<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone {
    one_of([Token::Dash, Token::Minus])
        .then(terminator(end_tokens))
        .ignored()
        .map_with(|(), extra| full_rc_node(Statement::Empty, extra))
        .labelled("Nonce")
}

pub fn chainable_statement<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone {
    assignment(end_tokens)
        .or(clear(end_tokens))
        .or(invoke(end_tokens))
        .or(modify(end_tokens))
        .or(expression(end_tokens))
        .boxed()
        .labelled("Chainable Statement")
}

pub fn statement_sequence<'src>()
-> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone {
    let chained = chainable_statement(COMMA_OR_RBRACKET)
        .separated_by(just(Token::Comma).then(just(Token::And).or_not()).ignored())
        .at_least(1)
        .collect::<Vec<_>>();

    let implied_roll_expr = implied_roll_expr()
        .then_ignore(terminator(RBRACKET))
        .map_with(|implied_roll, extra| vec![full_rc_node(implied_roll, extra)]);

    chained
        .or(any_statement(RBRACKET).map(|stmt| vec![stmt]))
        .or(implied_roll_expr)
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map_with(full_rc_node)
        .map_with(|items, extra| full_rc_node(Statement::Sequence(items), extra))
        .boxed()
}

pub fn assignment<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone {
    let explicit_form = just(Token::Set)
        .ignore_then(value_name().map_with(full_rc_node))
        .then_ignore(just(Token::Equals).or(just(Token::To)));

    let implicit_form = just(Token::Set)
        .or_not()
        .ignore_then(value_name().map_with(full_rc_node))
        .then_ignore(just(Token::Equals));

    explicit_form
        .or(implicit_form)
        .then(any_expr(end_tokens))
        .then_ignore(terminator(end_tokens))
        .map_with(|(lhs, rhs), extra| full_rc_node(Statement::Assignment(lhs, rhs), extra))
        .boxed()
        .labelled("Assignment Statement")
        .as_context()
}

pub fn clear<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone {
    just(Token::Clear)
        .ignore_then(duration().map_with(full_rc_node))
        .then(
            ident_maybe_sub()
                .then_ignore(just(Token::Roll).or_not())
                .then_ignore(terminator(end_tokens))
                .map_with(full_rc_node),
        )
        .map_with(|(lhs, rhs), extra| full_rc_node(Statement::Clear(lhs, rhs), extra))
        .boxed()
        .labelled("Clear Statement")
        .as_context()
}

pub fn expression<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone {
    any_expr(end_tokens)
        .map_with(|expr, extra| full_rc_node(Statement::Expr(expr), extra))
        .boxed()
}

pub fn invoke<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone {
    just(Token::Invoke)
        .then(just(Token::Colon).or_not())
        .ignore_then(ident().then_ignore(terminator(end_tokens)))
        .map_with(full_rc_node)
        .map_with(|node, extra| full_rc_node(Statement::Invoke(node), extra))
        .boxed()
        .labelled("Invoke Statement")
        .as_context()
}

pub fn load<'src>() -> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone
{
    just(Token::Load)
        .then(just(Token::Colon).or_not())
        .ignore_then(qstring().map_with(full_rc_node).or(words()))
        .then_ignore(
            one_of([Token::Tabs, Token::NewLines, Token::RBracket])
                .ignored()
                .rewind()
                .or(end()),
        )
        .map_with(|path, extra| full_rc_node(Statement::Load(path), extra))
        .boxed()
        .labelled("Load Statement")
        .as_context()
}

pub fn modify<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone {
    let keyword_form = just(Token::Modify)
        .ignore_then(duration())
        .then(ident_maybe_sub())
        .then_ignore(just(Token::Roll))
        .then(mod_by())
        .then_ignore(terminator(end_tokens))
        .map_with(|((dur, id), value), extra| {
            let modifier = Modifier::new(dur, value);
            full_rc_node(
                Statement::Modify(full_rc_node(modifier, extra), full_rc_node(id, extra)),
                extra,
            )
        });
    let leading_form = mod_by()
        .then_ignore(just(Token::To))
        .then(duration().or_not())
        .then(ident_maybe_sub())
        .then_ignore(just(Token::Roll))
        .then_ignore(terminator(end_tokens))
        .map_with(|((value, dur), id), extra| {
            let modifier = Modifier::new(dur.unwrap_or(Duration::All), value);
            full_rc_node(
                Statement::Modify(full_rc_node(modifier, extra), full_rc_node(id, extra)),
                extra,
            )
        });
    keyword_form
        .or(leading_form)
        .boxed()
        .labelled("Modify Statement")
        .as_context()
}

fn duration<'src>() -> impl Parser<'src, &'src [Token], Duration, TaleExtra<'src>> + Clone {
    just(Token::All)
        .map(|_| Duration::All)
        .or(just(Token::Next)
            .ignore_then(arithmetic())
            .map(Duration::Next))
        .boxed()
}

pub fn mod_by<'src>() -> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone {
    one_of([Token::Plus, Token::Minus])
        .then(number().map_with(full_rc_node))
        .map_with(|(sign, value), extra| match sign {
            Token::Plus => value,
            Token::Minus => full_rc_node(Expr::Neg(value), extra),
            _ => unreachable!(),
        })
        .boxed()
}

pub fn output<'src>() -> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone
{
    just(Token::Output)
        .ignore_then(just(Token::Colon).or_not())
        .ignore_then(interpolation())
        .map_with(|value, extra| full_rc_node(Statement::Output(value), extra))
        .boxed()
        .labelled("Output Statement")
        .as_context()
}

pub fn show<'src>() -> impl Parser<'src, &'src [Token], RcNode<Statement>, TaleExtra<'src>> + Clone
{
    just(Token::Show)
        .ignore_then(just(Token::Tag).or_not())
        .then_ignore(just(Token::Colon).or_not())
        .then(ident_maybe_sub())
        .then_ignore(terminator(DELIMITING_WHITESPACE))
        .map_with(|(tags, value), extra| {
            full_rc_node(
                Statement::Show(full_rc_node((tags.is_some(), value), extra)),
                extra,
            )
        })
        .boxed()
        .labelled("Show Statement")
        .as_context()
}

#[cfg(test)]
#[allow(unused_must_use)]
mod tests {

    use super::*;
    use crate::{
        parsers::tests::EOI_ONLY, state::ParserState, tests::stubbed_parser,
        utils::tests::read_sample_lines,
    };

    #[test]
    fn parse_nonce() {
        let mut p_state = ParserState::from_source(r"-".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, any_statement(EOI_ONLY));
        assert_eq!("Empty", format!("{output}"));

        let mut p_state = ParserState::from_source(r"–".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, any_statement(EOI_ONLY));
        assert_eq!("Empty", format!("{output}"));

        let mut p_state = ParserState::from_source(r"—".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, any_statement(EOI_ONLY));
        assert_eq!("Empty", format!("{output}"));

        let mut p_state = ParserState::from_source(String::new());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, any_statement(EOI_ONLY));
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..0, position: (0, 0), msg: \"found end of input \
            expected Statement\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_sequence() {
        let mut p_state = ParserState::from_source(r"[Easy Peasy]".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, statement_sequence());
        assert_eq!(
            "Sequence: [\n\tRoll `easy`, `peasy`\n]",
            format!("{output}")
        );

        let mut p_state = ParserState::from_source(r"[1d6 + 7]".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, statement_sequence());
        assert_eq!("Sequence: [\n\t(1d6 + 7)\n]", format!("{output}"));

        let mut p_state = ParserState::from_source(r"[Invoke taco, Invoke burrito]".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, statement_sequence());
        assert_eq!(
            "Sequence: [\n\tInvoke: `taco`,\n\tInvoke: `burrito`\n]",
            format!("{output}")
        );

        let mut p_state = ParserState::from_source(
            r#"[2 rolls on Table: "Magic Items #3", 1d2 rolls on Table: "Magic Items #10"]"#.into(),
        );
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, statement_sequence());
        assert_eq!(
            "Sequence: [\n\tRoll 2, `magic items #3`,\n\tRoll 1d2, `magic items #10`\n]",
            format!("{output}")
        );

        let mut p_state = ParserState::from_source(r"A + B".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, statement_sequence());
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..1, position: (1, 0), msg: \"found \
            'Word(\\\"A\\\")' expected 'LBracket'\" }]",
            format!("{output}")
        );

        let mut p_state = ParserState::from_source(String::new());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, statement_sequence());
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..0, position: (0, 0), msg: \"found end of input expected 'LBracket'\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_assignment() {
        let check_vals = [
            "Assignment: `bird` = 42",
            "Assignment: `the_word` = `bird`",
            r#"Assignment: `the_word` = !["bird"]!"#,
            r#"Assignment: `the_word` = !["bird"]!"#,
            "Assignment: `force` = (`mass` * `acceleration`)",
            "Assignment: `minutes` = `midnight`",
        ];

        let lines = read_sample_lines("11_statement_assignment.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let mut p_state = ParserState::from_source(line.unwrap());
            let tokens = p_state.tokens();
            let output = stubbed_parser(&mut p_state, &tokens, assignment(EOI_ONLY));
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source(String::new());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, assignment(EOI_ONLY));
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..0, position: (0, 0), msg: \"found end of input \
            expected Assignment Statement\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_clear() {
        let check_vals = [
            "Clear Next(10) `quality`",
            "Clear All `quality`",
            "Clear Next(10) `quality`",
            "Clear All `quality`",
        ];

        let lines = read_sample_lines("12_statement_clear.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let mut p_state = ParserState::from_source(line.unwrap());
            let tokens = p_state.tokens();
            let output = stubbed_parser(&mut p_state, &tokens, clear(EOI_ONLY));
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source(String::new());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, clear(EOI_ONLY));
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..0, position: (0, 0), msg: \"found end of input \
            expected Clear Statement\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_invoke() {
        let check_vals = [
            "Invoke: `some kind of bizarre ritual`",
            "Invoke: `last rites`",
        ];

        let lines = read_sample_lines("13_statement_invoke.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let mut p_state = ParserState::from_source(line.unwrap());
            let tokens = p_state.tokens();
            let output = stubbed_parser(&mut p_state, &tokens, invoke(EOI_ONLY));
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source(String::new());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, invoke(EOI_ONLY));
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..0, position: (0, 0), msg: \"found end of input \
            expected Invoke Statement\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_load() {
        let check_vals = [
            r#"Load: "01_table_minimal.tale""#,
            r#"Load: "src/samples/01_table_minimal.tale""#,
            r#"Load: "../../tons of _odd-characters_.tale""#,
            r#"Load: "../../tons of _odd-characters_.tale""#,
        ];

        let lines = read_sample_lines("14_statement_load.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let mut p_state = ParserState::from_source(line.unwrap());
            let tokens = p_state.tokens();
            let output = stubbed_parser(&mut p_state, &tokens, load());
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source(String::new());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, load());
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..0, position: (0, 0), msg: \"found end of input \
            expected Load Statement\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_modify() {
        let check_vals = [
            "Modify +2 All `quality`",
            "Modify +3 Next(7) `quality`",
            "Modify -2 Next(3) `quality`",
            "Modify +10 All `quality`",
        ];

        let lines = read_sample_lines("16_statement_modify.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let mut p_state = ParserState::from_source(line.unwrap());
            let tokens = p_state.tokens();
            let output = stubbed_parser(&mut p_state, &tokens, modify(EOI_ONLY));
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source(String::new());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, modify(EOI_ONLY));
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..0, position: (0, 0), msg: \"found end of input \
            expected Modify Statement\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_output() {
        let check_vals = [
            r#"Output: !["There are", (1d6 - 1), "lights illuminated out of a total of 5."]!"#,
            r#"Output: !["A lovely string"]!"#,
        ];

        let lines = read_sample_lines("17_statement_output.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let mut p_state = ParserState::from_source(line.unwrap());
            let tokens = p_state.tokens();
            let output = stubbed_parser(&mut p_state, &tokens, output());
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source(String::new());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, output());
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..0, position: (0, 0), msg: \"found end of input \
            expected Output Statement\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_show() {
        let check_vals = [
            "Show `minimalism`",
            "Show `midnight`",
            "Show `variables`",
            "Show `tables`",
            "Show `script`",
            "Show Tag `desert`",
        ];

        let lines = read_sample_lines("19_statement_show.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let mut p_state = ParserState::from_source(line.unwrap());
            let tokens = p_state.tokens();
            let output = stubbed_parser(&mut p_state, &tokens, show());
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source(String::new());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, show());
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..0, position: (0, 0), msg: \"found end of input \
            expected Show Statement\" }]",
            format!("{output}")
        );
    }
}
