use crate::lexer::Token;
use chumsky::prelude::*;

use crate::{
    ast::{Duration, Expr, Modifier, RcNode, Statement, full_rc_node},
    state::SimpleParserState,
};

use super::{
    atoms::{ident, ident_maybe_sub, number, qstring, terminator, value_name, words},
    expressions::{any_expr, arithmetic, implied_roll_expr, interpolation},
};

pub fn seq_or_statement<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    statement_sequence().or(any_statement()).boxed()
}

pub fn any_statement<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    nonce()
        .or(load())
        .or(output())
        .or(show())
        .or(chainable_statement())
        .and_is(
            just(Token::End)
                .then(one_of([Token::Script, Token::Table]))
                .not(),
        )
        .boxed()
        .labelled("Statement")
}

fn nonce<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    one_of([Token::Dash, Token::Minus])
        .then(terminator())
        .ignored()
        .map_with(|_, extra| full_rc_node(Statement::Empty, extra))
        .labelled("Nonce")
}

pub fn chainable_statement<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    assignment()
        .or(clear())
        .or(invoke())
        .or(modify())
        .or(expression())
        .boxed()
        .labelled("Chainable Statement")
}

pub fn statement_sequence<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    let chained = chainable_statement()
        .separated_by(just(Token::Comma).then(just(Token::And).or_not()).ignored())
        .at_least(1)
        .collect::<Vec<_>>();

    let implied_roll_expr = implied_roll_expr()
        .then_ignore(terminator())
        .map_with(|implied_roll, extra| vec![full_rc_node(implied_roll, extra)]);

    chained
        .or(any_statement().map(|stmt| vec![stmt]))
        .or(implied_roll_expr)
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map_with(full_rc_node)
        .map_with(|items, extra| full_rc_node(Statement::Sequence(items), extra))
        .boxed()
}

pub fn assignment<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    just(Token::Set)
        .or_not()
        .ignore_then(value_name().map_with(full_rc_node))
        .then_ignore(just(Token::Equals).or(just(Token::To)))
        .then(any_expr())
        .then_ignore(terminator())
        .map_with(|(lhs, rhs), extra| full_rc_node(Statement::Assignment(lhs, rhs), extra))
        .boxed()
        .labelled("Assignment Statement")
        .as_context()
}

pub fn expression<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    any_expr()
        .map_with(|expr, extra| full_rc_node(Statement::Expr(expr), extra))
        .boxed()
}

pub fn clear<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    just(Token::Clear)
        .ignore_then(duration().map_with(full_rc_node))
        .then(
            ident_maybe_sub()
                .then_ignore(just(Token::Roll).or_not())
                .then_ignore(terminator())
                .map_with(full_rc_node),
        )
        .map_with(|(lhs, rhs), extra| full_rc_node(Statement::Clear(lhs, rhs), extra))
        .boxed()
        .labelled("Clear Statement")
        .as_context()
}

pub fn invoke<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    just(Token::Invoke)
        .then(just(Token::Colon).or_not())
        .ignore_then(ident().then_ignore(terminator()))
        .map_with(full_rc_node)
        .map_with(|node, extra| full_rc_node(Statement::Invoke(node), extra))
        .boxed()
        .labelled("Invoke Statement")
        .as_context()
}

pub fn load<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
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

pub fn modify<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    let keyword_form = just(Token::Modify)
        .ignore_then(duration())
        .then(ident_maybe_sub())
        .then_ignore(just(Token::Roll))
        .then(mod_by())
        .then_ignore(terminator())
        .map_with(|((dur, id), value), extra| {
            let modifier = Modifier::new(dur, value);
            full_rc_node(
                Statement::Modify(full_rc_node(modifier, extra), full_rc_node(id, extra)),
                extra,
            )
        });
    let leading_form = mod_by()
        .then_ignore(just(Token::To))
        .then(duration())
        .then(ident_maybe_sub())
        .then_ignore(just(Token::Roll))
        .then_ignore(terminator())
        .map_with(|((value, dur), id), extra| {
            let modifier = Modifier::new(dur, value);
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

fn duration<'src>()
-> impl Parser<'src, &'src [Token], Duration, extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>>
+ Clone {
    just(Token::All)
        .map(|_| Duration::All)
        .or(just(Token::Next)
            .ignore_then(arithmetic())
            .map(|val| Duration::Next(val)))
        .boxed()
}

pub fn mod_by<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    one_of([Token::Plus, Token::Minus])
        .then(number().map_with(full_rc_node))
        .map_with(|(sign, value), extra| match sign {
            Token::Plus => value,
            Token::Minus => full_rc_node(Expr::Neg(value), extra),
            _ => unreachable!(),
        })
        .boxed()
}

pub fn output<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    just(Token::Output)
        .ignore_then(just(Token::Colon).or_not())
        .ignore_then(interpolation())
        .map_with(|value, extra| full_rc_node(Statement::Output(value), extra))
        .boxed()
        .labelled("Output Statement")
        .as_context()
}

pub fn show<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    just(Token::Show)
        .ignore_then(just(Token::Tag).or_not())
        .then_ignore(just(Token::Colon).or_not())
        .then(ident_maybe_sub())
        .then_ignore(terminator())
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

    use crate::state::ParserState;
    use crate::{tests::stubbed_parser, utils::tests::read_sample_lines};

    use super::*;

    #[test]
    fn parse_nonce() {
        let mut p_state = ParserState::from_source(r"-".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, any_statement());
        assert_eq!("Empty", format!("{output}"));

        let mut p_state = ParserState::from_source(r"–".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, any_statement());
        assert_eq!("Empty", format!("{output}"));

        let mut p_state = ParserState::from_source(r"—".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, any_statement());
        assert_eq!("Empty", format!("{output}"));

        let mut p_state = ParserState::from_source("".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, any_statement());
        assert_eq!(
            "[found end of input at 0..0 expected Statement]",
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

        let mut p_state = ParserState::from_source(r"A + B".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, statement_sequence());
        assert_eq!(
            "[found 'Word(\"A\")' at 0..1 expected 'LBracket']",
            format!("{output}")
        );

        let mut p_state = ParserState::from_source("".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, statement_sequence());
        assert_eq!(
            "[found end of input at 0..0 expected 'LBracket']",
            format!("{output}")
        );
    }

    #[test]
    fn parse_assignment() {
        let check_vals = vec![
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
            let output = stubbed_parser(&mut p_state, &tokens, assignment());
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source("".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, assignment());
        assert_eq!(
            "[found end of input at 0..0 expected Assignment Statement]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_clear() {
        let check_vals = vec![
            "Clear Next(10) `quality`",
            "Clear All `quality`",
            "Clear Next(10) `quality`",
            "Clear All `quality`",
        ];

        let lines = read_sample_lines("12_statement_clear.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let mut p_state = ParserState::from_source(line.unwrap());
            let tokens = p_state.tokens();
            let output = stubbed_parser(&mut p_state, &tokens, clear());
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source("".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, clear());
        assert_eq!(
            "[found end of input at 0..0 expected Clear Statement]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_invoke() {
        let check_vals = vec![
            "Invoke: `some kind of bizarre ritual`",
            "Invoke: `last rites`",
        ];

        let lines = read_sample_lines("13_statement_invoke.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let mut p_state = ParserState::from_source(line.unwrap());
            let tokens = p_state.tokens();
            let output = stubbed_parser(&mut p_state, &tokens, invoke());
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source("".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, invoke());
        assert_eq!(
            "[found end of input at 0..0 expected Invoke Statement]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_load() {
        let check_vals = vec![
            r#"Load: "01_table_minimal.tale""#,
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

        let mut p_state = ParserState::from_source("".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, load());
        assert_eq!(
            "[found end of input at 0..0 expected Load Statement]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_modify() {
        let check_vals = vec![
            "Modify +2 All `quality`",
            "Modify +3 Next(7) `quality`",
            "Modify -2 Next(3) `quality`",
            "Modify +10 All `quality`",
        ];

        let lines = read_sample_lines("16_statement_modify.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let mut p_state = ParserState::from_source(line.unwrap());
            let tokens = p_state.tokens();
            let output = stubbed_parser(&mut p_state, &tokens, modify());
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source("".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, modify());
        assert_eq!(
            "[found end of input at 0..0 expected Modify Statement]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_output() {
        let check_vals = vec![
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

        let mut p_state = ParserState::from_source("".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, output());
        assert_eq!(
            "[found end of input at 0..0 expected Output Statement]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_show() {
        let check_vals = vec![
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

        let mut p_state = ParserState::from_source("".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, show());
        assert_eq!(
            "[found end of input at 0..0 expected Show Statement]",
            format!("{output}")
        );
    }
}
