use crate::lexer::Token;
use chumsky::{
    pratt::{infix, left, prefix, right},
    prelude::*,
};

use crate::{
    ast::{Atom, Expr, RcNode, full_rc_node},
    state::SimpleParserState,
};

use super::{
    TaleExtra,
    atoms::{
        self, DELIMITING_WHITESPACE, RBRACKET, ident_maybe_sub, number, qstring, terminator,
        value_name, words,
    },
};

pub fn any_expr<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone {
    roll(end_tokens)
        .or(lookup(end_tokens))
        .or(arithmetic().then_ignore(terminator(end_tokens)))
        .or(interpolation())
        .boxed()
        .labelled("Any Expression")
}

pub fn roll<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone {
    let optional_roll = just(Token::Roll).or_not().ignore_then(repetition_clause());
    let optional_repetition =
        just(Token::Roll).ignore_then(repetition_clause().or_not().map_with(|maybe_rep, extra| {
            maybe_rep.unwrap_or(full_rc_node(Atom::Number(1), extra))
        }));

    optional_repetition
        .then(roll_predicate(end_tokens).or_not())
        .map_with(|(lhs, rhs), extra| {
            if let Some(rhs) = rhs {
                (lhs, rhs)
            } else {
                (full_rc_node(Atom::Number(1), extra), lhs)
            }
        })
        .or(optional_roll.then(roll_predicate(end_tokens)))
        .map_with(|(lhs, rhs), extra| {
            let roll_expr_node = full_rc_node(Expr::Roll(lhs, rhs), extra);
            if extra.slice().iter().all(|token| match token {
                // Keywords & Dice/digits
                Token::Roll | Token::Time | Token::On | Token::DieRoll(_) | Token::Digits(_) => {
                    false
                }
                _ => true,
            }) {
                let span = extra.span().into_range();
                roll_expr_node.add_detail(
                    "words_only".into(),
                    extra.state().get_source_slice(&span).to_string(),
                );
            }
            roll_expr_node
        })
        .boxed()
        .labelled("Roll Expression")
}

fn roll_predicate<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone {
    let pred_one = just(Token::On).or_not().ignore_then(arithmetic());
    let pred_two = just(Token::On)
        .then(just(Token::Table).then(just(Token::Colon)).or_not())
        .or_not()
        .ignore_then(ident_maybe_sub())
        .map_with(full_rc_node);
    let pred_three = just(Token::On).or_not().ignore_then(
        just(Token::Table)
            .then(just(Token::Colon))
            .ignore_then(minterpolation(end_tokens)),
    );
    pred_one
        .then_ignore(terminator(end_tokens))
        .or(pred_two)
        .then_ignore(terminator(end_tokens))
        .or(pred_three)
        .then_ignore(terminator(end_tokens))
        .boxed()
        .labelled("Roll Predicate")
}

fn repetition_clause<'src>()
-> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone {
    let repetition_word = select! {
        Token::Once => Atom::Number(1),
        Token::Twice => Atom::Number(2),
        Token::Thrice => Atom::Number(3),
    };
    repetition_word
        .map_with(full_rc_node)
        .or(arithmetic()
            .then_ignore(
                number()
                    .delimited_by(just(Token::LParens), just(Token::RParens))
                    .or_not(),
            )
            .then_ignore(
                just(Token::Time)
                    .or(just(Token::Roll))
                    .or_not()
                    .labelled("'Times/Rolls'"),
            ))
        .boxed()
        .labelled("Repetition Clause")
}

pub fn lookup<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone {
    just(Token::Lookup)
        .ignore_then(arithmetic().or(words()))
        .then_ignore(just(Token::On))
        .then(ident_maybe_sub().map_with(full_rc_node))
        .then_ignore(terminator(end_tokens))
        .map_with(|(lhs, rhs), extra| full_rc_node(Expr::Lookup(lhs, rhs), extra))
        .boxed()
        .labelled("Lookup Expression")
}

pub fn interpolation<'src>()
-> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone {
    let qstr_map = qstring().map_with(full_rc_node);

    qstr_map
        .or(words())
        .or(embed_expr())
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .then_ignore(terminator(DELIMITING_WHITESPACE))
        .map_with(full_rc_node)
        .map_with(|items, extra| full_rc_node(Expr::Interpol(items), extra))
        .boxed()
        .labelled("Interpolation")
}

/// Minimal interpolation
pub fn minterpolation<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone {
    let qstr_map = qstring().map_with(full_rc_node);
    let value_map = value_name()
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map_with(full_rc_node);
    qstr_map
        .or(words())
        .or(value_map)
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .then_ignore(terminator(end_tokens))
        .map_with(full_rc_node)
        .map_with(|items, extra| full_rc_node(Expr::Interpol(items), extra))
        .boxed()
        .labelled("Mini Interpolation")
}

fn embed_expr<'src>() -> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone {
    implied_roll_expr()
        .or(roll(RBRACKET))
        .or(lookup(RBRACKET))
        .then_ignore(terminator(RBRACKET))
        .or(arithmetic().then_ignore(terminator(RBRACKET)))
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .boxed()
}

pub fn implied_roll_expr<'src>()
-> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone {
    ident_maybe_sub()
        .map_with(full_rc_node)
        .map_with(|target, extra| {
            let implied_rep = full_rc_node(Atom::Number(1), extra);
            full_rc_node(Expr::Roll(implied_rep, target), extra)
        })
}

pub fn arithmetic<'src>() -> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone
{
    recursive(|arith| {
        let term = atoms::term().or(arith.delimited_by(just(Token::LParens), just(Token::RParens)));
        term.pratt((
            // Exponentiation
            infix(right(4), atoms::op(Token::Caret), fold_infix),
            // Negation
            prefix(3, atoms::op(Token::Minus), fold_prefix),
            // Multiplication and Division
            infix(left(2), atoms::op(Token::Asterisk), fold_infix),
            infix(left(2), atoms::op(Token::Slash), fold_infix),
            infix(left(2), atoms::op(Token::Modulo), fold_infix),
            // Addition and Subtraction
            infix(left(1), atoms::op(Token::Plus), fold_infix),
            infix(left(1), atoms::op(Token::Minus), fold_infix),
        ))
    })
    .boxed()
    .labelled("Arithmetic Expression")
    .as_context()
}

fn fold_prefix<'src>(
    op: atoms::Op,
    rhs: RcNode<Expr>,
    extra: &mut chumsky::input::MapExtra<
        'src,
        '_,
        &'src [Token],
        extra::Full<Rich<'src, Token>, SimpleParserState, ()>,
    >,
) -> RcNode<Expr> {
    match op {
        atoms::Op::Sub => full_rc_node(Expr::Neg(rhs), extra),
        _ => unreachable!(),
    }
}

fn fold_infix<'src>(
    lhs: RcNode<Expr>,
    op: atoms::Op,
    rhs: RcNode<Expr>,
    extra: &mut chumsky::input::MapExtra<
        'src,
        '_,
        &'src [Token],
        extra::Full<Rich<'src, Token>, SimpleParserState, ()>,
    >,
) -> RcNode<Expr> {
    match op {
        atoms::Op::Add => full_rc_node(Expr::Add(lhs, rhs), extra),
        atoms::Op::Sub => full_rc_node(Expr::Sub(lhs, rhs), extra),
        atoms::Op::Mul => full_rc_node(Expr::Mul(lhs, rhs), extra),
        atoms::Op::Div => full_rc_node(Expr::Div(lhs, rhs), extra),
        atoms::Op::Mod => full_rc_node(Expr::Mod(lhs, rhs), extra),
        atoms::Op::Pow => full_rc_node(Expr::Pow(lhs, rhs), extra),
    }
}

pub fn number_range_list<'src>()
-> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone {
    let num_range = number()
        .then(
            one_of([Token::Minus, Token::Dash, Token::Ellipsis])
                .ignore_then(number())
                .or_not(),
        )
        .map(|(lhs, rhs)| {
            if let Some(rhs) = rhs {
                lhs.range(&rhs)
            } else {
                vec![lhs]
            }
        });

    num_range
        .clone()
        .foldl(
            just(Token::Comma).ignore_then(num_range).repeated(),
            |mut lhs, mut rhs| {
                lhs.append(&mut rhs);
                lhs
            },
        )
        .map_with(|items, extra| full_rc_node(Expr::List(full_rc_node(items, extra)), extra))
        .labelled("Number, Range, List")
}

#[cfg(test)]
#[allow(unused_must_use)]
mod tests {
    use super::*;

    use crate::parsers::tests::EOI_ONLY;
    use crate::state::ParserState;
    use crate::{
        parsers::expressions::arithmetic, tests::stubbed_parser, utils::tests::read_sample_lines,
    };

    #[test]
    fn parse_roll() {
        let check_vals = [
            "Roll 1, 3d6",
            "Roll 1, (1d20 + 7)",
            "Roll 1, `magic item table a`",
            "Roll 7, `magic item table a`",
            "Roll 1d6, `magic item table a`",
            "Roll 1d6, `magic item table a`",
            "Roll (1d4 + 2), `farm animals`",
            "Roll (1d6 - 1), `farm animals`",
            "Roll 1d6, `magic items #3`",
            // Parser can't tell in this situation that there's an ellided repetition here
            // and a table called farm animals until semantic analysis.
            "Roll `farm`, `animals`",
            "Roll 1, `farm animals`",
            "Roll 1, `farm animals`",
            "Roll 3, 1d6",
            "Roll 1d6, 1d6",
            "Roll 6, 3d6",
        ];

        let lookup_lines = read_sample_lines("18_statement_roll.tale").unwrap();
        for (index, line) in lookup_lines.enumerate() {
            let mut p_state = ParserState::from_source(line.unwrap());
            let tokens = p_state.tokens();
            let output = stubbed_parser(&mut p_state, &tokens, roll(EOI_ONLY));
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source("Roll 2 @".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, roll(EOI_ONLY));
        assert_eq!(
            "[TaleError { kind: Parse, span: 7..8, position: (1, 7), msg: \"found 'At' expected \
            Arithmetic Operator, 'LParens', 'Times/Rolls', Roll Predicate, or end of input In: \
            [Arithmetic Expression]\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_roll_predicate() {
        let mut p_state = ParserState::from_source("On Table: Stuff".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, roll_predicate(EOI_ONLY));
        assert_eq!("`stuff`", format!("{output}"));

        let mut p_state = ParserState::from_source(r#"On Table: "StringLike""#.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, roll_predicate(EOI_ONLY));
        assert_eq!("`stringlike`", format!("{output}"));

        let mut p_state = ParserState::from_source("on `magic item table a`".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, roll_predicate(EOI_ONLY));
        assert_eq!("`magic item table a`", format!("{output}"));

        let mut p_state = ParserState::from_source("On Major: Minor".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, roll_predicate(EOI_ONLY));
        assert_eq!("`major minor`", format!("{output}"));

        let mut p_state = ParserState::from_source("10 gp gems".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, roll_predicate(EOI_ONLY));
        assert_eq!("`10 gp gems`", format!("{output}"));

        let mut p_state = ParserState::from_source("2d6 * 10".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, roll_predicate(EOI_ONLY));
        assert_eq!("(2d6 * 10)", format!("{output}"));

        let mut p_state = ParserState::from_source("1".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, roll_predicate(EOI_ONLY));
        assert_eq!("1", format!("{output}"));

        let mut p_state = ParserState::from_source("On Table: Kingdom of [CurrentKingdom]".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, roll_predicate(EOI_ONLY));
        assert_eq!(r#"!["Kingdom of", `currentkingdom`]!"#, format!("{output}"));

        let mut p_state = ParserState::from_source("Table: [Food]".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, roll_predicate(EOI_ONLY));
        assert_eq!("![`food`]!", format!("{output}"));

        let mut p_state = ParserState::from_source("Table: @".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, roll_predicate(EOI_ONLY));
        assert_eq!(
            "[TaleError { kind: Parse, span: 7..8, position: (1, 7), msg: \"found 'At' expected \
            Identity, Terminator, or Mini Interpolation\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_repetition_clause() {
        let mut p_state = ParserState::from_source("1".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, repetition_clause());
        assert_eq!("1", format!("{output}"));

        let mut p_state = ParserState::from_source("once".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, repetition_clause());
        assert_eq!("1", format!("{output}"));

        let mut p_state = ParserState::from_source("thrice".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, repetition_clause());
        assert_eq!("3", format!("{output}"));

        let mut p_state = ParserState::from_source("one time".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, repetition_clause());
        assert_eq!("1", format!("{output}"));

        let mut p_state = ParserState::from_source("nine times".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, repetition_clause());
        assert_eq!("9", format!("{output}"));

        let mut p_state = ParserState::from_source("1d8".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, repetition_clause());
        assert_eq!("1d8", format!("{output}"));

        let mut p_state = ParserState::from_source("1d2 rolls".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, repetition_clause());
        assert_eq!("1d2", format!("{output}"));

        let mut p_state = ParserState::from_source("1d4 times".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, repetition_clause());
        assert_eq!("1d4", format!("{output}"));

        let mut p_state = ParserState::from_source("1d6 + 1 (5)".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, repetition_clause());
        assert_eq!("(1d6 + 1)", format!("{output}"));

        let mut p_state = ParserState::from_source("1d6 + 1 (@5)".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, repetition_clause());
        assert_eq!(
            "[TaleError { kind: Parse, span: 9..10, position: (1, 9), msg: \"found 'At' expected Numeric\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_lookup() {
        let check_vals = [
            "Lookup `a` on `textkeys`",
            "Lookup 3 on `numkeyed`",
            "Lookup 1d4 on `numkeyed`",
            "Lookup (1d3 + 1) on `numkeyed`",
        ];

        let lookup_lines = read_sample_lines("15_statement_lookup.tale").unwrap();
        for (index, line) in lookup_lines.enumerate() {
            let mut p_state = ParserState::from_source(line.unwrap());
            let tokens = p_state.tokens();
            let output = stubbed_parser(&mut p_state, &tokens, lookup(EOI_ONLY));
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let mut p_state = ParserState::from_source("Lookup 2 @ TextKeys".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, lookup(EOI_ONLY));
        assert_eq!(
            "[TaleError { kind: Parse, span: 9..10, position: (1, 9), msg: \"found 'At' expected \
            Arithmetic Operator, or 'On' In: [Arithmetic Expression]\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_interpolation() {
        let mut p_state = ParserState::from_source("valid".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, interpolation());
        assert_eq!(r#"!["valid"]!"#, format!("{output}"));

        let mut p_state = ParserState::from_source("`also valid`".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, interpolation());
        assert_eq!(r#"!["also valid"]!"#, format!("{output}"));

        let mut p_state = ParserState::from_source("weirdly enough `also valid`".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, interpolation());
        assert_eq!(r#"!["weirdly enough", "also valid"]!"#, format!("{output}"));

        let mut p_state = ParserState::from_source("simple expression: [1 + 2]".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, interpolation());
        assert_eq!(r#"!["simple expression:", (1 + 2)]!"#, format!("{output}"));

        let mut p_state = ParserState::from_source("another [1d4 + 3] things".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, interpolation());
        assert_eq!(r#"!["another", (1d4 + 3), "things"]!"#, format!("{output}"));

        let mut p_state = ParserState::from_source("what's in the [Box]?".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, interpolation());
        assert_eq!(
            r#"!["what's in the", Roll 1, `box`, "?"]!"#,
            format!("{output}")
        );

        let mut p_state =
            ParserState::from_source("Immortal, wisest and fairest of all beings.".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, any_expr(EOI_ONLY));
        assert_eq!(
            r#"!["Immortal, wisest and fairest of all beings."]!"#,
            format!("{output}")
        );

        let mut p_state = ParserState::from_source("[one @ two]".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, interpolation());
        assert_eq!(
            "[TaleError { kind: Parse, span: 5..6, position: (1, 5), msg: \"found 'At' expected \
            Wordlike, 'Colon', Identity, Terminator, or Arithmetic Operator\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_arithmetic() {
        let mut p_state = ParserState::from_source("1".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, arithmetic());
        assert_eq!("1", format!("{output}"));

        let mut p_state = ParserState::from_source("-1".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, arithmetic());
        assert_eq!("-1", format!("{output}"));

        let mut p_state = ParserState::from_source("---1".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, arithmetic());
        assert_eq!("---1", format!("{output}"));

        let mut p_state = ParserState::from_source("-1 - 2".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, arithmetic());
        assert_eq!("(-1 - 2)", format!("{output}"));

        let mut p_state = ParserState::from_source("-(1 - 2)".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, arithmetic());
        assert_eq!("-(1 - 2)", format!("{output}"));

        let mut p_state = ParserState::from_source("2d6 + 3".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, arithmetic());
        assert_eq!("(2d6 + 3)", format!("{output}"));

        let mut p_state = ParserState::from_source("one + two".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, arithmetic());
        assert_eq!("(1 + 2)", format!("{output}"));

        let mut p_state = ParserState::from_source("99 * stored_value".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, arithmetic());
        assert_eq!("(99 * `stored_value`)", format!("{output}"));

        let mut p_state = ParserState::from_source("no-spaces".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, arithmetic());
        assert_eq!("(`no` - `spaces`)", format!("{output}"));

        let mut p_state = ParserState::from_source("PEMDAS_test +1-2*3/4%5^6^(7+8)".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, arithmetic());
        assert_eq!(
            "((`pemdas_test` + 1) - (((2 * 3) / 4) % (5 ^ (6 ^ (7 + 8)))))",
            format!("{output}")
        );

        let mut p_state = ParserState::from_source("one @ two".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, arithmetic());
        assert_eq!(
            "[TaleError { kind: Parse, span: 4..5, position: (1, 4), msg: \"found 'At' expected \
            Arithmetic Operator, or end of input In: [Arithmetic Expression]\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_number_range_list() {
        let mut p_state = ParserState::from_source("1".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, number_range_list());
        assert_eq!("[1]", format!("{output}"));

        let mut p_state = ParserState::from_source("1-3".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, number_range_list());
        assert_eq!("[1, 2, 3]", format!("{output}"));

        let mut p_state = ParserState::from_source("97-00".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, number_range_list());
        assert_eq!("[97, 98, 99, 100]", format!("{output}"));

        let mut p_state = ParserState::from_source("1,3".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, number_range_list());
        assert_eq!("[1, 3]", format!("{output}"));

        let mut p_state = ParserState::from_source("1-3,5,8-10".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, number_range_list());
        assert_eq!("[1, 2, 3, 5, 8, 9, 10]", format!("{output}"));

        let mut p_state = ParserState::from_source("bacon".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, number_range_list());
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..5, position: (1, 0), msg: \"found \
            'Word(\\\"bacon\\\")' expected Number, Range, List\" }]",
            format!("{output}")
        );
    }
}
