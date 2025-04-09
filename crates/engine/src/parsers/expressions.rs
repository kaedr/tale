use crate::lexer::Token;
use chumsky::{pratt::*, prelude::*};

use crate::{
    state::SimpleStateTable,
    ast::{Atom, Expr, RcNode, full_rc_node},
};

use super::atoms::{self, ident_maybe_sub, number, qstring, terminator, words};

pub fn any_expr<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    roll()
        .or(lookup())
        .or(arithmetic().then_ignore(terminator()))
        .or(interpolation())
        .boxed()
        .labelled("Any Expression")
}

pub fn roll<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    let optional_roll = just(Token::Roll).or_not().ignore_then(repetition_clause());
    let optional_repetition =
        just(Token::Roll).ignore_then(repetition_clause().or_not().map_with(|maybe_rep, extra| {
            maybe_rep.unwrap_or(full_rc_node(Atom::Number(1), extra))
        }));

    optional_repetition
        .then(roll_predicate().or_not())
        .map_with(|(lhs, rhs), extra| {
            if let Some(rhs) = rhs {
                (lhs, rhs)
            } else {
                (full_rc_node(Atom::Number(1), extra), lhs)
            }
        })
        .or(optional_roll.then(roll_predicate()))
        .map_with(|(lhs, rhs), extra| full_rc_node(Expr::Roll(lhs, rhs), extra))
        .boxed()
        .labelled("Roll Expression")
}

fn roll_predicate<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    let pred_one = just(Token::On).or_not().ignore_then(arithmetic());
    let pred_two = just(Token::On)
        .then(just(Token::Table).then(just(Token::Colon)).or_not())
        .or_not()
        .ignore_then(ident_maybe_sub())
        .map_with(full_rc_node);
    let pred_three = just(Token::On).or_not().ignore_then(
        just(Token::Table)
            .then(just(Token::Colon))
            .ignore_then(interpolation()),
    );
    pred_one
        .then_ignore(terminator())
        .or(pred_two)
        .then_ignore(terminator())
        .or(pred_three)
        .then_ignore(terminator())
        .boxed()
        .labelled("Roll Predicate")
}

fn repetition_clause<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
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
            .then_ignore(just(Token::Time).or(just(Token::Roll)).or_not().labelled("'Times/Rolls'"))
            )
        .boxed()
        .labelled("Repetition Clause")
}

pub fn lookup<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    just(Token::Lookup)
        .ignore_then(arithmetic().or(words()))
        .then_ignore(just(Token::On))
        .then(ident_maybe_sub().map_with(full_rc_node))
        .then_ignore(terminator())
        .map_with(|(lhs, rhs), extra| full_rc_node(Expr::Lookup(lhs, rhs), extra))
        .boxed()
        .labelled("Lookup Expression")
}

pub fn interpolation<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    let qstr_map = qstring().map_with(full_rc_node);

    let words_map = words();

    qstr_map
        .or(words_map)
        .or(embed_expr())
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .then_ignore(terminator())
        .map_with(full_rc_node)
        .map_with(|items, extra| full_rc_node(Expr::Interpol(items), extra))
        .boxed()
        .labelled("Interpolation")
}

fn embed_expr<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    implied_roll_expr()
        .then_ignore(terminator())
        .or(arithmetic().then_ignore(terminator()))
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .boxed()
}

pub fn implied_roll_expr<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    ident_maybe_sub()
        .map_with(full_rc_node)
        .map_with(|target, extra| {
            let implied_rep = full_rc_node(Atom::Number(1), extra);
            full_rc_node(Expr::Roll(implied_rep, target), extra)
        })
}

pub fn arithmetic<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
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
        extra::Full<Rich<'src, Token>, SimpleStateTable, ()>,
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
        extra::Full<Rich<'src, Token>, SimpleStateTable, ()>,
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

pub fn number_range_list<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
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

    use crate::lexer::tests::quick_tokens;

    use crate::{
        state::StateTable, parsers::expressions::arithmetic, tests::stubbed_parser,
        utils::tests::read_sample_lines,
    };

    #[test]
    fn parse_roll() {
        let mut table = StateTable::new();
        let check_vals = vec![
            "Roll 1, 3d6",
            "Roll 1, (1d20 + 7)",
            "Roll 1, `magic item table a`",
            "Roll 7, `magic item table a`",
            "Roll 1d6, `magic item table a`",
            "Roll 1d6, `magic item table a`",
            "Roll (1d4 + 2), `farm animals`",
            "Roll (1d6 - 1), `farm animals`",
            "Roll (1d4 - 2), `farm animals`",
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
            let tokens = quick_tokens(line.unwrap().as_str());
            let output = stubbed_parser(&mut table, &tokens, roll());
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let tokens = quick_tokens("Roll 2 @");
        let output = stubbed_parser(&mut table, &tokens, roll());
        assert_eq!(
            "[found 'At' at 2..3 expected Arithmetic Operator, 'LParens', 'Times/Rolls', Roll Predicate, or end of input in Arithmetic Expression at 1..2]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_roll_predicate() {
        let mut table = StateTable::new();
        let tokens = quick_tokens("On Table: Stuff");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("`stuff`", format!("{output}"));

        let tokens = quick_tokens(r#"On Table: "StringLike""#);
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("`stringlike`", format!("{output}"));

        let tokens = quick_tokens("on `magic item table a`");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("`magic item table a`", format!("{output}"));

        let tokens = quick_tokens("On Major: Minor");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("`major minor`", format!("{output}"));

        let tokens = quick_tokens("10 gp gems");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("`10 gp gems`", format!("{output}"));

        let tokens = quick_tokens("2d6 * 10");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("(2d6 * 10)", format!("{output}"));

        let tokens = quick_tokens("1");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("1", format!("{output}"));

        table.add_source(
            "test".into(),
            "On Table: Kingdom of [Current Kingdom]".into(),
        );
        table.lex_current();
        let tokens = &table.get_tokens("test").unwrap();
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!(
            r#"!["Kingdom of", Roll 1, `current kingdom`]!"#,
            format!("{output}")
        );

        let tokens = quick_tokens("Table: [Food]");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("![Roll 1, `food`]!", format!("{output}"));

        let tokens = quick_tokens("Table: @");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!(
            "[found 'At' at 2..3 expected Identity, Terminator, or Interpolation]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_repetition_clause() {
        let mut table = StateTable::new();
        let tokens = quick_tokens("1");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("1", format!("{output}"));

        let tokens = quick_tokens("once");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("1", format!("{output}"));

        let tokens = quick_tokens("thrice");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("3", format!("{output}"));

        let tokens = quick_tokens("one time");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("1", format!("{output}"));

        let tokens = quick_tokens("nine times");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("9", format!("{output}"));

        let tokens = quick_tokens("1d8");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("1d8", format!("{output}"));

        let tokens = quick_tokens("1d2 rolls");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("1d2", format!("{output}"));

        let tokens = quick_tokens("1d4 times");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("1d4", format!("{output}"));

        let tokens = quick_tokens("1d6 + 1 (5)");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("(1d6 + 1)", format!("{output}"));

        let tokens = quick_tokens("1d6 + 1 (@5)");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("[found 'At' at 4..5 expected Numeric]", format!("{output}"));
    }

    #[test]
    fn parse_lookup() {
        let mut table = StateTable::new();
        let check_vals = vec![
            "Lookup `a` on `textkeys`",
            "Lookup 3 on `numkeyed`",
            "Lookup 1d4 on `numkeyed`",
            "Lookup (1d3 + 1) on `numkeyed`",
        ];

        let lookup_lines = read_sample_lines("15_statement_lookup.tale").unwrap();
        for (index, line) in lookup_lines.enumerate() {
            let tokens = quick_tokens(line.unwrap().as_str());
            let output = stubbed_parser(&mut table, &tokens, lookup());
            assert_eq!(check_vals[index], format!("{output}"));
        }

        let tokens = quick_tokens("Lookup 2 @ TextKeys");
        let output = stubbed_parser(&mut table, &tokens, lookup());
        assert_eq!(
            "[found 'At' at 2..3 expected Arithmetic Operator, or 'On' in Arithmetic Expression at 1..2]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_interpolation() {
        let mut table = StateTable::new();
        table.add_source("test".into(), "valid".into());
        table.lex_current();
        let tokens = &table.get_tokens("test").unwrap();
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(r#"!["valid"]!"#, format!("{output}"));

        table.add_source("test".into(), "`also valid`".into());
        table.lex_current();
        let tokens = &table.get_tokens("test").unwrap();
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(r#"!["also valid"]!"#, format!("{output}"));

        table.add_source("test".into(), "weirdly enough `also valid`".into());
        table.lex_current();
        let tokens = &table.get_tokens("test").unwrap();
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(r#"!["weirdly enough", "also valid"]!"#, format!("{output}"));

        table.add_source("test".into(), "simple expression: [1 + 2]".into());
        table.lex_current();
        let tokens = &table.get_tokens("test").unwrap();
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(r#"!["simple expression:", (1 + 2)]!"#, format!("{output}"));

        table.add_source("test".into(), "another [1d4 + 3] things".into());
        table.lex_current();
        let tokens = &table.get_tokens("test").unwrap();
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(r#"!["another", (1d4 + 3), "things"]!"#, format!("{output}"));

        table.add_source("test".into(), "what's in the [Box]?".into());
        table.lex_current();
        let tokens = &table.get_tokens("test").unwrap();
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(
            r#"!["what's in the", Roll 1, `box`, "?"]!"#,
            format!("{output}")
        );

        table.add_source("test".into(), "[one @ two]".into());
        table.lex_current();
        let tokens = &table.get_tokens("test").unwrap();
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(
            "[found 'At' at 2..3 expected Wordlike, 'Colon', Identity, Terminator, or Arithmetic Operator]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_arithmetic() {
        let mut table = StateTable::new();
        let tokens = quick_tokens("1");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("1", format!("{output}"));

        let tokens = quick_tokens("-1");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("-1", format!("{output}"));

        let tokens = quick_tokens("---1");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("---1", format!("{output}"));

        let tokens = quick_tokens("-1 - 2");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("(-1 - 2)", format!("{output}"));

        let tokens = quick_tokens("-(1 - 2)");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("-(1 - 2)", format!("{output}"));

        let tokens = quick_tokens("2d6 + 3");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("(2d6 + 3)", format!("{output}"));

        let tokens = quick_tokens("one + two");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("(1 + 2)", format!("{output}"));

        let tokens = quick_tokens("99 * stored_value");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("(99 * `stored_value`)", format!("{output}"));

        let tokens = quick_tokens("no-spaces");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("(`no` - `spaces`)", format!("{output}"));

        let tokens = quick_tokens("PEMDAS_test +1-2*3/4%5^6^(7+8)");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!(
            "((`pemdas_test` + 1) - (((2 * 3) / 4) % (5 ^ (6 ^ (7 + 8)))))",
            format!("{output}")
        );

        let tokens = quick_tokens("one @ two");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!(
            "[found 'At' at 1..2 expected Arithmetic Operator, or end of input in Arithmetic Expression at 0..1]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_number_range_list() {
        let mut table = StateTable::new();
        let tokens = quick_tokens("1");
        let output = stubbed_parser(&mut table, &tokens, number_range_list());
        assert_eq!("[1]", format!("{output}"));

        let tokens = quick_tokens("1-3");
        let output = stubbed_parser(&mut table, &tokens, number_range_list());
        assert_eq!("[1, 2, 3]", format!("{output}"));

        let tokens = quick_tokens("97-00");
        let output = stubbed_parser(&mut table, &tokens, number_range_list());
        assert_eq!("[97, 98, 99, 100]", format!("{output}"));

        let tokens = quick_tokens("1,3");
        let output = stubbed_parser(&mut table, &tokens, number_range_list());
        assert_eq!("[1, 3]", format!("{output}"));

        let tokens = quick_tokens("1-3,5,8-10");
        let output = stubbed_parser(&mut table, &tokens, number_range_list());
        assert_eq!("[1, 2, 3, 5, 8, 9, 10]", format!("{output}"));

        let tokens = quick_tokens("bacon");
        let output = stubbed_parser(&mut table, &tokens, number_range_list());
        assert_eq!(
            "[found 'Word(\"bacon\")' at 0..1 expected Number, Range, List]",
            format!("{output}")
        );
    }
}
