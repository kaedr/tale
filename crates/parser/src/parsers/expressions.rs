use chumsky::{pratt::*, prelude::*};
use lexer::Token;

use crate::{
    SimpleStateTable,
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
        .or(interpolation()).boxed()
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
        .map_with(|(lhs, rhs), extra| full_rc_node(Expr::Roll(lhs, rhs), extra)).boxed()
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
        .then_ignore(terminator()).boxed()
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
    repetition_word.map_with(full_rc_node).or(arithmetic()
        .then_ignore(
            number()
                .delimited_by(just(Token::LParens), just(Token::RParens))
                .or_not(),
        )
        .then_ignore(just(Token::Time).or(just(Token::Roll)).or_not())).boxed()
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
        .map_with(|(lhs, rhs), extra| full_rc_node(Expr::Lookup(lhs, rhs), extra)).boxed()
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
        .map_with(full_rc_node)
        .map_with(|items, extra| full_rc_node(Expr::Interpol(items), extra)).boxed()
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
        .delimited_by(just(Token::LBracket), just(Token::RBracket)).boxed()
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
    }).boxed()
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
}

#[cfg(test)]
mod tests {
    use super::*;

    use lexer::{quick_tokens, utils::read_sample_lines};

    use crate::{StateTable, parsers::expressions::arithmetic, tests::stubbed_parser};

    #[test]
    fn parse_roll() {
        let mut table = StateTable::new();
        let check_vals = vec![
            "Roll(Atom(1), Atom(3d6))",
            "Roll(Atom(1), Add(Atom(1d20) + Atom(7)))",
            "Roll(Atom(1), Atom(magic item table a))",
            "Roll(Atom(7), Atom(magic item table a))",
            "Roll(Atom(1d6), Atom(magic item table a))",
            "Roll(Atom(1d6), Atom(magic item table a))",
            "Roll(Add(Atom(1d4) + Atom(2)), Atom(farm animals))",
            "Roll(Sub(Atom(1d6) - Atom(1)), Atom(farm animals))",
            "Roll(Sub(Atom(1d4) - Atom(2)), Atom(farm animals))",
            // TODO: parser can't tell in this situation that there's an ellided repetition here
            // and a table called farm animals this will be able to be fixed once I actually have
            // the symbol table implemented
            "Roll(Atom(farm), Atom(animals))",
            "Roll(Atom(1), Atom(farm animals))",
            "Roll(Atom(1), Atom(farm animals))",
            "Roll(Atom(3), Atom(1d6))",
            "Roll(Atom(1d6), Atom(1d6))",
            "Roll(Atom(6), Atom(3d6))",
        ];

        let lookup_lines = read_sample_lines("18_statement_roll.tale").unwrap();
        for (index, line) in lookup_lines.enumerate() {
            let tokens = quick_tokens(line.unwrap().as_str());
            let output = stubbed_parser(&mut table, &tokens, roll());
            assert_eq!(check_vals[index], format!("{}", output));
        }

        let tokens = quick_tokens("Roll 2 @");
        let output = stubbed_parser(&mut table, &tokens, roll());
        assert_eq!(
            "[found 'At' at 2..3 expected 'Caret', 'Asterisk', 'Slash', 'Modulo', 'Plus', 'Minus'\
            , 'LParens', 'Time', 'Roll', 'On', something else, 'Table', or end of input]",
            format!("{}", output)
        );
        todo!("Fix aforementioned issue. Or maybe have the semantic analyzer handle it");
    }

    #[test]
    fn parse_roll_predicate() {
        let mut table = StateTable::new();
        let tokens = quick_tokens("On Table: Stuff");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("Atom(stuff)", format!("{}", output));

        let tokens = quick_tokens(r#"On Table: "StringLike""#);
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("Atom(stringlike)", format!("{}", output));

        let tokens = quick_tokens("on Magic Item Table A");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("Atom(magic item table a)", format!("{}", output));

        let tokens = quick_tokens("On Major: Minor");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("Atom(major minor)", format!("{}", output));

        let tokens = quick_tokens("10 gp gems");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("Atom(10 gp gems)", format!("{}", output));

        let tokens = quick_tokens("2d6 * 10");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("Mul(Atom(2d6) * Atom(10))", format!("{}", output));

        let tokens = quick_tokens("1");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("Atom(1)", format!("{}", output));

        table.add_source(
            "test".into(),
            "On Table: Kingdom of [Current Kingdom]".into(),
        );
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!(
            r#"Interpol(Atom("Kingdom of"), Roll(Atom(1), Atom(current kingdom)))"#,
            format!("{}", output)
        );

        let tokens = quick_tokens("Table: [Food]");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("Interpol(Roll(Atom(1), Atom(food)))", format!("{}", output));

        let tokens = quick_tokens("Table: @");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!(
            "[found 'At' at 2..3 expected something else, 'Comma', 'Period', 'NewLines'\
        , 'RBracket', 'SemiColon', 'Tabs', end of input, or 'LBracket']",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_repetition_clause() {
        let mut table = StateTable::new();
        let tokens = quick_tokens("1");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("Atom(1)", format!("{}", output));

        let tokens = quick_tokens("once");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("Atom(1)", format!("{}", output));

        let tokens = quick_tokens("thrice");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("Atom(3)", format!("{}", output));

        let tokens = quick_tokens("one time");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("Atom(1)", format!("{}", output));

        let tokens = quick_tokens("nine times");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("Atom(9)", format!("{}", output));

        let tokens = quick_tokens("1d8");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("Atom(1d8)", format!("{}", output));

        let tokens = quick_tokens("1d2 rolls");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("Atom(1d2)", format!("{}", output));

        let tokens = quick_tokens("1d4 times");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("Atom(1d4)", format!("{}", output));

        let tokens = quick_tokens("1d6 + 1 (5)");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!("Add(Atom(1d6) + Atom(1))", format!("{}", output));

        let tokens = quick_tokens("1d6 + 1 (@5)");
        let output = stubbed_parser(&mut table, &tokens, repetition_clause());
        assert_eq!(
            "[found 'At' at 4..5 expected something else]",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_lookup() {
        let mut table = StateTable::new();
        let check_vals = vec![
            "Lookup(Atom(a) on Atom(textkeys))",
            "Lookup(Atom(3) on Atom(numkeyed))",
            "Lookup(Atom(1d4) on Atom(numkeyed))",
            "Lookup(Add(Atom(1d3) + Atom(1)) on Atom(numkeyed))",
        ];

        let lookup_lines = read_sample_lines("15_statement_lookup.tale").unwrap();
        for (index, line) in lookup_lines.enumerate() {
            let tokens = quick_tokens(line.unwrap().as_str());
            let output = stubbed_parser(&mut table, &tokens, lookup());
            assert_eq!(check_vals[index], format!("{}", output));
        }

        let tokens = quick_tokens("Lookup 2 @ TextKeys");
        let output = stubbed_parser(&mut table, &tokens, lookup());
        assert_eq!(
            "[found 'At' at 2..3 expected 'Caret', 'Asterisk', 'Slash', 'Modulo', 'Plus', 'Minus', or 'On']",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_interpolation() {
        let mut table = StateTable::new();
        table.add_source("test".into(), "valid".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(r#"Interpol(Atom("valid"))"#, format!("{}", output));

        table.add_source("test".into(), "`also valid`".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(r#"Interpol(Atom("also valid"))"#, format!("{}", output));

        table.add_source("test".into(), "weirdly enough `also valid`".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(
            r#"Interpol(Atom("weirdly enough"), Atom("also valid"))"#,
            format!("{}", output)
        );

        table.add_source("test".into(), "simple expression: [1 + 2]".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(
            r#"Interpol(Atom("simple expression:"), Add(Atom(1) + Atom(2)))"#,
            format!("{}", output)
        );

        table.add_source("test".into(), "another [1d4 + 3] things".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(
            r#"Interpol(Atom("another"), Add(Atom(1d4) + Atom(3)), Atom("things"))"#,
            format!("{}", output)
        );

        table.add_source("test".into(), "what's in the [Box]?".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(
            r#"Interpol(Atom("what's in the"), Roll(Atom(1), Atom(box)), Atom("?"))"#,
            format!("{}", output)
        );

        table.add_source("test".into(), "[one @ two]".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(
            "[found 'At' at 2..3 expected something else, 'Colon', 'Comma', 'Period'\
        , 'NewLines', 'RBracket', 'SemiColon', 'Tabs', end of input, 'Caret', 'Asterisk', 'Slash'\
        , 'Modulo', 'Plus', or 'Minus']",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_arithmetic() {
        let mut table = StateTable::new();
        let tokens = quick_tokens("1");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("Atom(1)", format!("{}", output));

        let tokens = quick_tokens("-1");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("Neg(Atom(1))", format!("{}", output));

        let tokens = quick_tokens("---1");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("Neg(Neg(Neg(Atom(1))))", format!("{}", output));

        let tokens = quick_tokens("-1 - 2");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("Sub(Neg(Atom(1)) - Atom(2))", format!("{}", output));

        let tokens = quick_tokens("-(1 - 2)");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("Neg(Sub(Atom(1) - Atom(2)))", format!("{}", output));

        let tokens = quick_tokens("2d6 + 3");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("Add(Atom(2d6) + Atom(3))", format!("{}", output));

        let tokens = quick_tokens("one + two");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("Add(Atom(1) + Atom(2))", format!("{}", output));

        let tokens = quick_tokens("99 * stored_value");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("Mul(Atom(99) * Atom(stored_value))", format!("{}", output));

        let tokens = quick_tokens("no-spaces");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!("Sub(Atom(no) - Atom(spaces))", format!("{}", output));

        let tokens = quick_tokens("PEMDAS_test +1-2*3/4%5^6^(7+8)");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!(
            "Sub(Add(Atom(pemdas_test) + Atom(1)) - Mod(Div(Mul(Atom(2) * Atom(3)) / Atom(4)) % Pow(Atom(5) ^ Pow(Atom(6) ^ Add(Atom(7) + Atom(8))))))",
            format!("{}", output)
        );

        let tokens = quick_tokens("one @ two");
        let output = stubbed_parser(&mut table, &tokens, arithmetic());
        assert_eq!(
            "[found 'At' at 1..2 expected 'Caret', 'Asterisk', 'Slash', 'Modulo', 'Plus', 'Minus', or end of input]",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_number_range_list() {
        let mut table = StateTable::new();
        let tokens = quick_tokens("1");
        let output = stubbed_parser(&mut table, &tokens, number_range_list());
        assert_eq!("[Atom(1)]", format!("{}", output));

        let tokens = quick_tokens("1-3");
        let output = stubbed_parser(&mut table, &tokens, number_range_list());
        assert_eq!("[Atom(1), Atom(2), Atom(3)]", format!("{}", output));

        let tokens = quick_tokens("97-00");
        let output = stubbed_parser(&mut table, &tokens, number_range_list());
        assert_eq!(
            "[Atom(97), Atom(98), Atom(99), Atom(100)]",
            format!("{}", output)
        );

        let tokens = quick_tokens("1,3");
        let output = stubbed_parser(&mut table, &tokens, number_range_list());
        assert_eq!("[Atom(1), Atom(3)]", format!("{}", output));

        let tokens = quick_tokens("1-3,5,8-10");
        let output = stubbed_parser(&mut table, &tokens, number_range_list());
        assert_eq!(
            "[Atom(1), Atom(2), Atom(3), Atom(5), Atom(8), Atom(9), Atom(10)]",
            format!("{}", output)
        );
    }
}
