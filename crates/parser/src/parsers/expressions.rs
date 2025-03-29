use chumsky::{pratt::*, prelude::*};
use lexer::Token;

use crate::{
    SimpleStateTable,
    ast::{Atom, Expr, RcNode, full_rc_node},
};

use super::atoms::{self, ident_maybe_sub, number, terminator, words};

pub fn any_expr<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    roll().or(lookup()).or(interpolation()).or(arithmetic())
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
        .or(optional_roll)
        .then(roll_predicate().or_not())
        .map_with(|(lhs, rhs), extra| {
            if let Some(rhs) = rhs {
                full_rc_node(Expr::Roll(lhs, rhs), extra)
            } else {
                full_rc_node(Expr::Roll(full_rc_node(Atom::Number(1), extra), lhs), extra)
            }
        })
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
        .map(|stuff| println!("{:?}", stuff))
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
        .then_ignore(just(Token::Time).or(just(Token::Roll)).or_not()))
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
}

pub fn interpolation<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    todo()
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

        let tokens = quick_tokens("On Table: Kingdom of [Current Kingdom]");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!(
            r#"Interpol(Atom("Kingdom of"), Roll(Atom(1), Atom(current kingdom)))"#,
            format!("{}", output)
        );

        let tokens = quick_tokens("Table: [Food]");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("Atom(stuff)", format!("{}", output));

        let tokens = quick_tokens("Table: @");
        let output = stubbed_parser(&mut table, &tokens, roll_predicate());
        assert_eq!("[found 'At' at 2..3]", format!("{}", output));
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
        let tokens = quick_tokens("valid");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(r#"Interpol(Atom("valid"))"#, format!("{}", output));

        let tokens = quick_tokens("'also valid'");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(r#"Interpol(Atom("also valid"))"#, format!("{}", output));

        let tokens = quick_tokens("weirdly enough 'also valid'");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(
            r#"Interpol(Atom("weirdly enough"), Atom("also valid"))"#,
            format!("{}", output)
        );

        let tokens = quick_tokens("simple expression: [1 + 2]");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(
            r#"Interpol(Atom("simple expression"), Add(Atom(1) + Atom(2)))"#,
            format!("{}", output)
        );

        let tokens = quick_tokens("another [1d4 + 3] things");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(
            r#"Interpol(Atom("another"), Add(Atom(1d4) + Atom(3)), Atom("things"))"#,
            format!("{}", output)
        );

        let tokens = quick_tokens("what's in the [Box]?");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!(
            r#"Interpol(Atom("what's in the"), Roll(Atom(1), Atom(box)), Atom("?"))"#,
            format!("{}", output)
        );

        let tokens = quick_tokens("[one @ two]");
        let output = stubbed_parser(&mut table, &tokens, interpolation());
        assert_eq!("[found 'At' at 2..3]", format!("{}", output));
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
}
