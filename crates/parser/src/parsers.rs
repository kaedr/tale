use chumsky::{error::Simple, pratt::*, prelude::*};
use lexer::Token;

use crate::{
    SimpleStateTable,
    ast::{Atom, Expr, RcNode, Statement, full_rc_node},
};

pub fn parser<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    arithmetic().map_with(full_rc_node)
}

// fn atom<'src>() -> impl Parser<
//     'src,
//     &'src [Token],
//     RcNode<Statement>,
//     extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>,
// > + Clone {
//     let qstring = qstring();
//     let ident = ident();

//     qstring.or(ident).map(Statement::from).map(rc_node)
// }

fn arithmetic<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    recursive(|arith| {
        let term = term().or(arith.delimited_by(just(Token::LParens), just(Token::RParens)));
        term.pratt((
            // Exponentiation
            infix(right(4), op_parser(Token::Caret), fold_infix),
            // Negation
            prefix(3, op_parser(Token::Minus), fold_prefix),
            // Multiplication and Division
            infix(left(2), op_parser(Token::Asterisk), fold_infix),
            infix(left(2), op_parser(Token::Slash), fold_infix),
            infix(left(2), op_parser(Token::Modulo), fold_infix),
            // Addition and Subtraction
            infix(left(1), op_parser(Token::Plus), fold_infix),
            infix(left(1), op_parser(Token::Minus), fold_infix),
        ))
    })
}

fn fold_prefix<'src>(
    op: Op,
    rhs: RcNode<Expr>,
    extra: &mut chumsky::input::MapExtra<
        'src,
        '_,
        &'src [Token],
        extra::Full<Simple<'src, Token>, SimpleStateTable, ()>,
    >,
) -> RcNode<Expr> {
    match op {
        Op::Sub => full_rc_node(Expr::Neg(rhs), extra),
        _ => unreachable!(),
    }
}

fn fold_infix<'src>(
    lhs: RcNode<Expr>,
    op: Op,
    rhs: RcNode<Expr>,
    extra: &mut chumsky::input::MapExtra<
        'src,
        '_,
        &'src [Token],
        extra::Full<Simple<'src, Token>, SimpleStateTable, ()>,
    >,
) -> RcNode<Expr> {
    match op {
        Op::Add => full_rc_node(Expr::Add(lhs, rhs), extra),
        Op::Sub => full_rc_node(Expr::Sub(lhs, rhs), extra),
        Op::Mul => full_rc_node(Expr::Mul(lhs, rhs), extra),
        Op::Div => full_rc_node(Expr::Div(lhs, rhs), extra),
        Op::Mod => full_rc_node(Expr::Mod(lhs, rhs), extra),
        Op::Pow => full_rc_node(Expr::Pow(lhs, rhs), extra),
    }
}

fn term<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    let (number, dice, value_name) = (number(), dice(), value_name());
    number.or(dice).or(value_name).map_with(full_rc_node)
}

enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}

impl From<Token> for Op {
    fn from(value: Token) -> Self {
        match value {
            Token::Plus => Op::Add,
            Token::Minus => Op::Sub,
            Token::Asterisk => Op::Mul,
            Token::Slash => Op::Div,
            Token::Modulo => Op::Mod,
            Token::Caret => Op::Pow,
            _ => unreachable!(),
        }
    }
}

fn op_parser<'src>(
    token: Token,
) -> impl Parser<'src, &'src [Token], Op, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    just(token).map(Op::from)
}

fn qstring<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    select! { Token::String(s) => Atom::Str(s) }
}

fn ident<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    let word = word();
    word.clone().foldl(word.repeated(), ident_normalize)
}

fn value_name<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    word()
}

fn word<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    select! { Token::Word(word) => Atom::Ident(word) }
}

fn dice<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    select! { Token::DieRoll((x, y)) => Atom::Dice(x, y) }
}

fn number<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    select! {
        Token::DoubleOught => Atom::Number(0),
        Token::Digits(number) => Atom::Number(number),
    }
}

fn ident_normalize(l: Atom, r: Atom) -> Atom {
    l.merge(&r)
}

#[cfg(test)]
mod tests {
    use chumsky::extra::SimpleState;

    use crate::StateTable;

    use super::*;

    #[test]
    fn it_works() {
        let tokens = vec![Token::String("stuff".into()), Token::All];
        let mut table = StateTable::new();
        let mut state = SimpleState::from(&mut table);
        let stuff = parser().parse_with_state(&tokens, &mut state);
        println!("{}", stuff.output().unwrap());
        assert!(false);
    }

    #[test]
    fn test_arithmetic() {
        let tokens = vec![
            Token::DoubleOught,
            Token::Plus,
            Token::Digits(1),
            Token::Asterisk,
            Token::Digits(2),
        ];
        let mut table = StateTable::new();
        let mut state = SimpleState::from(&mut table);
        let expr = arithmetic().parse_with_state(&tokens, &mut state);
        println!("{}", expr.output().unwrap());
        assert!(false);
    }
}
