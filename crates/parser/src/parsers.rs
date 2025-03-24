use chumsky::{combinator::To, error::Simple, pratt::*, prelude::*, span};
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
    words().map_with(full_rc_node)
}

// pub fn assignment<'src>() -> impl Parser<
//     'src,
//     &'src [Token],
//     RcNode<Statement>,
//     extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>,
// > + Clone {
//     just(Token::Set)
// }

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

fn words<'src>()
-> impl Parser<'src, &'src [Token], RcNode<Expr>, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    word()
        .or(raw_keywords())
        .or(typical_punctuation())
        .repeated()
        .collect::<Vec<_>>()
        .map_with(|_, extra| {
            let span = extra.span().into_range();
            Atom::Str(extra.state().get_source_slice(&span).to_string())
        }).map_with(full_rc_node)
}

fn raw_keywords<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    select! {
        Token::Once => Atom::Raw(Token::Once),
        Token::Twice => Atom::Raw(Token::Twice),
        Token::Thrice => Atom::Raw(Token::Thrice),

        Token::One => Atom::Raw(Token::One),
        Token::Two => Atom::Raw(Token::Two),
        Token::Three => Atom::Raw(Token::Three),
        Token::Four => Atom::Raw(Token::Four),
        Token::Five => Atom::Raw(Token::Five),
        Token::Six => Atom::Raw(Token::Six),
        Token::Seven => Atom::Raw(Token::Seven),
        Token::Eight => Atom::Raw(Token::Eight),
        Token::Nine => Atom::Raw(Token::Nine),
        Token::Ten => Atom::Raw(Token::Ten),

        Token::All => Atom::Raw(Token::All),
        Token::And => Atom::Raw(Token::And),
        Token::Clear => Atom::Raw(Token::Clear),
        Token::End => Atom::Raw(Token::End),
        Token::Group => Atom::Raw(Token::Group),
        Token::Invoke => Atom::Raw(Token::Invoke),
        Token::List => Atom::Raw(Token::List),
        Token::Load => Atom::Raw(Token::Load),
        Token::Lookup => Atom::Raw(Token::Lookup),
        Token::Modify => Atom::Raw(Token::Modify),
        Token::Next => Atom::Raw(Token::Next),
        Token::On => Atom::Raw(Token::On),
        Token::Output => Atom::Raw(Token::Output),
        Token::Roll => Atom::Raw(Token::Roll),
        Token::Set => Atom::Raw(Token::Set),
        Token::Show => Atom::Raw(Token::Show),
        Token::Table => Atom::Raw(Token::Table),
        Token::Tag => Atom::Raw(Token::Tag),
        Token::Time => Atom::Raw(Token::Time),
        Token::To => Atom::Raw(Token::To),
    }
}

fn typical_punctuation<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    select! {
        Token::Comma => Atom::Raw(Token::Comma),
        Token::Period => Atom::Raw(Token::Period),
        Token::Ellipsis => Atom::Raw(Token::Ellipsis),
        Token::Bang => Atom::Raw(Token::Bang),
        Token::Question => Atom::Raw(Token::Question),
        Token::Colon => Atom::Raw(Token::Colon),
        Token::SemiColon => Atom::Raw(Token::SemiColon),
        Token::LParens => Atom::Raw(Token::LParens),
        Token::RParens => Atom::Raw(Token::RParens),
        Token::Dash => Atom::Raw(Token::Dash),
        Token::Minus => Atom::Raw(Token::Minus),
    }
}

fn ident<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    word().foldl(word().repeated(), ident_normalize)
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
    use logos::Logos;

    use crate::StateTable;

    use super::*;

    #[test]
    fn it_works() {
        let tokens = vec![Token::String("stuff".into()), Token::All];
        let mut table = StateTable::new();
        let mut state = SimpleState::from(&mut table);
        let stuff = parser().parse_with_state(&tokens, &mut state);
        println!("{:?}", stuff);
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

    #[test]
    fn test_words() {
        let mut table = StateTable::new();
        table.add_source("test".into(), r"this is a test: once upon a time...".into());
        table.lex_source("test".into());
        table.parse_source("test".into());
        let output = table.asts.get("test").unwrap();
        // let mut state = SimpleState::from(&mut table);
        // let tokens = Token::lexer("this is a test").flatten().collect::<Vec<_>>();
        // let output = words().parse_with_state(&tokens, &mut state);
        println!("{:?}", output);
        assert!(false);
    }
}
