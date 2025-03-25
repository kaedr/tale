use crate::{
    SimpleStateTable,
    ast::{Atom, Expr, RcNode, full_rc_node},
};
use chumsky::{Parser, error::Simple, extra, prelude::*};
use lexer::Token;

pub fn term<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    let (number, dice, value_name) = (number(), dice(), value_name());
    number.or(dice).or(value_name).map_with(full_rc_node)
}

pub enum Op {
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

pub fn op_parser<'src>(
    token: Token,
) -> impl Parser<'src, &'src [Token], Op, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    just(token).map(Op::from)
}

pub fn qstring<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    select! { Token::String(s) => Atom::Str(s) }
}

pub fn words<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    word()
        .or(raw_keywords())
        .or(typical_punctuation())
        .repeated()
        .collect::<Vec<_>>()
        .map_with(|_, extra| {
            let span = extra.span().into_range();
            Atom::Str(extra.state().get_source_slice(&span).to_string())
        })
        .map_with(full_rc_node)
}

pub fn raw_keywords<'src>()
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

pub fn typical_punctuation<'src>()
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

pub fn ident<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    word().foldl(word().repeated(), ident_normalize)
}

pub fn value_name<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    word()
}

pub fn word<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    select! { Token::Word(word) => Atom::Ident(word) }
}

pub fn dice<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    select! { Token::DieRoll((x, y)) => Atom::Dice(x, y) }
}

pub fn number<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    select! {
        Token::DoubleOught => Atom::Number(100),
        Token::Digits(number) => Atom::Number(number),
    }
}

pub fn ident_normalize(l: Atom, r: Atom) -> Atom {
    l.merge(&r)
}
