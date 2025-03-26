use chumsky::{Parser, error::Simple, extra, pratt::*, prelude::*};
use lexer::Token;

use crate::{
    SimpleStateTable,
    ast::{Expr, RcNode, full_rc_node},
};

use super::atoms;

pub fn arithmetic<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>,
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
        extra::Full<Simple<'src, Token>, SimpleStateTable, ()>,
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
        extra::Full<Simple<'src, Token>, SimpleStateTable, ()>,
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
