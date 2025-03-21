use chumsky::{combinator::To, error::Simple, prelude::*, Parser};
use lexer::Token;
use logos::Span;

use crate::ast::{Atom, Node, Statement};


fn parser() -> impl Parser<Token, Atom, Error = Simple<Token>> {
    let text = select! {
        Token::String(s) => Atom::Str(s)
    };
    text.then_ignore(end())
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let tokens = vec![Token::String("stuff".into()), Token::All];
        let stuff = parser().parse(tokens);
        println!("{:?}", stuff);
        assert!(false);
    }
}
