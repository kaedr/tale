use chumsky::{Parser, combinator::To, error::Simple, prelude::*, primitive::Container};
use lexer::Token;
use logos::Span;

use crate::ast::{Atom, Node, Statement};

fn parser() -> impl Parser<Token, Atom, Error = Simple<Token>> {
    let text = qstring();
    text.then_ignore(end())
}

fn atom() -> impl Parser<Token, Atom, Error = Simple<Token>> + Clone {
    let text = qstring();
    let ident = ident();

    text.or(ident)
}

fn qstring() -> impl Parser<Token, Atom, Error = Simple<Token>> + Clone {
    select! { Token::String(s) => Atom::Str(s) }
}

fn ident() -> impl Parser<Token, Atom, Error = Simple<Token>> + Clone {
    let word = word();
    word.clone().then(word.repeated()).foldl(ident_normalize)
}

fn word() -> impl Parser<Token, Atom, Error = Simple<Token>> + Clone {
    select! { Token::Word(word) => Atom::Ident(word) }
}

fn ident_normalize(l: Atom, r: Atom) -> Atom {
    match (l, r) {
        (Atom::Ident(l), Atom::Ident(r)) => {
            Atom::Ident(format!("{} {}", l.to_lowercase(), r.to_lowercase()))
        }
        _ => unreachable!(),
    }
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

    #[test]
    fn ident_normalization() {
        let (l, r) = (Atom::Ident("This".into()), (Atom::Ident("tHaT".into())));
        assert_eq!(Atom::Ident("this that".into()), ident_normalize(l, r));
    }
}
