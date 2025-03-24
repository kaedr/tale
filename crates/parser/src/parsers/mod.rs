use chumsky::{error::Simple, prelude::*};
use lexer::Token;

use crate::{
    SimpleStateTable,
    ast::{RcNode, Statement, full_rc_node},
};

mod atoms;
mod definitions;
mod expressions;
mod statements;

pub fn parser<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Simple<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    atoms::words().map_with(full_rc_node)
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

#[cfg(test)]
mod tests {
    use chumsky::extra::SimpleState;

    use crate::{StateTable, tests::stubbed_parser};

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
        let output = stubbed_parser(&mut table, &tokens, expressions::arithmetic());
        assert_eq!("Add(Atom(100) + Mul(Atom(1) * Atom(2)))", output);
    }

    #[test]
    fn test_words() {
        let mut table = StateTable::new();
        table.add_source("test".into(), r"this is a test: once upon a time...".into());
        table.lex_source("test".into());
        table.parse_source("test".into());
        let output = table.asts.get("test").unwrap();
        assert_eq!(
            "Expr(Atom(this is a test: once upon a time...))",
            format!("{}", output)
        );
    }
}
