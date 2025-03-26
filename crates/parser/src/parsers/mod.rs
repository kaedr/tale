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
    fn it_works() {}
}
