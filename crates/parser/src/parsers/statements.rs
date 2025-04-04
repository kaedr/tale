use chumsky::prelude::*;
use lexer::Token;

use crate::{
    SimpleStateTable,
    ast::{Duration, Expr, Modifier, RcNode, Statement, full_rc_node},
};

use super::{
    atoms::{ident, ident_maybe_sub, number, qstring, terminator, value_name, words},
    expressions::{any_expr, arithmetic, implied_roll_expr, interpolation},
};

pub fn seq_or_statement<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    statement_sequence().or(any_statement()).boxed()
}

pub fn any_statement<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    nonce()
        .or(chainable_statement())
        .or(load())
        .or(output())
        .or(show()).boxed()
}

fn nonce<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    one_of([Token::Dash, Token::Minus])
        .then(terminator())
        .ignored()
        .map_with(|_, extra| full_rc_node(Statement::Empty, extra))
}

pub fn chainable_statement<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    assignment()
        .or(clear())
        .or(invoke())
        .or(modify())
        .or(expression()).boxed()
}

pub fn statement_sequence<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    let chained = chainable_statement()
        .separated_by(just(Token::Comma).then(just(Token::And).or_not()).ignored())
        .at_least(1)
        .collect::<Vec<_>>();

    let implied_roll_expr = implied_roll_expr()
        .then_ignore(terminator())
        .map_with(|implied_roll, extra| vec![full_rc_node(implied_roll, extra)]);

    chained
        .or(any_statement().map(|stmt| vec![stmt]))
        .or(implied_roll_expr)
        .delimited_by(just(Token::LBracket), just(Token::RBracket))
        .map_with(full_rc_node)
        .map_with(|items, extra| full_rc_node(Statement::Sequence(items), extra)).boxed()
}

pub fn assignment<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    just(Token::Set)
        .or_not()
        .ignore_then(value_name().map_with(full_rc_node))
        .then_ignore(just(Token::Equals).or(just(Token::To)))
        .then(any_expr())
        .map_with(|(lhs, rhs), extra| full_rc_node(Statement::Assignment(lhs, rhs), extra)).boxed()
}

pub fn expression<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    any_expr().map_with(|expr, extra| full_rc_node(Statement::Expr(expr), extra)).boxed()
}

pub fn clear<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    just(Token::Clear)
        .ignore_then(duration().map_with(full_rc_node))
        .then(
            ident_maybe_sub()
                .then_ignore(just(Token::Roll).or_not())
                .then_ignore(terminator())
                .map_with(full_rc_node),
        )
        .map_with(|(lhs, rhs), extra| full_rc_node(Statement::Clear(lhs, rhs), extra)).boxed()
}

pub fn invoke<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    just(Token::Invoke)
        .then(just(Token::Colon).or_not())
        .ignore_then(ident().then_ignore(terminator()))
        .map_with(full_rc_node)
        .map_with(|node, extra| full_rc_node(Statement::Invoke(node), extra)).boxed()
}

pub fn load<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    just(Token::Load)
        .then(just(Token::Colon).or_not())
        .ignore_then(qstring().map_with(full_rc_node).or(words()))
        .then_ignore(
            one_of([Token::Tabs, Token::NewLines, Token::RBracket])
                .ignored()
                .rewind()
                .or(end()),
        )
        .map_with(|path, extra| full_rc_node(Statement::Load(path), extra)).boxed()
}

pub fn modify<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    let keyword_form = just(Token::Modify)
        .ignore_then(duration())
        .then(ident_maybe_sub())
        .then_ignore(just(Token::Roll))
        .then(mod_by())
        .then_ignore(terminator())
        .map_with(|((dur, id), value), extra| {
            let modifier = Modifier::new(dur, value);
            full_rc_node(
                Statement::Modify(full_rc_node(modifier, extra), full_rc_node(id, extra)),
                extra,
            )
        });
    let leading_form = mod_by()
        .then_ignore(just(Token::To))
        .then(duration())
        .then(ident_maybe_sub())
        .then_ignore(just(Token::Roll))
        .then_ignore(terminator())
        .map_with(|((value, dur), id), extra| {
            let modifier = Modifier::new(dur, value);
            full_rc_node(
                Statement::Modify(full_rc_node(modifier, extra), full_rc_node(id, extra)),
                extra,
            )
        });
    keyword_form.or(leading_form).boxed()
}

fn duration<'src>()
-> impl Parser<'src, &'src [Token], Duration, extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    just(Token::All).map(|_| Duration::All).or(just(Token::Next)
        .ignore_then(arithmetic())
        .map(|val| Duration::Next(val))).boxed()
}

pub fn mod_by<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    one_of([Token::Plus, Token::Minus])
        .then(number().map_with(full_rc_node))
        .map_with(|(sign, value), extra| match sign {
            Token::Plus => value,
            Token::Minus => full_rc_node(Expr::Neg(value), extra),
            _ => unreachable!(),
        }).boxed()
}

pub fn output<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    just(Token::Output)
        .ignore_then(just(Token::Colon).or_not())
        .ignore_then(interpolation())
        .map_with(|value, extra| full_rc_node(Statement::Output(value), extra)).boxed()
}

pub fn show<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    just(Token::Show)
        .ignore_then(just(Token::Tag).or_not())
        .then(ident_maybe_sub())
        .map_with(|(tags, value), extra| {
            full_rc_node(
                Statement::Show(full_rc_node((tags.is_some(), value), extra)),
                extra,
            )
        }).boxed()
}

#[cfg(test)]
mod tests {
    use lexer::{quick_tokens, utils::read_sample_lines};

    use crate::{StateTable, tests::stubbed_parser};

    use super::*;

    #[test]
    fn parse_nonce() {
        let mut table = StateTable::new();

        table.add_source("test".into(), r"-".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, any_statement());
        assert_eq!("Empty", format!("{}", output));

        table.add_source("test".into(), r"–".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, any_statement());
        assert_eq!("Empty", format!("{}", output));

        table.add_source("test".into(), r"—".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, any_statement());
        assert_eq!("Empty", format!("{}", output));

        table.add_source("test".into(), "".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, any_statement());
        assert_eq!(
            "[found end of input at 0..0 expected 'Dash', 'Minus', 'Set', something else\
            , 'Clear', 'Invoke', 'Modify', 'Plus', 'Roll', 'LParens', 'Lookup', 'LBracket'\
            , 'Load', 'Output', or 'Show']",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_sequence() {
        let mut table = StateTable::new();

        table.add_source("test".into(), r"[Easy Peasy]".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, statement_sequence());
        assert_eq!(
            "Sequence(Expr(Roll(Atom(easy), Atom(peasy))))",
            format!("{}", output)
        );

        table.add_source("test".into(), r"[1d6 + 7]".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, statement_sequence());
        assert_eq!(
            "Sequence(Expr(Add(Atom(1d6) + Atom(7))))",
            format!("{}", output)
        );

        table.add_source("test".into(), r"[Invoke taco, Invoke burrito]".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, statement_sequence());
        assert_eq!(
            "Sequence(Invoke(Atom(taco)), Invoke(Atom(burrito)))",
            format!("{}", output)
        );

        table.add_source("test".into(), r"A + B".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, statement_sequence());
        assert_eq!(
            "[found 'Word(\"A\")' at 0..1 expected 'LBracket']",
            format!("{}", output)
        );

        table.add_source("test".into(), "".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, statement_sequence());
        assert_eq!(
            "[found end of input at 0..0 expected 'LBracket']",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_assignment() {
        let mut table = StateTable::new();
        let check_vals = vec![
            "Assignment(Atom(the_word) = Atom(bird))",
            r#"Assignment(Atom(the_word) = Interpol(Atom("bird")))"#,
            "Assignment(Atom(force) = Mul(Atom(mass) * Atom(acceleration)))",
            "Assignment(Atom(minutes) = Atom(midnight))",
        ];

        let lines = read_sample_lines("11_statement_assignment.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let tokens = quick_tokens(line.unwrap().as_str());
            let output = stubbed_parser(&mut table, &tokens, assignment());
            assert_eq!(check_vals[index], format!("{}", output));
        }

        table.add_source("test".into(), "".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, assignment());
        assert_eq!(
            "[found end of input at 0..0 expected 'Set', or something else]",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_clear() {
        let mut table = StateTable::new();
        let check_vals = vec![
            "Clear(Next(Atom(10)) Atom(quality))",
            "Clear(All Atom(quality))",
            "Clear(Next(Atom(10)) Atom(quality))",
            "Clear(All Atom(quality))",
        ];

        let lines = read_sample_lines("12_statement_clear.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let tokens = quick_tokens(line.unwrap().as_str());
            let output = stubbed_parser(&mut table, &tokens, clear());
            assert_eq!(check_vals[index], format!("{}", output));
        }

        table.add_source("test".into(), "".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, clear());
        assert_eq!(
            "[found end of input at 0..0 expected 'Clear']",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_invoke() {
        let mut table = StateTable::new();
        let check_vals = vec![
            "Invoke(Atom(some kind of bizarre ritual))",
            "Invoke(Atom(last rites))",
        ];

        let lines = read_sample_lines("13_statement_invoke.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let tokens = quick_tokens(line.unwrap().as_str());
            let output = stubbed_parser(&mut table, &tokens, invoke());
            assert_eq!(check_vals[index], format!("{}", output));
        }

        table.add_source("test".into(), "".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, invoke());
        assert_eq!(
            "[found end of input at 0..0 expected 'Invoke']",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_load() {
        let mut table = StateTable::new();
        let check_vals = vec![
            r#"Load(Atom("01_table_minimal.tale"))"#,
            r#"Load(Atom("../../tons of _odd-characters_.tale"))"#,
            r#"Load(Atom("../../tons of _odd-characters_.tale"))"#,
        ];

        let lines = read_sample_lines("14_statement_load.tale").unwrap();
        for (index, line) in lines.enumerate() {
            table.add_source("test".into(), line.unwrap());
            table.lex_current();
            let tokens = &table.get_tokens("test");
            let output = stubbed_parser(&mut table, &tokens, load());
            assert_eq!(check_vals[index], format!("{}", output));
        }

        table.add_source("test".into(), "".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, load());
        assert_eq!(
            "[found end of input at 0..0 expected 'Load']",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_modify() {
        let mut table = StateTable::new();
        let check_vals = vec![
            "Modify(All Atom(2) Atom(quality))",
            "Modify(Next(Atom(7)) Atom(3) Atom(quality))",
            "Modify(Next(Atom(3)) Neg(Atom(2)) Atom(quality))",
            "Modify(All Atom(10) Atom(quality))",
        ];

        let lines = read_sample_lines("16_statement_modify.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let tokens = quick_tokens(line.unwrap().as_str());
            let output = stubbed_parser(&mut table, &tokens, modify());
            assert_eq!(check_vals[index], format!("{}", output));
        }

        table.add_source("test".into(), "".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, modify());
        assert_eq!(
            "[found end of input at 0..0 expected 'Modify', 'Plus', or 'Minus']",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_output() {
        let mut table = StateTable::new();
        let check_vals = vec![
            r#"Output(Interpol(Atom("There are"), Sub(Atom(1d6) - Atom(1)), Atom("lights illuminated out of a total of 5.")))"#,
            r#"Output(Interpol(Atom("A lovely string")))"#,
        ];

        let lines = read_sample_lines("17_statement_output.tale").unwrap();
        for (index, line) in lines.enumerate() {
            table.add_source("test".into(), line.unwrap());
            table.lex_current();
            let tokens = &table.get_tokens("test");
            let output = stubbed_parser(&mut table, &tokens, output());
            assert_eq!(check_vals[index], format!("{}", output));
        }

        table.add_source("test".into(), "".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, output());
        assert_eq!(
            "[found end of input at 0..0 expected 'Output']",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_show() {
        let mut table = StateTable::new();
        let check_vals = vec![
            "Show(Atom(minimalism))",
            "Show(Atom(the_word))",
            "Show(Atom(variables))",
            "Show(Atom(tables))",
            "ShowTag(Atom(desert))",
        ];

        let lines = read_sample_lines("19_statement_show.tale").unwrap();
        for (index, line) in lines.enumerate() {
            let tokens = quick_tokens(line.unwrap().as_str());
            let output = stubbed_parser(&mut table, &tokens, show());
            assert_eq!(check_vals[index], format!("{}", output));
        }

        table.add_source("test".into(), "".into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, show());
        assert_eq!(
            "[found end of input at 0..0 expected 'Show']",
            format!("{}", output)
        );
    }
}
