use chumsky::prelude::*;
use lexer::Token;

use crate::{
    SimpleStateTable,
    ast::{Atom, Expr, RcNode, Script, Statement, Table, TableRows, full_rc_node},
};

use super::{
    atoms::ident,
    expressions::{arithmetic, number_range_list},
    statements::{any_statement, seq_or_statement},
};

pub fn script<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    just(Token::Script)
        .then(just(Token::Colon))
        .ignore_then(ident().map_with(full_rc_node))
        .then_ignore(just(Token::NewLines))
        .then(
            seq_or_statement()
                .then_ignore(just(Token::NewLines))
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::End).then(just(Token::Script)))
        .then_ignore(just(Token::NewLines).ignored().or(end()))
        .map_with(|(name, statements), extra| {
            let value = Script::new(name, statements);
            Statement::Script(full_rc_node(value, extra))
        })
        .map_with(full_rc_node)
}

pub fn table<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    just(Token::Table)
        .then(just(Token::Colon))
        .ignore_then(ident().map_with(full_rc_node))
        .then_ignore(just(Token::NewLines))
        .then(table_headings())
        .then(table_rows())
        .map_with(|((name, (roll, tags)), rows), extra| {
            let roll = match roll.inner() {
                Expr::Empty => full_rc_node(rows.inner().calc_roll(), extra),
                _ => roll,
            };
            let tags = tags;
            let table = full_rc_node(Table::new(name, roll, tags, rows), extra);
            full_rc_node(Statement::Table(table), extra)
        })
}

fn table_rows<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<TableRows>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    let list_form = just(Token::List)
        .then(just(Token::Colon))
        .ignore_then(
            ident()
                .map_with(|_, extra| {
                    let span = extra.span().into_range();
                    Atom::Str(extra.state().get_source_slice(&span).to_string())
                })
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>(),
        )
        .map_with(full_rc_node);

    let flat_form = any_statement()
        .then_ignore(just(Token::NewLines))
        .repeated()
        .at_least(1)
        .collect()
        .map_with(|rows, extra| full_rc_node(TableRows::Flat(rows), extra));

    let keyed_form = row_key()
        .then(any_statement())
        .then_ignore(just(Token::NewLines))
        .repeated()
        .at_least(1)
        .collect()
        .map_with(|rows, extra| full_rc_node(TableRows::Keyed(rows), extra));

    list_form
        .or(flat_form
            .or(keyed_form)
            .or(empty().map_with(|_, extra| full_rc_node(TableRows::Empty, extra)))
            .then_ignore(just(Token::End).then(just(Token::Table))))
        .then_ignore(just(Token::NewLines).ignored().or(end()))
}

pub fn table_group<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    // just(Token::Table)
    //     .then(just(Token::Group))
    //     .then(just(Token::Colon))
    //     .ignore_then(ident().map_with(full_rc_node))
    //     .then_ignore(just(Token::NewLines))
    //     .then(table_headings());
    todo()
}

fn table_headings<'src>() -> impl Parser<
    'src,
    &'src [Token],
    (RcNode<Expr>, RcNode<Vec<Atom>>),
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    let roll_directive = just(Token::Roll)
        .then(just(Token::Colon))
        .ignore_then(arithmetic())
        .then_ignore(just(Token::NewLines))
        .map_with(|item, extra| (item, full_rc_node(Vec::new(), extra)));

    let tags_directive = just(Token::Tag)
        .then(just(Token::Colon))
        .ignore_then(
            ident()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .map_with(full_rc_node),
        )
        .then_ignore(just(Token::NewLines))
        .map_with(|item, extra| (full_rc_node(Expr::Empty, extra), item));

    // For the sake of the next person to look at this, let's review what's going on here:
    // A Roll directive
    roll_directive
        .clone()
        // Possibly followed by a tags directive
        .then(tags_directive.clone().or_not())
        .map(|(rd, td)| {
            match (rd, td) {
                // If we have both, we take filled side of each
                ((rdl, _), Some((_, tdr))) => (rdl, tdr),
                // If just the roll, take its whole tuple
                ((rdl, rdr), None) => (rdl, rdr),
            }
        })
        // Or a tags directive
        .or(tags_directive
            // Possibly followed by roll directive
            .then(roll_directive.or_not())
            .map(|(td, rd)| match (rd, td) {
                // If we have both, we take filled side of each
                (Some((rdl, _)), (_, tdr)) => (rdl, tdr),
                // If just the tags, take its whole tuple
                (None, (tdl, tdr)) => (tdl, tdr),
            }))
        // Or neither of those things
        .or_not()
        .map_with(|heading, extra| {
            heading.unwrap_or((
                full_rc_node(Expr::Empty, extra),
                full_rc_node(Vec::new(), extra),
            ))
        })
}

fn row_key<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    number_range_list()
        .then_ignore(just(Token::Tabs))
        .or(ident()
            .map_with(full_rc_node)
            .then_ignore(just(Token::Tabs)))
}

#[cfg(test)]
mod tests {
    use crate::{
        StateTable,
        tests::{grubbed_parser, stubbed_parser},
    };

    use super::*;

    #[test]
    fn parse_script() {
        let mut table = StateTable::new();
        let source = "Script: Example
                            Set Phasers to stun
                            End Script";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, script());
        assert_eq!("Script(Atom(example), 1 Statements)", format!("{}", output));
    }

    #[test]
    fn parse_table() {
        let mut state = StateTable::new();
        let source = "Table: Colors
                            List: Red, Orange, Yellow, Green, Blue, Purple";
        state.add_source("test".into(), source.into());
        state.lex_current();
        let tokens = &state.get_tokens("test");
        let output = stubbed_parser(&mut state, &tokens, table());
        assert_eq!(
            "Table(Atom(colors), (Atom(1d6)) 6 Rows)",
            format!("{}", output)
        );

        let source = "Table: Stub
                            Roll: d20
                            End Table";
        state.add_source("test".into(), source.into());
        state.lex_current();
        let tokens = &state.get_tokens("test");
        let output = stubbed_parser(&mut state, &tokens, table());
        assert_eq!(
            "Table(Atom(stub), (Atom(1d20)) 0 Rows)",
            format!("{}", output)
        );

        let source = "Table: Basic
                            Pork
                            Beef
                            Chicken
                            End Table";
        state.add_source("test".into(), source.into());
        state.lex_current();
        let tokens = &state.get_tokens("test");
        let output = stubbed_parser(&mut state, &tokens, table());
        assert_eq!(
            "Table(Atom(basic), (Atom(1d3)) 3 Rows)",
            format!("{}", output)
        );

        let source = "Table: keyed
                            1\tis the loneliest number
                            2\tCan be as bad as one
                            3-5,7\tProbably pretty garbage too...
                            6\tlastly
                            End Table";
        state.add_source("test".into(), source.into());
        state.lex_current();
        let tokens = &state.get_tokens("test");
        let output = stubbed_parser(&mut state, &tokens, table());
        assert_eq!(
            "Table(Atom(keyed), (Atom(1d7)) 4 Rows)",
            format!("{}", output)
        );
    }

    #[test]
    fn parse_table_headings() {
        let mut table = StateTable::new();
        let source = "";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = grubbed_parser(&mut table, &tokens, table_headings());
        assert!(output.starts_with("(Node"));
        assert!(output.contains("actual: Empty"));
        assert!(output.contains("actual: []"));
        assert!(output.ends_with("} })"));

        let source = "Roll: 1d8\n";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = grubbed_parser(&mut table, &tokens, table_headings());
        assert!(output.starts_with("(Node"));
        assert!(output.contains("Atom(Dice(1, 8))"));
        assert!(output.contains("actual: []"));
        assert!(output.ends_with("} })"));

        let source = "Tags: Dark, Stormy\n";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = grubbed_parser(&mut table, &tokens, table_headings());
        //println!("{:?}", output);
        assert!(output.starts_with("(Node"));
        assert!(output.contains("actual: Empty"));
        assert!(output.contains("[Ident(\"dark\")"));
        assert!(output.contains("Ident(\"stormy\")]"));
        assert!(output.ends_with("} })"));

        let source = "Roll: 1d6\nTags: This, That, The Other Thing\n";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = grubbed_parser(&mut table, &tokens, table_headings());
        //println!("{:?}", output);
        assert!(output.starts_with("(Node"));
        assert!(output.contains("Atom(Dice(1, 6))"));
        assert!(output.contains("[Ident(\"this\")"));
        assert!(output.contains("Ident(\"that\")"));
        assert!(output.contains("Ident(\"the other thing\")]"));
        assert!(output.ends_with("} })"));

        let source = "Tags: the other, way around\nRoll: 2d20\n";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = grubbed_parser(&mut table, &tokens, table_headings());
        println!("{:?}", output);
        assert!(output.starts_with("(Node"));
        assert!(output.contains("Atom(Dice(2, 20)"));
        assert!(output.contains("[Ident(\"the other\")"));
        assert!(output.contains("Ident(\"way around\")]"));
        assert!(output.ends_with("} })"));

        let source = "Tag: @";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = grubbed_parser(&mut table, &tokens, table_headings());
        assert_eq!(
            "[found 'At' at 2..3 expected something else, or 'NewLines']",
            output
        );

        let source = "roll: @";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = grubbed_parser(&mut table, &tokens, table_headings());
        assert_eq!(
            "[found 'At' at 2..3 expected 'Minus', something else, or 'LParens']",
            output
        );
    }

    #[test]
    fn parse_row_key() {
        let mut table = StateTable::new();
        let source = "22\t";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, row_key());
        assert_eq!("[Atom(22)]", format!("{}", output));

        let source = "4,6-8\t";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, row_key());
        assert_eq!(
            "[Atom(4), Atom(6), Atom(7), Atom(8)]",
            format!("{}", output)
        );

        let source = "Elves\t";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, row_key());
        assert_eq!("Atom(elves)", format!("{}", output));

        let source = "`Dwarves`\t";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, row_key());
        assert_eq!("Atom(dwarves)", format!("{}", output));

        let source = "4 Non Blondes\t";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, row_key());
        assert_eq!("Atom(4 non blondes)", format!("{}", output));

        let source = "3-4 Business Days\t";
        table.add_source("test".into(), source.into());
        table.lex_current();
        let tokens = &table.get_tokens("test");
        let output = stubbed_parser(&mut table, &tokens, row_key());
        assert_eq!(
            "[found 'Word(\"Business\")' at 3..4 expected 'Comma', or 'Tabs']",
            format!("{}", output)
        );
    }
}
