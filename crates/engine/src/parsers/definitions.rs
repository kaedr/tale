use crate::lexer::Token;
use chumsky::prelude::*;

use crate::{
    ast::{Atom, Expr, RcNode, Script, Statement, Table, TableGroup, TableRows, full_rc_node},
    state::SimpleParserState,
};

use super::{
    atoms::{dice, ident, ident_normalize},
    expressions::{arithmetic, number_range_list},
    statements::{any_statement, seq_or_statement},
};

pub fn script<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    just(Token::Script)
        .then(just(Token::Colon))
        .ignore_then(ident().map_with(full_rc_node))
        .then_ignore(just(Token::NewLines))
        .then(
            just(Token::Tabs)
                .or_not()
                .ignore_then(seq_or_statement())
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
        .labelled("Script Definition")
        .as_context()
}

pub fn table<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    just(Token::Table)
        .then(just(Token::Colon))
        .ignore_then(ident().map_with(full_rc_node))
        .then_ignore(just(Token::NewLines))
        .then(table_headings())
        .then(table_rows())
        .map_with(|((name, (roll, tags)), rows), extra| {
            let roll = if roll.inner_t().is_empty() {
                full_rc_node(rows.inner_t().calc_roll(), extra)
            } else {
                roll
            };
            let tags = tags;
            let table = full_rc_node(Table::new(name, roll, tags, rows), extra);
            full_rc_node(Statement::Table(table), extra)
        })
        .labelled("Table Definition")
        .as_context()
}

pub fn table_group<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Statement>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    just(Token::Table)
        .then(just(Token::Group))
        .then(just(Token::Colon))
        .ignore_then(ident().map_with(full_rc_node::<Atom, Atom>))
        .then_ignore(just(Token::NewLines))
        .then(
            tags_directive().or_not().map_with(|maybe_tags, extra| {
                maybe_tags.unwrap_or(full_rc_node(Vec::new(), extra))
            }),
        )
        .then(sub_tables_row())
        .then(table_group_rows())
        .then_ignore(
            just(Token::End)
                .then(just(Token::Table))
                .then(just(Token::Group).or_not()),
        )
        .then_ignore(just(Token::NewLines).ignored().or(end()))
        .try_map_with(|(((name, tags), (roll, sub_names)), sub_rows), extra| {
            if sub_names.len() != sub_rows.len() {
                return Err(Rich::custom(
                    extra.span(),
                    "Table Group rows must all have same number of columns",
                ));
            }
            let sub_tables = sub_names
                .into_iter()
                .zip(sub_rows.into_iter())
                .map(|(sub_name, rows)| {
                    let full_name =
                        full_rc_node(ident_normalize(name.inner_t().clone(), sub_name), extra);
                    let table_value = Table::new(full_name, roll.clone(), tags.clone(), rows);
                    full_rc_node(table_value, extra)
                })
                .collect();
            let value: RcNode<TableGroup> =
                full_rc_node(TableGroup::new(name, tags, sub_tables), extra);
            Ok(full_rc_node(value, extra))
        })
        .labelled("Table Group Definition")
        .as_context()
}

fn sub_tables_row<'src>() -> impl Parser<
    'src,
    &'src [Token],
    (RcNode<Expr>, Vec<Atom>),
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    dice()
        .map_with(full_rc_node)
        .then_ignore(just(Token::Tabs))
        .then(ident().separated_by(just(Token::Tabs)).collect::<Vec<_>>())
        .then_ignore(just(Token::NewLines))
}

fn table_group_rows<'src>() -> impl Parser<
    'src,
    &'src [Token],
    Vec<RcNode<TableRows>>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    row_key()
        .then(
            any_statement()
                .separated_by( one_of([Token::Period, Token::SemiColon]).or_not().then(just(Token::Tabs)))
                .collect::<Vec<_>>(),
        )
        .map(|(key, items)| {
            items
                .into_iter()
                .map(|item| (key.clone(), item))
                .collect::<Vec<_>>()
        })
        .then_ignore(one_of([Token::Period, Token::SemiColon]).or_not().then(just(Token::NewLines)))
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .try_map_with(|rows, extra| {
            let width = rows[0].len();
            for (idx, row) in rows.iter().enumerate() {
                if row.len() != width {
                    let err = Err(Rich::custom(
                        // TODO: Figure out why this span doesn't populate up to the final error.
                        SimpleSpan::new((), 9999..77777),
                        format!("Table Group rows must all have same number of columns, row {} has {} columns but expected {width}",
                            idx + 1, row.len()
                        ),
                    ));
                    return err;
                }
            }
            let iter_rows = rows
                .into_iter()
                .map(|row| row.into_iter())
                .collect::<Vec<_>>();
            let mut columns: Vec<Vec<_>> = Vec::new();
            for (rn, row) in iter_rows.into_iter().enumerate() {
                for (cn, item) in row.enumerate() {
                    if rn == 0 {
                        columns.push(vec![item]);
                    } else {
                        columns[cn].push(item);
                    }
                }
            }
            let columns = columns
                .into_iter()
                .map(|column| full_rc_node(TableRows::Keyed(column), extra))
                .collect();
            Ok(columns)
        })
}

fn table_rows<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<TableRows>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
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
            .then_ignore(just(Token::End).then(just(Token::Table))))
        .or(just(Token::End)
            .then(just(Token::Table))
            .map_with(|_, extra| full_rc_node(TableRows::Empty, extra)))
        .then_ignore(just(Token::NewLines).ignored().or(end()))
}

fn table_headings<'src>() -> impl Parser<
    'src,
    &'src [Token],
    (RcNode<Expr>, RcNode<Vec<Atom>>),
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    let roll_directive = just(Token::Roll)
        .then(just(Token::Colon))
        .ignore_then(arithmetic())
        .then_ignore(just(Token::NewLines))
        .map_with(|item, extra| (item, full_rc_node(Vec::new(), extra)));

    let tags_directive =
        tags_directive().map_with(|item, extra| (full_rc_node(Expr::Empty, extra), item));

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
        .boxed()
}

fn tags_directive<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Vec<Atom>>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    just(Token::Tag)
        .then(just(Token::Colon))
        .ignore_then(
            // Tags are parsed like Idents, but don't actually represent a value, so
            // we convert them to Str to make analysis/evaluation cleaner
            ident()
                .map(|id| Atom::Str(id.to_lowercase()))
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .map_with(full_rc_node),
        )
        .then_ignore(just(Token::NewLines))
}

fn row_key<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleParserState<'src>, ()>,
> + Clone {
    number_range_list()
        .then_ignore(just(Token::Tabs))
        .or(ident()
            .map_with(full_rc_node)
            .then_ignore(just(Token::Tabs)))
}

#[cfg(test)]
#[allow(unused_must_use)]
mod tests {
    use crate::{
        state::ParserState,
        tests::{grubbed_parser, stubbed_parser},
    };

    use super::*;

    #[test]
    fn parse_script() {

        let source = "Script: Example
                            Set Phasers to stun
                            End Script";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, script());
        assert_eq!("Script: `example`, 1 Statement", format!("{output}"));
    }

    #[test]
    fn parse_table() {

        let source = "Table: Colors
                            List: Red, Orange, Yellow, Green, Blue, Purple
                            ";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, table());
        assert_eq!("Table: `colors`, 1d6, 6 Rows", format!("{output}"));

        let source = "Table: Stub
                            Roll: d20
                            End Table
                            ";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, table());
        assert_eq!("Table: `stub`, 1d20, 0 Rows", format!("{output}"));

        let source = "Table: Basic
                            Pork
                            Beef
                            Chicken
                            End Table
                            ";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, table());
        assert_eq!("Table: `basic`, 1d3, 3 Rows", format!("{output}"));

        let source = "Table: keyed
                            1\tis the loneliest number
                            2\tCan be as bad as one
                            3-5,7\tProbably pretty garbage too...
                            6\tlastly
                            End Table";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, table());
        assert_eq!("Table: `keyed`, 1d7, 4 Rows", format!("{output}"));
    }

    #[test]
    fn parse_table_group() {


        let source = "Table Group: minimal
                            1d3\texample
                            1\ta
                            2\tb
                            3\tc
                            End Table Group\n";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, table_group());
        assert_eq!(
            "TableGroup: `minimal`\n\
            \t`minimal example`, 1d3, 3 Rows\n",
            output
        );

        let source = "Table Group: Animals
                            Tags: animals
                            1d3\tHouse\tBarn\tForest
                            1\tCat\tCow\tSquirrel
                            2\tDog\tHorse\tRabbit
                            3\tMouse\tPig\tDeer
                            End Table Group\n";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, table_group());
        assert_eq!(
            "TableGroup: `animals`\n\
            \t`animals house`, 1d3, 3 Rows\n\
            \t`animals barn`, 1d3, 3 Rows\n\
            \t`animals forest`, 1d3, 3 Rows\n",
            output
        );

        let source = "Table Group: Broken
                            1d1\ttwo\theadings
                            1\tThree\tRow\tItems
                            End Table Group\n";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, table_group());
        assert_eq!(
            "[Table Group rows must all have same number of columns at 0..23 in Table Group Definition at 0..23]",
            output
        );
    }

    #[test]
    fn parse_sub_tables_row() {


        let source = "1d6\tColor\tShape\tSize\n";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = grubbed_parser(&mut p_state, &tokens, sub_tables_row());
        // Assert we parsed a single die roll and 3 columns.
        assert_eq!(1, output.matches("Dice(1, 6)").count());
        assert_eq!(3, output.matches("Ident(").count());
    }

    #[test]
    fn parse_table_group_rows() {


        let source = "1\ta
                            2\tb
                            3\tc
                            ";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = grubbed_parser(&mut p_state, &tokens, table_group_rows());
        println!("{}", output);
        // Assert we parsed 1 column and 3 rows.
        assert_eq!(1, output.matches("Keyed(").count());
        assert_eq!(3, output.matches("List(").count());
        assert_eq!(3, output.matches("Atom(Ident").count());

        let source = "1\tCat\tCow\tSquirrel
                            2\tDog\tHorse\tRabbit
                            3\tMouse\tPig\tDeer
                            ";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = grubbed_parser(&mut p_state, &tokens, table_group_rows());
        // Assert we parsed 1 column and 3 rows.
        assert_eq!(3, output.matches("Keyed(").count());
        assert_eq!(9, output.matches("List(").count());
        assert_eq!(9, output.matches("Atom(Ident").count());

        let source = "1\ta\tz
                            2\tb
                            3\tc
                            ";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = grubbed_parser(&mut p_state, &tokens, table_group_rows());
        // Check the uneven rows case:
        assert_eq!(
            "[Table Group rows must all have same number of columns, row 2 has 1 columns but expected 2 at 14..14]",
            output
        );
    }

    #[test]
    fn parse_table_headings() {

        let source = "";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = grubbed_parser(&mut p_state, &tokens, table_headings());
        assert!(output.starts_with("(Node"));
        assert!(output.contains("value: Empty"));
        assert!(output.contains("value: []"));
        assert!(output.ends_with("} })"));

        let source = "Roll: 1d8\n";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = grubbed_parser(&mut p_state, &tokens, table_headings());
        assert!(output.starts_with("(Node"));
        assert!(output.contains("Atom(Dice(1, 8))"));
        assert!(output.contains("value: []"));
        assert!(output.ends_with("} })"));

        let source = "Tags: Dark, Stormy\n";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = grubbed_parser(&mut p_state, &tokens, table_headings());
        println!("{}", output);
        assert!(output.starts_with("(Node"));
        assert!(output.contains("value: Empty"));
        assert!(output.contains("[Str(\"dark\")"));
        assert!(output.contains("Str(\"stormy\")]"));
        assert!(output.ends_with("} })"));

        let source = "Roll: 1d6\nTags: This, That, The Other Thing\n";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = grubbed_parser(&mut p_state, &tokens, table_headings());
        println!("{}", output);
        assert!(output.starts_with("(Node"));
        assert!(output.contains("Atom(Dice(1, 6))"));
        assert!(output.contains("[Str(\"this\")"));
        assert!(output.contains("Str(\"that\")"));
        assert!(output.contains("Str(\"the other thing\")]"));
        assert!(output.ends_with("} })"));

        let source = "Tags: the other, way around\nRoll: 2d20\n";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = grubbed_parser(&mut p_state, &tokens, table_headings());
        println!("{}", output);
        assert!(output.starts_with("(Node"));
        assert!(output.contains("Atom(Dice(2, 20)"));
        assert!(output.contains("[Str(\"the other\")"));
        assert!(output.contains("Str(\"way around\")]"));
        assert!(output.ends_with("} })"));

        let source = "Tag: @";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = grubbed_parser(&mut p_state, &tokens, table_headings());
        assert_eq!(
            "[found 'At' at 2..3 expected Identity, or 'NewLines']",
            output
        );

        let source = "roll: @";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = grubbed_parser(&mut p_state, &tokens, table_headings());
        assert_eq!(
            "[found 'At' at 2..3 expected Arithmetic Expression]",
            output
        );
    }

    #[test]
    fn parse_row_key() {

        let source = "22\t";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, row_key());
        assert_eq!("[22]", format!("{output}"));

        let source = "4,6-8\t";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, row_key());
        assert_eq!("[4, 6, 7, 8]", format!("{output}"));

        let source = "Elves\t";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, row_key());
        assert_eq!("`elves`", format!("{output}"));

        let source = "`Dwarves`\t";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, row_key());
        assert_eq!("`dwarves`", format!("{output}"));

        let source = "4 Non Blondes\t";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, row_key());
        assert_eq!("`4 non blondes`", format!("{output}"));

        let source = "3-4 Business Days\t";
        let mut p_state = ParserState::from_source(source.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, row_key());
        assert_eq!(
            "[found 'Word(\"Business\")' at 3..4 expected 'Comma', or 'Tabs']",
            format!("{output}")
        );
    }
}
