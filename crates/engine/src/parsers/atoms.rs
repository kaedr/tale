use crate::lexer::Token;
use crate::{
    state::SimpleStateTable,
    ast::{Atom, Expr, RcNode, full_rc_node},
};
use chumsky::prelude::*;

pub fn term<'src>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<Expr>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone {
    let (number, dice, value_name) = (number(), dice(), value_name());
    number
        .or(dice)
        .or(value_name)
        .map_with(full_rc_node)
        .boxed()
        .labelled("Arithmetic Term")
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

pub fn op<'src>(
    token: Token,
) -> impl Parser<'src, &'src [Token], Op, extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    just(token).map(Op::from).labelled("Arithmetic Operator")
}

pub fn qstring<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    let qstring = select! { Token::String(s) => Atom::Str(s) };
    qstring.labelled("Quoted String")
}

pub fn words<'src, T>() -> impl Parser<
    'src,
    &'src [Token],
    RcNode<T>,
    extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>,
> + Clone
where
    T: From<Atom> + 'src,
{
    wordlike()
        .or(typical_punctuation())
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map_with(|_, extra| {
            let span = extra.span().into_range();
            Atom::Str(extra.state().get_source_slice(&span).to_string())
        })
        .map_with(full_rc_node)
        .boxed()
        .labelled("Words")
}

pub fn ident_maybe_sub<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    ident()
        .then_ignore(just(Token::Colon).or_not())
        .then(ident().or_not())
        .map(|(l, r)| {
            if let Some(r) = r {
                ident_normalize(l, r)
            } else {
                l
            }
        })
        .labelled("Identity")
}

pub fn ident<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    wordlike()
        .foldl(wordlike().repeated(), ident_normalize)
        .or(qstring())
        .map(|id| Atom::Ident(id.to_lowercase()))
        .boxed()
        .labelled("Identity")
}

pub fn wordlike<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    // Raw keywords coming before numbers is important
    // As numbers numeric keyswords that stand alone are parsed as their value, not the word
    word()
        .or(raw_keywords())
        .or(number())
        .or(dice())
        .boxed()
        .labelled("Wordlike")
}

pub fn raw_keywords<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    let raw_keywords = select! {
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
        // TODO: Figure out how to allow roll in some places without breaking a bunch of crap
        // Token::Roll => Atom::Raw(Token::Roll),
        Token::Set => Atom::Raw(Token::Set),
        Token::Show => Atom::Raw(Token::Show),
        Token::Table => Atom::Raw(Token::Table),
        Token::Tag => Atom::Raw(Token::Tag),
        Token::Time => Atom::Raw(Token::Time),
        Token::To => Atom::Raw(Token::To),
    };
    raw_keywords.labelled("Keyword")
}

pub fn typical_punctuation<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    let typical_punctuation = select! {
        Token::Ampersand => Atom::Raw(Token::Ampersand),
        Token::Apostrophe => Atom::Raw(Token::Apostrophe),
        Token::Bang => Atom::Raw(Token::Bang),
        Token::Colon => Atom::Raw(Token::Colon),
        Token::Comma => Atom::Raw(Token::Comma),
        Token::Dash => Atom::Raw(Token::Dash),
        Token::Ellipsis => Atom::Raw(Token::Ellipsis),
        Token::LParens => Atom::Raw(Token::LParens),
        Token::Minus => Atom::Raw(Token::Minus),
        Token::Period => Atom::Raw(Token::Period),
        Token::Question => Atom::Raw(Token::Question),
        Token::RParens => Atom::Raw(Token::RParens),
        Token::SemiColon => Atom::Raw(Token::SemiColon),
        Token::Slash => Atom::Raw(Token::Slash),
    };
    typical_punctuation.labelled("Typical Punctuation")
}

pub fn value_name<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    let value_name = select! { Token::Word(word) => Atom::Ident(word.to_lowercase()) };
    value_name.labelled("Value Name")
}

pub fn word<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    let word = select! { Token::Word(word) => Atom::Str(word) };
    word.labelled("Word")
}

pub fn dice<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    let dice = select! { Token::DieRoll((x, y)) => Atom::Dice(x, y) };
    dice.labelled("Dice")
}

pub fn number<'src>()
-> impl Parser<'src, &'src [Token], Atom, extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    let number = select! {
        Token::DoubleOught => Atom::Number(100),
        Token::Digits(number) => Atom::Number(number),

        Token::One => Atom::Number(1),
        Token::Two => Atom::Number(2),
        Token::Three => Atom::Number(3),
        Token::Four => Atom::Number(4),
        Token::Five => Atom::Number(5),
        Token::Six => Atom::Number(6),
        Token::Seven => Atom::Number(7),
        Token::Eight => Atom::Number(8),
        Token::Nine => Atom::Number(9),
        Token::Ten => Atom::Number(10),
    };
    number.labelled("Numeric")
}

// Match a character designating termination of the current statement
// then, rewind so the character is still available for use a delimiter
pub fn terminator<'src>()
-> impl Parser<'src, &'src [Token], (), extra::Full<Rich<'src, Token>, SimpleStateTable<'src>, ()>>
+ Clone {
    one_of([
            Token::Comma,
            Token::Period,
            Token::NewLines,
            Token::RBracket,
            Token::SemiColon,
            Token::Tabs,
        ]).ignored()
        .or(end()).rewind()
        .labelled("Terminator")
}

pub fn ident_normalize(l: Atom, r: Atom) -> Atom {
    Atom::Ident(format!("{} {}", l.to_lowercase(), r.to_lowercase()))
}

#[cfg(test)]
#[allow(unused_must_use)]
mod tests {
    use crate::lexer::tests::quick_tokens;

    use crate::{state::StateTable, tests::stubbed_parser};

    use super::*;

    #[test]
    fn test_words() {
        let mut table = StateTable::new();
        table.add_source("test".into(), r"This is a test: Once upon a time...".into());
        table.lex_current();
        let tokens = &table.get_tokens("test").unwrap();
        let output = stubbed_parser(&mut table, &tokens, words::<Atom>());
        assert_eq!(
            r#""This is a test: Once upon a time...""#,
            format!("{output}")
        );

        table.add_source("test2".into(), r"Let's do this!".into());
        table.lex_current();
        let tokens = &table.get_tokens("test2").unwrap();
        let output = stubbed_parser(&mut table, &tokens, words::<Atom>());
        assert_eq!(r#""Let's do this!""#, format!("{output}"));

        table.add_source(
            "test3".into(),
            r"This is a (test): // Once upon a time...".into(),
        );
        table.lex_current();
        let tokens = &table.get_tokens("test3").unwrap();
        let output = stubbed_parser(&mut table, &tokens, words::<Atom>());
        assert_eq!(r#""This is a (test):""#, format!("{output}"));

        table.add_source("test4".into(), r"This is a test: Reject @ once".into());
        table.lex_current();
        let tokens = &table.get_tokens("test4").unwrap();
        let output = stubbed_parser(&mut table, &tokens, words::<Atom>());
        assert_eq!(
            "[found 'At' at 6..7 expected Wordlike, Typical Punctuation, or end of input]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_ident_maybe_sub() {
        let mut table = StateTable::new();
        let tokens = quick_tokens("Group: Subtable");
        let output = stubbed_parser(&mut table, &tokens, ident_maybe_sub());
        assert_eq!("`group subtable`", output);

        // TODO: This will probably create a bug somewhere, using quoted ids followed by not
        // At the moment, it's not clear where that bug will be
        let tokens = quick_tokens(r#""Treasure Hoard: Challenge 0-4": Magic Items"#);
        let output = stubbed_parser(&mut table, &tokens, ident_maybe_sub());
        assert_eq!("`treasure hoard: challenge 0-4 magic items`", output);

        let tokens = quick_tokens("A; Typo");
        let output = stubbed_parser(&mut table, &tokens, ident_maybe_sub());
        assert_eq!(
            "[found 'SemiColon' at 1..2 expected Wordlike, 'Colon', Identity, or end of input]",
            output
        );
    }

    #[test]
    fn parse_identity() {
        let mut table = StateTable::new();
        let tokens = vec![Token::Word("Simple".into())];
        let output = stubbed_parser(&mut table, &tokens, ident());
        assert_eq!("`simple`", output);

        let tokens = quick_tokens("Not _quite_ Simple");
        let output = stubbed_parser(&mut table, &tokens, ident());
        assert_eq!("`not _quite_ simple`", output);

        let tokens = quick_tokens("50 gp Art Objects");
        let output = stubbed_parser(&mut table, &tokens, ident());
        assert_eq!("`50 gp art objects`", output);

        let tokens = quick_tokens("4 8  15   16 23 42");
        let output = stubbed_parser(&mut table, &tokens, ident());
        assert_eq!("`4 8 15 16 23 42`", output);

        let tokens = quick_tokens("Once upon 1d4 times ten goblins");
        let output = stubbed_parser(&mut table, &tokens, ident());
        assert_eq!("`once upon 1d4 time ten goblins`", output);

        let tokens = quick_tokens(r#""Treasure Hoard: Challenge 0-4""#);
        let output = stubbed_parser(&mut table, &tokens, ident());
        assert_eq!("`treasure hoard: challenge 0-4`", output);

        let tokens = quick_tokens("Not: Valid");
        let output = stubbed_parser(&mut table, &tokens, ident());
        assert_eq!(
            "[found 'Colon' at 1..2 expected Wordlike, or end of input]",
            output
        );
    }

    #[test]
    fn parse_value_name() {
        let tokens = vec![
            Token::Word("bare".into()),
            Token::Word("with_underscore".into()),
            Token::Word("CaSeD".into()),
            Token::Word("CaSeD_UnDeRsCoReD".into()),
            Token::Word("num83r5".into()),
            Token::Word("num83r5_und3r5c0r3d".into()),
            Token::Word("CaSeD_NuM83r5".into()),
            Token::Digits(42),
        ];
        let mut table = StateTable::new();
        let output = stubbed_parser(&mut table, &tokens[..1], value_name());
        assert_eq!("`bare`", output);
        let output = stubbed_parser(&mut table, &tokens[1..2], value_name());
        assert_eq!("`with_underscore`", output);
        let output = stubbed_parser(&mut table, &tokens[2..3], value_name());
        assert_eq!("`cased`", output);
        let output = stubbed_parser(&mut table, &tokens[3..4], value_name());
        assert_eq!("`cased_underscored`", output);
        let output = stubbed_parser(&mut table, &tokens[4..5], value_name());
        assert_eq!("`num83r5`", output);
        let output = stubbed_parser(&mut table, &tokens[5..6], value_name());
        assert_eq!("`num83r5_und3r5c0r3d`", output);
        let output = stubbed_parser(&mut table, &tokens[6..7], value_name());
        assert_eq!("`cased_num83r5`", output);
        let output = stubbed_parser(&mut table, &tokens[7..], value_name());
        assert_eq!("[found 'Digits(42)' at 0..1 expected Value Name]", output);
    }

    #[test]
    fn parse_number() {
        let tokens = vec![
            Token::DoubleOught,
            Token::Digits(1),
            Token::Digits(9001),
            Token::One,
            Token::Two,
            Token::Three,
            Token::Four,
            Token::Five,
            Token::Six,
            Token::Seven,
            Token::Eight,
            Token::Nine,
            Token::Ten,
            Token::Once,
        ];
        let mut table = StateTable::new();
        let output = stubbed_parser(&mut table, &tokens[..1], number());
        assert_eq!("100", output);
        let output = stubbed_parser(&mut table, &tokens[1..2], number());
        assert_eq!("1", output);
        let output = stubbed_parser(&mut table, &tokens[2..3], number());
        assert_eq!("9001", output);
        let output = stubbed_parser(&mut table, &tokens[3..4], number());
        assert_eq!("1", output);
        let output = stubbed_parser(&mut table, &tokens[4..5], number());
        assert_eq!("2", output);
        let output = stubbed_parser(&mut table, &tokens[5..6], number());
        assert_eq!("3", output);
        let output = stubbed_parser(&mut table, &tokens[6..7], number());
        assert_eq!("4", output);
        let output = stubbed_parser(&mut table, &tokens[7..8], number());
        assert_eq!("5", output);
        let output = stubbed_parser(&mut table, &tokens[8..9], number());
        assert_eq!("6", output);
        let output = stubbed_parser(&mut table, &tokens[9..10], number());
        assert_eq!("7", output);
        let output = stubbed_parser(&mut table, &tokens[10..11], number());
        assert_eq!("8", output);
        let output = stubbed_parser(&mut table, &tokens[11..12], number());
        assert_eq!("9", output);
        let output = stubbed_parser(&mut table, &tokens[12..13], number());
        assert_eq!("10", output);
        let output = stubbed_parser(&mut table, &tokens[13..], number());
        assert_eq!("[found 'Once' at 0..1 expected Numeric]", output);
    }
}
