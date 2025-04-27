use chumsky::prelude::*;

use super::TaleExtra;
use crate::{
    ast::{Atom, Expr, RcNode, TypedNode, full_rc_node},
    lexer::Token,
};

/// When you want to chomp nothing
pub const NOTHING: &[Token] = &[];

pub const RBRACKET: &[Token] = &[Token::RBracket];
pub const COMMA_OR_RBRACKET: &[Token] = &[Token::RBracket, Token::Comma];

pub const COLON: &[Token] = &[Token::Colon];

pub const PERIOD_OR_SEMICOLON: &[Token] = &[Token::Period, Token::SemiColon];

pub const TABS: &[Token] = &[Token::Tabs];
pub const NEWLINES: &[Token] = &[Token::NewLines];
pub const DELIMITING_WHITESPACE: &[Token] = &[Token::Tabs, Token::NewLines];

pub const CELL_ENDINGS: &[Token] = &[
    Token::Period,
    Token::SemiColon,
    Token::Tabs,
    Token::NewLines,
];

// Match a character designating termination of the current statement
// then, rewind so the character is still available for use a delimiter
pub fn terminator<'src>(
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], (), TaleExtra<'src>> + Clone {
    one_of(end_tokens)
        .ignored()
        .or(end())
        .rewind()
        .labelled("Terminator")
}

// Where the terminator rewinds its end tokens, the separator consumes
// TODO: Maybe name these better?
pub fn chomp_separator<'src>(
    chomp_tokens: &'static [Token],
    end_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], (), TaleExtra<'src>> + Clone {
    one_of(chomp_tokens)
        .or_not()
        .then(one_of(end_tokens))
        .ignored()
        .labelled(format!("Separator( {chomp_tokens:?} -> {end_tokens:?} )"))
}

// Take care of potentially commented/tabbed lines interspersed
pub fn chomp_disjoint_newlines<'src>(
    chomp_tokens: &'static [Token],
) -> impl Parser<'src, &'src [Token], (), TaleExtra<'src>> + Clone {
    chomp_separator(chomp_tokens, NEWLINES)
        .then(chomp_separator(TABS, NEWLINES).repeated())
        .ignored()
}

pub fn term<'src>() -> impl Parser<'src, &'src [Token], RcNode<Expr>, TaleExtra<'src>> + Clone {
    let (number, dice, value_name) = (number(), dice(), value_name());
    number
        .or(dice)
        .or(value_name)
        .map_with(|term, extra| {
            let term_node = full_rc_node(term, extra);
            let span = extra.span().into_range();
            term_node.add_detail(
                "original_text".into(),
                extra.state().get_source_slice(&span).to_string(),
            );
            term_node
        })
        .boxed()
        .labelled("Arithmetic Term")
}

#[derive(Debug, Clone, Copy)]
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

pub fn op<'src>(token: Token) -> impl Parser<'src, &'src [Token], Op, TaleExtra<'src>> + Clone {
    just(token).map(Op::from).labelled("Arithmetic Operator")
}

pub fn qstring<'src>() -> impl Parser<'src, &'src [Token], Atom, TaleExtra<'src>> + Clone {
    let qstring = select! { Token::String(s) => Atom::Str(s) };
    qstring.labelled("Quoted String")
}

pub fn words<'src, T>() -> impl Parser<'src, &'src [Token], RcNode<T>, TaleExtra<'src>> + Clone
where
    T: From<Atom> + TypedNode + 'src,
{
    wordlike(true)
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

pub fn ident_maybe_sub<'src>() -> impl Parser<'src, &'src [Token], Atom, TaleExtra<'src>> + Clone {
    ident()
        .then_ignore(just(Token::Colon).or_not())
        .then(ident().or_not())
        .map(|(l, r)| {
            if let Some(r) = r {
                ident_normalize(&l, &r)
            } else {
                l
            }
        })
        .labelled("Identity")
}

pub fn ident<'src>() -> impl Parser<'src, &'src [Token], Atom, TaleExtra<'src>> + Clone {
    wordlike(false)
        .foldl(wordlike(false).repeated(), |l, r| ident_normalize(&l, &r))
        .or(qstring())
        .map(|id| Atom::Ident(id.to_lowercase()))
        .boxed()
        .labelled("Identity")
}

pub fn wordlike<'src>(
    allow_roll: bool,
) -> impl Parser<'src, &'src [Token], Atom, TaleExtra<'src>> + Clone {
    // Raw keywords coming before numbers is important
    // As numbers numeric keyswords that stand alone are parsed as their value, not the word
    word()
        .or(raw_keywords(allow_roll))
        .or(number())
        .or(dice())
        .boxed()
        .labelled("Wordlike")
}

pub fn raw_keywords<'src>(
    allow_roll: bool,
) -> impl Parser<'src, &'src [Token], Atom, TaleExtra<'src>> + Clone {
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
        Token::Roll if allow_roll => Atom::Raw(Token::Roll),
        Token::Script => Atom::Raw(Token::Script),
        Token::Set => Atom::Raw(Token::Set),
        Token::Show => Atom::Raw(Token::Show),
        Token::Table => Atom::Raw(Token::Table),
        Token::Tag => Atom::Raw(Token::Tag),
        Token::Time => Atom::Raw(Token::Time),
        Token::To => Atom::Raw(Token::To),
    };
    raw_keywords.labelled("Keyword")
}

pub fn typical_punctuation<'src>() -> impl Parser<'src, &'src [Token], Atom, TaleExtra<'src>> + Clone
{
    let typical_punctuation = select! {
        Token::Ampersand => Atom::Raw(Token::Ampersand),
        Token::Apostrophe => Atom::Raw(Token::Apostrophe),
        Token::Asterisk => Atom::Raw(Token::Asterisk),
        Token::ExclamationPoint => Atom::Raw(Token::ExclamationPoint),
        Token::Colon => Atom::Raw(Token::Colon),
        Token::Comma => Atom::Raw(Token::Comma),
        Token::Dash => Atom::Raw(Token::Dash),
        Token::Ellipsis => Atom::Raw(Token::Ellipsis),
        Token::Hash => Atom::Raw(Token::Hash),
        Token::LParens => Atom::Raw(Token::LParens),
        Token::Minus => Atom::Raw(Token::Minus),
        Token::Plus => Atom::Raw(Token::Plus),
        Token::Modulo => Atom::Raw(Token::Modulo),
        Token::Period => Atom::Raw(Token::Period),
        Token::QuestionMark => Atom::Raw(Token::QuestionMark),
        Token::RParens => Atom::Raw(Token::RParens),
        Token::SemiColon => Atom::Raw(Token::SemiColon),
        Token::Slash => Atom::Raw(Token::Slash),
    };
    typical_punctuation.labelled("Typical Punctuation")
}

pub fn value_name<'src>() -> impl Parser<'src, &'src [Token], Atom, TaleExtra<'src>> + Clone {
    let value_name = select! { Token::Word(word) => Atom::Ident(word.to_lowercase()) };
    value_name.labelled("Value Name")
}

pub fn word<'src>() -> impl Parser<'src, &'src [Token], Atom, TaleExtra<'src>> + Clone {
    let word = select! { Token::Word(word) => Atom::Str(word) };
    word.labelled("Word")
}

pub fn dice<'src>() -> impl Parser<'src, &'src [Token], Atom, TaleExtra<'src>> + Clone {
    let dice = select! { Token::DieRoll((x, y)) => Atom::Dice(x, y) };
    dice.labelled("Dice")
}

pub fn number<'src>() -> impl Parser<'src, &'src [Token], Atom, TaleExtra<'src>> + Clone {
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

pub fn ident_normalize(l: &Atom, r: &Atom) -> Atom {
    Atom::Ident(format!("{} {}", l.to_lowercase(), r.to_lowercase()))
}

#[cfg(test)]
#[allow(unused_must_use)]
mod tests {

    use super::*;
    use crate::{state::ParserState, tests::stubbed_parser};

    #[test]
    fn parse_words() {
        let mut p_state = ParserState::from_source(r"This is a test: Once upon a time...".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, words::<Atom>());
        assert_eq!(
            r#""This is a test: Once upon a time...""#,
            format!("{output}")
        );

        let mut p_state = ParserState::from_source(r"Let's do this!".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, words::<Atom>());
        assert_eq!(r#""Let's do this!""#, format!("{output}"));

        let mut p_state =
            ParserState::from_source(r"This is a (test): // Once upon a time...".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, words::<Atom>());
        assert_eq!(r#""This is a (test):""#, format!("{output}"));

        let mut p_state = ParserState::from_source(r"This is a test: Reject @ once".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, words::<Atom>());
        assert_eq!(
            "[TaleError { kind: Parse, span: 23..24, position: (1, 23), msg: \"found 'At' expected Wordlike, Typical Punctuation, or end of input\" }]",
            format!("{output}")
        );
    }

    #[test]
    fn parse_ident_maybe_sub() {
        let mut p_state = ParserState::from_source("Group: Subtable".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, ident_maybe_sub());
        assert_eq!("`group subtable`", output);

        // TODO: This will probably create a bug somewhere, using quoted ids followed by not
        // At the moment, it's not clear where that bug will be
        let mut p_state =
            ParserState::from_source(r#""Treasure Hoard: Challenge 0-4": Magic Items"#.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, ident_maybe_sub());
        assert_eq!("`treasure hoard: challenge 0-4 magic items`", output);

        let mut p_state = ParserState::from_source("A; Typo".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, ident_maybe_sub());
        assert_eq!(
            "[TaleError { kind: Parse, span: 1..2, position: (1, 1), msg: \"found 'SemiColon' expected Wordlike, 'Colon', Identity, or end of input\" }]",
            output
        );
    }

    #[test]
    fn parse_identity() {
        let mut p_state = ParserState::from_source("Simple".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, ident());
        assert_eq!("`simple`", output);

        let mut p_state = ParserState::from_source("Not _quite_ Simple".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, ident());
        assert_eq!("`not _quite_ simple`", output);

        let mut p_state = ParserState::from_source("50 gp Art Objects".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, ident());
        assert_eq!("`50 gp art objects`", output);

        let mut p_state = ParserState::from_source("4 8  15   16 23 42".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, ident());
        assert_eq!("`4 8 15 16 23 42`", output);

        let mut p_state = ParserState::from_source("Once upon 1d4 times ten goblins".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, ident());
        assert_eq!("`once upon 1d4 time ten goblins`", output);

        let mut p_state = ParserState::from_source(r#""Treasure Hoard: Challenge 0-4""#.into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, ident());
        assert_eq!("`treasure hoard: challenge 0-4`", output);

        let mut p_state = ParserState::from_source("Not: Valid".into());
        let tokens = p_state.tokens();
        let output = stubbed_parser(&mut p_state, &tokens, ident());
        assert_eq!(
            "[TaleError { kind: Parse, span: 3..4, position: (1, 3), msg: \"found 'Colon' expected Wordlike, or end of input\" }]",
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
        let mut p_state = ParserState::from_source(String::new());
        let output = stubbed_parser(&mut p_state, &tokens[..1], value_name());
        assert_eq!("`bare`", output);
        let output = stubbed_parser(&mut p_state, &tokens[1..2], value_name());
        assert_eq!("`with_underscore`", output);
        let output = stubbed_parser(&mut p_state, &tokens[2..3], value_name());
        assert_eq!("`cased`", output);
        let output = stubbed_parser(&mut p_state, &tokens[3..4], value_name());
        assert_eq!("`cased_underscored`", output);
        let output = stubbed_parser(&mut p_state, &tokens[4..5], value_name());
        assert_eq!("`num83r5`", output);
        let output = stubbed_parser(&mut p_state, &tokens[5..6], value_name());
        assert_eq!("`num83r5_und3r5c0r3d`", output);
        let output = stubbed_parser(&mut p_state, &tokens[6..7], value_name());
        assert_eq!("`cased_num83r5`", output);
        let output = stubbed_parser(&mut p_state, &tokens[7..], value_name());
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..0, position: (0, 0), msg: \"found 'Digits(42)' expected Value Name\" }]",
            output
        );
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
        let mut p_state = ParserState::from_source(String::new());
        let output = stubbed_parser(&mut p_state, &tokens[..1], number());
        assert_eq!("100", output);
        let output = stubbed_parser(&mut p_state, &tokens[1..2], number());
        assert_eq!("1", output);
        let output = stubbed_parser(&mut p_state, &tokens[2..3], number());
        assert_eq!("9001", output);
        let output = stubbed_parser(&mut p_state, &tokens[3..4], number());
        assert_eq!("1", output);
        let output = stubbed_parser(&mut p_state, &tokens[4..5], number());
        assert_eq!("2", output);
        let output = stubbed_parser(&mut p_state, &tokens[5..6], number());
        assert_eq!("3", output);
        let output = stubbed_parser(&mut p_state, &tokens[6..7], number());
        assert_eq!("4", output);
        let output = stubbed_parser(&mut p_state, &tokens[7..8], number());
        assert_eq!("5", output);
        let output = stubbed_parser(&mut p_state, &tokens[8..9], number());
        assert_eq!("6", output);
        let output = stubbed_parser(&mut p_state, &tokens[9..10], number());
        assert_eq!("7", output);
        let output = stubbed_parser(&mut p_state, &tokens[10..11], number());
        assert_eq!("8", output);
        let output = stubbed_parser(&mut p_state, &tokens[11..12], number());
        assert_eq!("9", output);
        let output = stubbed_parser(&mut p_state, &tokens[12..13], number());
        assert_eq!("10", output);
        let output = stubbed_parser(&mut p_state, &tokens[13..], number());
        assert_eq!(
            "[TaleError { kind: Parse, span: 0..0, position: (0, 0), msg: \"found 'Once' expected Numeric\" }]",
            output
        );
    }
}
