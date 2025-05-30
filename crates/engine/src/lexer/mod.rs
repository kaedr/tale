use std::{fmt::Display, str::FromStr, sync::LazyLock};

use logos::{Lexer, Logos, Span};
use regex::Regex;

use crate::error::{TaleError, TaleResultVec};

fn die_roll(lex: &mut Lexer<Token>) -> Option<(usize, usize)> {
    let parts: Vec<_> = lex.slice().split('d').collect();
    Some((
        parts.first()?.parse().ok().or(Some(1))?,
        parts.get(1)?.parse().ok()?,
    ))
}

fn digits(lex: &mut Lexer<Token>) -> Option<usize> {
    lex.slice().parse().ok()
}

fn verbatim(lex: &mut Lexer<Token>) -> String {
    lex.slice().to_string()
}

// TODO: Add in unicode support -> |[\f\v\u{2028}\u{2029}\u{85}]
static NEWLINE_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(\r\n?)|(\n\r?)").unwrap());

fn newlines_callback(lex: &mut Lexer<Token>) {
    let found_newlines = NEWLINE_REGEX.find_iter(lex.slice());
    found_newlines
        .map(|m| {
            lex.extras.0 += 1;
            lex.extras
                .1
                .push((lex.extras.0, lex.span().start + m.end()));
        })
        .count();
}

fn get_string_content(lex: &mut Lexer<Token>) -> Option<String> {
    newlines_callback(lex);
    let raw = lex.slice();
    let mut chars = raw.chars();
    chars.next();
    chars.next_back();
    String::from_str(chars.as_str()).ok()
}

/// All the Tokens that the lexer can produce
#[rustfmt::skip]
#[derive(Logos, Debug, PartialEq, Eq, PartialOrd, Hash, Clone)]
#[logos(extras = (usize, Vec<(usize, usize)>))]
#[logos(skip r"[ ]+")]
pub enum Token {
    // General Tokens
    #[regex(r"\d*d\d+",die_roll)]       DieRoll((usize, usize)),
    #[regex(r"\d+",digits,priority=3)]  Digits(usize),
    #[token("00")]                      DoubleOught,
    #[regex(r"\w+",verbatim)]           Word(String),
    #[regex(r"[\t]*//[^\n\r]+",logos::skip)] Comment,


    // Strings
    #[regex("\u{FF02}[^\u{FF02}]*\u{FF02}",get_string_content)] // FullDQuote
    #[regex(r#""[^"]*""#,get_string_content)]                   // DQuote,
    #[regex("\u{FF40}[^\u{FF40}]*\u{FF40}",get_string_content)] // FullGQuote,
    #[regex("`[^`]*`",get_string_content)]                      // GQuote,
    #[regex("\u{FF07}[^\u{FF07}]*\u{FF07}",get_string_content)] // FullSQuote,
    // #[regex("'[^']*'",get_string_content)]                      // SQuote,
    #[regex("\u{2018}[^\u{2019}]*\u{2019}",get_string_content)] // LeftSQuote, RightSQuote,
    #[regex("\u{201C}[^\u{201D}]*\u{201D}",get_string_content)] // LeftDQuote, RightDQuote,
                                        String(String),

    // Whitespace
    #[regex(r"(\r\n?)+",newlines_callback)]
    #[regex(r"(\n\r?)+",newlines_callback)]
    // #[regex(r"[\f\v\u{2028}\u{2029}\u{85}]+",newlines_callback)]
                                        NewLines,
    #[regex(r"\t+")]                    Tabs,

    // Symbols/Punctuation
    #[token("&")]                       Ampersand,
    #[token("\u{2019}")]                // RightSQuote as Apostrophe
    #[token("'")]                       Apostrophe,
    #[token("*")]                       Asterisk,
    #[token("@")]                       At,
    #[token(r"\")]                      BackSlash,
    #[token("|")]                       Bar,
    #[token("^")]                       Caret,
    #[token(":")]                       Colon,
    #[token(",")]                       Comma,
    #[token("\u{2013}")] // En Dash
    #[token("\u{2014}")] /* Em Dash */  Dash,
    #[token("$")]                       DollarSign,
    #[token("\u{2026}")] // Horizontal Ellipsis
    #[token("..")] #[token("...")]      Ellipsis,
    #[token("=")]                       Equals,
    #[token("!")]                       ExclamationPoint,
    #[token("#")]                       Hash,
    #[token("-")] #[token("\u{2212}")]  Minus,
    #[token("%")]                       Modulo,
    #[token(".")]                       Period,
    #[token("+")]                       Plus,
    #[token("?")]                       QuestionMark,
    #[token(";")]                       SemiColon,
    #[token("/")]                       Slash,
    #[token("~")]                       Tilde,
    #[token("_",priority=3)]            Underscore,

    // Brackets/Braces
    #[token("<")]                       LAngle,
    #[token("[")]                       LBracket,
    #[token("{")]                       LCurly,
    #[token("(")]                       LParens,
    #[token(">")]                       RAngle,
    #[token("]")]                       RBracket,
    #[token("}")]                       RCurly,
    #[token(")")]                       RParens,

    // Repetition Keywords
    #[token("once", ignore(case))]      Once,
    #[token("twice", ignore(case))]     Twice,
    #[token("thrice", ignore(case))]    Thrice,

    // Numeral Keywords
    #[token("one", ignore(case))]       One,
    #[token("two", ignore(case))]       Two,
    #[token("three", ignore(case))]     Three,
    #[token("four", ignore(case))]      Four,
    #[token("five", ignore(case))]      Five,
    #[token("six", ignore(case))]       Six,
    #[token("seven", ignore(case))]     Seven,
    #[token("eight", ignore(case))]     Eight,
    #[token("nine", ignore(case))]      Nine,
    #[token("ten", ignore(case))]       Ten,

    // Keywords
    #[token("all", ignore(case))]       All,
    #[token("and", ignore(case))]       And,
    #[token("clear", ignore(case))]     Clear,
    #[token("end", ignore(case))]       End,
    #[token("group", ignore(case))]     Group,
    #[token("invoke", ignore(case))]    Invoke,
    #[token("list", ignore(case))]      List,
    #[token("load", ignore(case))]      Load,
    #[token("lookup", ignore(case))]    Lookup,
    #[token("modify", ignore(case))]    Modify,
    #[token("next", ignore(case))]      Next,
    #[token("on", ignore(case))]        On,
    #[token("output", ignore(case))]    Output,
    #[regex(r"rolls?", ignore(case))]   Roll,
    #[token("script", ignore(case))]    Script,
    #[token("set", ignore(case))]       Set,
    #[token("show", ignore(case))]      Show,
    #[token("table", ignore(case))]     Table,
    #[regex(r"tags?", ignore(case))]    Tag,
    #[regex(r"times?", ignore(case))]   Time,
    #[token("to", ignore(case))]        To,
}

impl Token {
    /// Return a lowercased string representing the token.
    pub fn to_lowercase(&self) -> String {
        match self {
            Self::DieRoll((num, sides)) => format!("{num}d{sides}"),
            Self::Digits(num) => num.to_string(),
            Self::Word(word) => word.to_lowercase(),
            Self::String(string) => string.to_lowercase(),
            token => format!("{token:?}").to_lowercase(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

pub type Position = (usize, usize);

pub type Lexicon = Vec<(Token, Span, Position)>;

/// Lexes the source string into a vector of tokens, their source spans,
/// and their line/characters positions.
pub fn tokenize(source: &str) -> TaleResultVec<Lexicon> {
    let mut lex = Token::lexer(source);
    let mut tokens = Vec::new();
    let mut errs = Vec::new();

    while let Some(token) = lex.next() {
        let span = lex.span();
        if let Ok(token) = token {
            let position = find_position(span.start, &lex.extras.1);
            tokens.push((token, span, position));
        } else {
            let position = find_position(span.start, &lex.extras.1);
            errs.push(TaleError::lexer(
                span,
                position,
                format!("Invalid input character(s): '{}'", lex.slice()),
            ));
        }
    }

    if errs.is_empty() {
        Ok(tokens)
    } else {
        Err(errs)
    }
}

fn find_position(start: usize, lines: &[(usize, usize)]) -> Position {
    for (line, line_end) in lines.iter().rev() {
        if start >= *line_end {
            return (line + 1, start - line_end);
        }
    }
    (1, start)
}

#[cfg(test)]
#[allow(unused_must_use)]
pub(crate) mod tests {
    use super::*;

    #[cfg(test)]
    mod test_sample_files {
        use super::*;
        use crate::samples::*;

        #[test]
        #[cfg(not(target_os = "windows"))]
        fn tokenotomy() {
            let token_vec: Vec<_> = tokenize(STRINGS).unwrap();
            assert_eq!(
                token_vec[0..2],
                [
                    (
                        Token::String("double quoted string: '`".into()),
                        0..26,
                        (1, 0)
                    ),
                    (Token::NewLines, 26..27, (1, 26)),
                ]
            );

            assert_eq!(
                token_vec[2..4],
                [
                    (
                        Token::String(r#"grave quoted string: '""#.into()),
                        27..52,
                        (2, 0)
                    ),
                    (Token::NewLines, 52..53, (2, 25)),
                ]
            );

            assert_eq!(
                token_vec[4..6],
                [
                    (
                        Token::String("string\nwith\nnewlines".into()),
                        53..75,
                        (3, 0)
                    ),
                    (Token::NewLines, 75..76, (5, 9)),
                ]
            );

            assert_eq!(
                token_vec[6..],
                [
                    (Token::String(String::new()), 76..78, (6, 0)),
                    (Token::NewLines, 78..79, (6, 2)),
                ]
            );
        }

        #[test]
        #[cfg(target_os = "windows")]
        fn tokenotomy_windows() {
            let token_vec: Vec<_> = tokenize(STRINGS).unwrap();
            assert_eq!(
                token_vec[0..2],
                [
                    (
                        Token::String("double quoted string: '`".into()),
                        0..26,
                        (1, 0)
                    ),
                    (Token::NewLines, 26..28, (1, 26)),
                ]
            );

            assert_eq!(
                token_vec[2..4],
                [
                    (
                        Token::String(r#"grave quoted string: '""#.into()),
                        28..53,
                        (2, 0)
                    ),
                    (Token::NewLines, 53..55, (2, 25)),
                ]
            );

            assert_eq!(
                token_vec[4..6],
                [
                    (
                        Token::String("string\r\nwith\r\nnewlines".into()),
                        55..79,
                        (3, 0)
                    ),
                    (Token::NewLines, 79..81, (5, 9)),
                ]
            );

            assert_eq!(
                token_vec[6..],
                [
                    (Token::String("".into()), 81..83, (6, 0)),
                    (Token::NewLines, 83..85, (6, 2)),
                ]
            );
        }

        #[test]
        fn table_minimal() {
            let mut lex = Token::lexer(TABLE_MINIMAL);

            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::Colon)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("Minimalism".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("less".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("is".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("more".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::End)));
            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn table_roll_def() {
            let mut lex = Token::lexer(TABLE_ROLL_DEF);

            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::Colon)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("Melee".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("Attack".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Roll)));
            assert_eq!(lex.next(), Some(Ok(Token::Colon)));
            assert_eq!(lex.next(), Some(Ok(Token::DieRoll((1, 20)))));
            assert_eq!(lex.next(), Some(Ok(Token::Plus)));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(7))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::End)));
            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn table_list() {
            let mut lex = Token::lexer(TABLE_LIST);

            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::Colon)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("Groceries".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::List)));
            assert_eq!(lex.next(), Some(Ok(Token::Colon)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("eggs".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Comma)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("bacon".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Comma)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("milk".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Comma)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("butter".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Comma)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("bread".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn table_keyed_numeric() {
            let mut lex = Token::lexer(TABLE_KEYED_NUMERIC);

            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::Colon)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("NumKeyed".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(1))));
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("Is".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("the".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("loneliest".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("number".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(2))));
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("Can".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("be".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("as".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("bad".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("as".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::One)));
            assert_eq!(lex.slice(), "one");
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(3))));
            assert_eq!(lex.next(), Some(Ok(Token::Minus)));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(4))));
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("Range".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(5))));
            assert_eq!(lex.next(), Some(Ok(Token::Comma)));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(6))));
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("CSV".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::End)));
            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn table_keyed_word() {
            let mut lex = Token::lexer(TABLE_KEYED_WORD);

            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::Colon)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("TextKeys".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Once)));
            assert_eq!(lex.slice(), "once");
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("upon".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("a".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::Time)));
            assert_eq!(lex.slice(), "time");
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::End)));
            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        #[allow(clippy::too_many_lines)] // It's a test and it's this long because of cargo fmt
        fn table_group() {
            let lex = Token::lexer(TABLE_GROUP);

            let token_vec: Vec<_> = lex.flatten().collect();
            assert_eq!(
                token_vec[0..5],
                [
                    Token::Table,
                    Token::Group,
                    Token::Colon,
                    Token::String("Treasure Chest: Challenge 0-4".into()),
                    Token::NewLines,
                ]
            );
            assert_eq!(
                token_vec[5..15],
                [
                    Token::DieRoll((1, 20)),
                    Token::Tabs,
                    Token::Word("Coins".into()),
                    Token::Tabs,
                    Token::Word("Other".into()),
                    Token::Word("Valuables".into()),
                    Token::Tabs,
                    Token::Word("Magic".into()),
                    Token::Word("Items".into()),
                    Token::NewLines,
                ]
            );
            assert_eq!(
                token_vec[15..25],
                [
                    Token::Digits(1),
                    Token::Tabs,
                    Token::Minus,
                    Token::Tabs,
                    Token::Minus,
                    Token::Tabs,
                    Token::Invoke,
                    Token::Word("Mimic".into()),
                    Token::Word("Encounter".into()),
                    Token::NewLines,
                ]
            );
            assert_eq!(
                token_vec[201..],
                [Token::End, Token::Table, Token::Group, Token::NewLines]
            );
        }

        #[test]
        fn statement_assignment() {
            let lex = Token::lexer(STATEMENT_ASSIGNMENT);
            let token_vec: Vec<_> = lex.flatten().collect();

            assert_eq!(
                token_vec[..5],
                [
                    Token::Set,
                    Token::Word("bird".into()),
                    Token::Equals,
                    Token::Digits(42),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[5..10],
                [
                    Token::Set,
                    Token::Word("the_word".into()),
                    Token::Equals,
                    Token::Word("bird".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[10..15],
                [
                    Token::Set,
                    Token::Word("the_word".into()),
                    Token::To,
                    Token::String("bird".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[15..20],
                [
                    Token::Set,
                    Token::Word("the_word".into()),
                    Token::To,
                    Token::String("bird".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[20..26],
                [
                    Token::Word("force".into()),
                    Token::Equals,
                    Token::Word("mass".into()),
                    Token::Asterisk,
                    Token::Word("acceleration".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[26..],
                [
                    Token::Set,
                    Token::Word("minutes".into()),
                    Token::To,
                    Token::Word("midnight".into()),
                    Token::NewLines
                ]
            );
        }

        #[test]
        fn statement_expression() {
            let lex = Token::lexer(STATEMENT_EXPRESSION);
            let token_vec: Vec<_> = lex.flatten().collect();

            assert_eq!(
                token_vec[..12],
                [
                    Token::DieRoll((1, 7)),
                    Token::Asterisk,
                    Token::Digits(8),
                    Token::Plus,
                    Token::Digits(4),
                    Token::Caret,
                    Token::LParens,
                    Token::DieRoll((1, 4)),
                    Token::Minus,
                    Token::Digits(1),
                    Token::RParens,
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[12..],
                [
                    Token::DieRoll((8, 6)),
                    Token::Slash,
                    Token::LParens,
                    Token::DieRoll((1, 100)),
                    Token::Modulo,
                    Token::Digits(10),
                    Token::Plus,
                    Token::Digits(1),
                    Token::RParens,
                    Token::NewLines
                ]
            );
        }

        #[test]
        fn statement_clear() {
            let lex = Token::lexer(STATEMENT_CLEAR);
            let token_vec: Vec<_> = lex.flatten().collect();

            assert_eq!(
                token_vec[..6],
                [
                    Token::Clear,
                    Token::Next,
                    Token::Digits(10),
                    Token::Word("Quality".into()),
                    Token::Roll,
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[6..11],
                [
                    Token::Clear,
                    Token::All,
                    Token::Word("Quality".into()),
                    Token::Roll,
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[11..16],
                [
                    Token::Clear,
                    Token::Next,
                    Token::Digits(10),
                    Token::Word("Quality".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[16..],
                [
                    Token::Clear,
                    Token::All,
                    Token::Word("Quality".into()),
                    Token::NewLines
                ]
            );
        }

        #[test]
        fn statement_invoke() {
            let lex = Token::lexer(STATEMENT_INVOKE);
            let token_vec: Vec<_> = lex.flatten().collect();

            assert_eq!(
                token_vec[..7],
                [
                    Token::Invoke,
                    Token::Word("Some".into()),
                    Token::Word("kind".into()),
                    Token::Word("of".into()),
                    Token::Word("bizarre".into()),
                    Token::Word("ritual".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[7..],
                [
                    Token::Invoke,
                    Token::Word("last".into()),
                    Token::Word("rites".into()),
                    Token::NewLines
                ]
            );
        }

        #[test]
        fn statement_load() {
            let lex = Token::lexer(STATEMENT_LOAD);
            let token_vec: Vec<_> = lex.flatten().collect();

            assert_eq!(
                token_vec[..5],
                [
                    Token::Load,
                    Token::Word("01_table_minimal".into()),
                    Token::Period,
                    Token::Word("tale".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[5..14],
                [
                    Token::Load,
                    Token::Word("src".into()),
                    Token::Slash,
                    Token::Word("samples".into()),
                    Token::Slash,
                    Token::Word("01_table_minimal".into()),
                    Token::Period,
                    Token::Word("tale".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[14..18],
                [
                    Token::Load,
                    Token::Colon,
                    Token::String("../../tons of _odd-characters_.tale".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[18..],
                [
                    Token::Load,
                    Token::Ellipsis,
                    Token::Slash,
                    Token::Ellipsis,
                    Token::Slash,
                    Token::Word("tons".into()),
                    Token::Word("of".into()),
                    Token::Word("_odd".into()),
                    Token::Minus,
                    Token::Word("characters_".into()),
                    Token::Period,
                    Token::Word("tale".into()),
                    Token::NewLines
                ]
            );
        }

        #[test]
        fn statement_lookup() {
            let lex = Token::lexer(STATEMENT_LOOKUP);
            let token_vec: Vec<_> = lex.flatten().collect();

            assert_eq!(
                token_vec[..5],
                [
                    Token::Lookup,
                    Token::Word("a".into()),
                    Token::On,
                    Token::Word("TextKeys".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[5..10],
                [
                    Token::Lookup,
                    Token::Digits(3),
                    Token::On,
                    Token::Word("NumKeyed".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[10..15],
                [
                    Token::Lookup,
                    Token::DieRoll((1, 4)),
                    Token::On,
                    Token::Word("NumKeyed".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[15..],
                [
                    Token::Lookup,
                    Token::DieRoll((1, 3)),
                    Token::Plus,
                    Token::Digits(1),
                    Token::On,
                    Token::Word("NumKeyed".into()),
                    Token::NewLines
                ]
            );
        }

        #[test]
        fn statement_modify() {
            let lex = Token::lexer(STATEMENT_MODIFY);
            let token_vec: Vec<_> = lex.flatten().collect();

            assert_eq!(
                token_vec[..7],
                [
                    Token::Modify,
                    Token::All,
                    Token::Word("Quality".into()),
                    Token::Roll,
                    Token::Plus,
                    Token::Digits(2),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[7..15],
                [
                    Token::Modify,
                    Token::Next,
                    Token::Digits(7),
                    Token::Word("Quality".into()),
                    Token::Roll,
                    Token::Plus,
                    Token::Digits(3),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[15..23],
                [
                    Token::Minus,
                    Token::Digits(2),
                    Token::To,
                    Token::Next,
                    Token::Digits(3),
                    Token::Word("Quality".into()),
                    Token::Roll,
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[23..],
                [
                    Token::Plus,
                    Token::Digits(10),
                    Token::To,
                    Token::All,
                    Token::Word("Quality".into()),
                    Token::Roll,
                    Token::NewLines
                ]
            );
        }

        #[test]
        fn statement_output() {
            let lex = Token::lexer(STATEMENT_OUTPUT);
            let token_vec: Vec<_> = lex.flatten().collect();

            assert_eq!(
                token_vec[..19],
                [
                    Token::Output,
                    Token::Colon,
                    Token::Word("There".into()),
                    Token::Word("are".into()),
                    Token::LBracket,
                    Token::DieRoll((1, 6)),
                    Token::Minus,
                    Token::Digits(1),
                    Token::RBracket,
                    Token::Word("lights".into()),
                    Token::Word("illuminated".into()),
                    Token::Word("out".into()),
                    Token::Word("of".into()),
                    Token::Word("a".into()),
                    Token::Word("total".into()),
                    Token::Word("of".into()),
                    Token::Digits(5),
                    Token::Period,
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[19..],
                [
                    Token::Output,
                    Token::String("A lovely string".into()),
                    Token::NewLines
                ]
            );
        }

        #[test]
        fn statement_roll() {
            let lex = Token::lexer(STATEMENT_ROLL);
            let token_vec: Vec<_> = lex.flatten().collect();

            assert_eq!(
                token_vec[..3],
                [Token::Roll, Token::DieRoll((3, 6)), Token::NewLines]
            );

            assert_eq!(
                token_vec[3..8],
                [
                    Token::Roll,
                    Token::DieRoll((1, 20)),
                    Token::Plus,
                    Token::Digits(7),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[8..14],
                [
                    Token::Roll,
                    Token::On,
                    Token::Word("Basic".into()),
                    Token::Word("Magic".into()),
                    Token::Word("Items".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[14..22],
                [
                    Token::Roll,
                    Token::Digits(7),
                    Token::Time,
                    Token::On,
                    Token::Word("Basic".into()),
                    Token::Word("Magic".into()),
                    Token::Word("Items".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[22..30],
                [
                    Token::Roll,
                    Token::DieRoll((1, 6)),
                    Token::Time,
                    Token::On,
                    Token::Word("Basic".into()),
                    Token::Word("Magic".into()),
                    Token::Word("Items".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[30..37],
                [
                    Token::Roll,
                    Token::DieRoll((1, 6)),
                    Token::On,
                    Token::Word("Basic".into()),
                    Token::Word("Magic".into()),
                    Token::Word("Items".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[37..46],
                [
                    Token::Roll,
                    Token::DieRoll((1, 4)),
                    Token::Plus,
                    Token::Digits(2),
                    Token::Time,
                    Token::On,
                    Token::Word("Farm".into()),
                    Token::Word("Animals".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[46..55],
                [
                    Token::Roll,
                    Token::DieRoll((1, 6)),
                    Token::Minus,
                    Token::Digits(1),
                    Token::Time,
                    Token::On,
                    Token::Word("Farm".into()),
                    Token::Word("Animals".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[55..62],
                [
                    Token::DieRoll((1, 6)),
                    Token::Roll,
                    Token::On,
                    Token::Table,
                    Token::Colon,
                    Token::String("Magic Items #3".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[62..66],
                [
                    Token::Roll,
                    Token::Word("Farm".into()),
                    Token::Word("Animals".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[66..70],
                [
                    Token::Once,
                    Token::Word("Farm".into()),
                    Token::Word("Animals".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[70..74],
                [
                    Token::Digits(1),
                    Token::Word("Farm".into()),
                    Token::Word("Animals".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[74..78],
                [
                    Token::Roll,
                    Token::Digits(3),
                    Token::DieRoll((1, 6)),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[78..81],
                [
                    Token::DieRoll((1, 6)),
                    Token::DieRoll((1, 6)),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[81..],
                [Token::Digits(6), Token::DieRoll((3, 6)), Token::NewLines]
            );
        }

        #[test]
        fn statement_show() {
            let lex = Token::lexer(STATEMENT_SHOW);
            let token_vec: Vec<_> = lex.flatten().collect();

            assert_eq!(
                token_vec[..3],
                [
                    Token::Show,
                    Token::Word("Minimalism".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[3..6],
                [Token::Show, Token::Word("midnight".into()), Token::NewLines]
            );

            assert_eq!(
                token_vec[6..9],
                [
                    Token::Show,
                    Token::Word("Variables".into()),
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[9..12],
                [Token::Show, Token::Word("Tables".into()), Token::NewLines]
            );

            assert_eq!(
                token_vec[12..15],
                [Token::Show, Token::Script, Token::NewLines]
            );

            assert_eq!(
                token_vec[15..],
                [
                    Token::Show,
                    Token::Tag,
                    Token::Word("Desert".into()),
                    Token::NewLines
                ]
            );
        }

        #[test]
        fn script() {
            let lex = Token::lexer(SCRIPT);
            let token_vec: Vec<_> = lex.flatten().collect();

            assert_eq!(
                token_vec[..12],
                [
                    Token::Script,
                    Token::Colon,
                    Token::Word("LoadSome".into()),
                    Token::NewLines,
                    Token::Load,
                    Token::Word("02_table_roll_def".into()),
                    Token::Period,
                    Token::Word("tale".into()),
                    Token::NewLines,
                    Token::End,
                    Token::Script,
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[12..33],
                [
                    Token::Script,
                    Token::Colon,
                    Token::Word("Attack".into()),
                    Token::Word("with".into()),
                    Token::Word("Damage".into()),
                    Token::NewLines,
                    Token::Tabs,
                    Token::Roll,
                    Token::On,
                    Token::Word("Melee".into()),
                    Token::Word("Attack".into()),
                    Token::NewLines,
                    Token::Tabs,
                    Token::Roll,
                    Token::DieRoll((2, 6)),
                    Token::Plus,
                    Token::Digits(3),
                    Token::NewLines,
                    Token::End,
                    Token::Script,
                    Token::NewLines
                ]
            );

            assert_eq!(
                token_vec[33..],
                [
                    Token::Script,
                    Token::Colon,
                    Token::String("roll after load".into()),
                    Token::NewLines,
                    Token::Invoke,
                    Token::Word("LoadSome".into()),
                    Token::NewLines,
                    Token::Tabs,
                    Token::Invoke,
                    Token::Word("Attack".into()),
                    Token::Word("with".into()),
                    Token::Word("Damage".into()),
                    Token::NewLines,
                    Token::NewLines,
                    Token::End,
                    Token::Script,
                    Token::NewLines
                ]
            );
        }
    }

    #[cfg(test)]
    mod test_general_tokens {

        use super::*;
        use crate::samples::STRINGS;

        #[test]
        fn die_rolls() {
            let mut lex = Token::lexer(r"d6 2d10 1d1 0d0");

            assert_eq!(lex.next(), Some(Ok(Token::DieRoll((1, 6)))));
            assert_eq!(lex.next(), Some(Ok(Token::DieRoll((2, 10)))));
            assert_eq!(lex.next(), Some(Ok(Token::DieRoll((1, 1)))));
            assert_eq!(lex.next(), Some(Ok(Token::DieRoll((0, 0)))));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn digits() {
            let mut lex = Token::lexer(r"0 1 2 3 4 5 6 7 8 9 10 8675309");

            assert_eq!(lex.next(), Some(Ok(Token::Digits(0))));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(1))));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(2))));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(3))));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(4))));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(5))));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(6))));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(7))));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(8))));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(9))));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(10))));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(8_675_309))));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn words() {
            let mut lex = Token::lexer(
                r"This is some text:
                It spans three or four lines.
                Potato!
            ",
            );

            assert_eq!(lex.next(), Some(Ok(Token::Word("This".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("is".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("some".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("text".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Colon)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("It".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("spans".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Three)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("or".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Four)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("lines".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Period)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("Potato".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::ExclamationPoint)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn comments() {
            let mut lex = Token::lexer("not comment//comment\nnext line // more comment");

            assert_eq!(lex.next(), Some(Ok(Token::Word("not".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Word("comment".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Next)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("line".into()))));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn strings() {
            let mut lex = Token::lexer(STRINGS);

            assert_eq!(
                lex.next(),
                Some(Ok(Token::String("double quoted string: '`".into())))
            );
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(
                lex.next(),
                Some(Ok(Token::String(r#"grave quoted string: '""#.into())))
            );
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));

            #[cfg(not(target_os = "windows"))]
            {
                assert_eq!(
                    lex.next(),
                    Some(Ok(Token::String("string\nwith\nnewlines".into())))
                );
            }

            #[cfg(target_os = "windows")]
            {
                assert_eq!(
                    lex.next(),
                    Some(Ok(Token::String("string\r\nwith\r\nnewlines".into())))
                );
            }

            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::String(String::new()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), None);

            #[cfg(not(target_os = "windows"))]
            {
                assert_eq!(
                    lex.extras.1,
                    vec![(1, 27), (2, 53), (3, 61), (4, 66), (5, 76), (6, 79)]
                );
            }

            #[cfg(target_os = "windows")]
            {
                assert_eq!(
                    lex.extras.1,
                    vec![(1, 28), (2, 55), (3, 64), (4, 70), (5, 81), (6, 85)]
                );
            }
        }

        #[test]
        fn whitespace() {
            let mut lex = Token::lexer("\t\t\t\n\n\t\r\n\t\n\r\t\r");

            // assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            // assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Form Feed
            // assert_eq!(lex.extras.0, 1);
            // assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            // assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Vertical tab (VT)
            // assert_eq!(lex.extras.0, 2);
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Multiple line feeds (LF)
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Carriage Return Line Feed (CRLF)
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Line Feed Carriage Return (LFCR)
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Carriage Return (CR)
            // assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            // assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Line Separator (LS)
            // assert_eq!(lex.extras.0, 8);
            // assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            // assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Page Separator (PS)
            // assert_eq!(lex.extras.0, 9);
            // assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            // assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Next Line (NEL)
            // assert_eq!(lex.extras.0, 10);
            assert_eq!(lex.next(), None);
            assert_eq!(lex.extras.1, vec![(1, 4), (2, 5), (3, 8), (4, 11), (5, 13)]);
        }
    }

    #[cfg(test)]
    mod test_symbols {
        use super::*;

        #[test]
        fn sym_individuals() {
            let mut lex = Token::lexer(r"~!@$%^&*_+=|\:;',.?/#");

            assert_eq!(lex.next(), Some(Ok(Token::Tilde)));
            assert_eq!(lex.next(), Some(Ok(Token::ExclamationPoint)));
            assert_eq!(lex.next(), Some(Ok(Token::At)));
            assert_eq!(lex.next(), Some(Ok(Token::DollarSign)));
            assert_eq!(lex.next(), Some(Ok(Token::Modulo)));
            assert_eq!(lex.next(), Some(Ok(Token::Caret)));
            assert_eq!(lex.next(), Some(Ok(Token::Ampersand)));
            assert_eq!(lex.next(), Some(Ok(Token::Asterisk)));
            assert_eq!(lex.next(), Some(Ok(Token::Underscore)));
            assert_eq!(lex.next(), Some(Ok(Token::Plus)));
            assert_eq!(lex.next(), Some(Ok(Token::Equals)));
            assert_eq!(lex.next(), Some(Ok(Token::Bar)));
            assert_eq!(lex.next(), Some(Ok(Token::BackSlash)));
            assert_eq!(lex.next(), Some(Ok(Token::Colon)));
            assert_eq!(lex.next(), Some(Ok(Token::SemiColon)));
            assert_eq!(lex.next(), Some(Ok(Token::Apostrophe)));
            assert_eq!(lex.next(), Some(Ok(Token::Comma)));
            assert_eq!(lex.next(), Some(Ok(Token::Period)));
            assert_eq!(lex.next(), Some(Ok(Token::QuestionMark)));
            assert_eq!(lex.next(), Some(Ok(Token::Slash)));
            assert_eq!(lex.next(), Some(Ok(Token::Hash)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn sym_dashes() {
            let mut lex = Token::lexer("-−–—");

            assert_eq!(lex.next(), Some(Ok(Token::Minus)));
            assert_eq!(lex.next(), Some(Ok(Token::Minus)));
            assert_eq!(lex.next(), Some(Ok(Token::Dash)));
            assert_eq!(lex.next(), Some(Ok(Token::Dash)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn sym_ellipsis() {
            let mut lex = Token::lexer(". .. ... .... ..... ...... …");

            assert_eq!(lex.next(), Some(Ok(Token::Period))); // One dot
            // First space
            assert_eq!(lex.next(), Some(Ok(Token::Ellipsis))); // Two dot
            // Second space
            assert_eq!(lex.next(), Some(Ok(Token::Ellipsis))); // Three dot
            // Third space
            assert_eq!(lex.next(), Some(Ok(Token::Ellipsis))); // Four dot
            assert_eq!(lex.next(), Some(Ok(Token::Period)));
            // Fourth space
            assert_eq!(lex.next(), Some(Ok(Token::Ellipsis))); // Five dot
            assert_eq!(lex.next(), Some(Ok(Token::Ellipsis)));
            // Fifth space
            assert_eq!(lex.next(), Some(Ok(Token::Ellipsis))); // Six dot
            assert_eq!(lex.next(), Some(Ok(Token::Ellipsis)));
            // Sixth space
            assert_eq!(lex.next(), Some(Ok(Token::Ellipsis)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn sym_enclosures() {
            let mut lex = Token::lexer(r"<[{()}]>");

            assert_eq!(lex.next(), Some(Ok(Token::LAngle)));
            assert_eq!(lex.next(), Some(Ok(Token::LBracket)));
            assert_eq!(lex.next(), Some(Ok(Token::LCurly)));
            assert_eq!(lex.next(), Some(Ok(Token::LParens)));
            assert_eq!(lex.next(), Some(Ok(Token::RParens)));
            assert_eq!(lex.next(), Some(Ok(Token::RCurly)));
            assert_eq!(lex.next(), Some(Ok(Token::RBracket)));
            assert_eq!(lex.next(), Some(Ok(Token::RAngle)));
            assert_eq!(lex.next(), None);
        }
    }

    #[cfg(test)]
    mod test_keywords {
        use super::*;

        #[test]
        fn kw_once() {
            let mut lex = Token::lexer("ONCE Once once");

            assert_eq!(lex.next(), Some(Ok(Token::Once)));
            assert_eq!(lex.next(), Some(Ok(Token::Once)));
            assert_eq!(lex.next(), Some(Ok(Token::Once)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_twice() {
            let mut lex = Token::lexer("TWICE Twice twice");

            assert_eq!(lex.next(), Some(Ok(Token::Twice)));
            assert_eq!(lex.next(), Some(Ok(Token::Twice)));
            assert_eq!(lex.next(), Some(Ok(Token::Twice)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_thrice() {
            let mut lex = Token::lexer("THRICE Thrice thrice");

            assert_eq!(lex.next(), Some(Ok(Token::Thrice)));
            assert_eq!(lex.next(), Some(Ok(Token::Thrice)));
            assert_eq!(lex.next(), Some(Ok(Token::Thrice)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_one() {
            let mut lex = Token::lexer("ONE One one");

            assert_eq!(lex.next(), Some(Ok(Token::One)));
            assert_eq!(lex.next(), Some(Ok(Token::One)));
            assert_eq!(lex.next(), Some(Ok(Token::One)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_two() {
            let mut lex = Token::lexer("TWO Two two");

            assert_eq!(lex.next(), Some(Ok(Token::Two)));
            assert_eq!(lex.next(), Some(Ok(Token::Two)));
            assert_eq!(lex.next(), Some(Ok(Token::Two)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_three() {
            let mut lex = Token::lexer("THREE Three three");

            assert_eq!(lex.next(), Some(Ok(Token::Three)));
            assert_eq!(lex.next(), Some(Ok(Token::Three)));
            assert_eq!(lex.next(), Some(Ok(Token::Three)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_four() {
            let mut lex = Token::lexer("FOUR Four four");

            assert_eq!(lex.next(), Some(Ok(Token::Four)));
            assert_eq!(lex.next(), Some(Ok(Token::Four)));
            assert_eq!(lex.next(), Some(Ok(Token::Four)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_five() {
            let mut lex = Token::lexer("FIVE Five five");

            assert_eq!(lex.next(), Some(Ok(Token::Five)));
            assert_eq!(lex.next(), Some(Ok(Token::Five)));
            assert_eq!(lex.next(), Some(Ok(Token::Five)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_six() {
            let mut lex = Token::lexer("SIX Six six");

            assert_eq!(lex.next(), Some(Ok(Token::Six)));
            assert_eq!(lex.next(), Some(Ok(Token::Six)));
            assert_eq!(lex.next(), Some(Ok(Token::Six)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_seven() {
            let mut lex = Token::lexer("SEVEN Seven seven");

            assert_eq!(lex.next(), Some(Ok(Token::Seven)));
            assert_eq!(lex.next(), Some(Ok(Token::Seven)));
            assert_eq!(lex.next(), Some(Ok(Token::Seven)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_eight() {
            let mut lex = Token::lexer("EIGHT Eight eight");

            assert_eq!(lex.next(), Some(Ok(Token::Eight)));
            assert_eq!(lex.next(), Some(Ok(Token::Eight)));
            assert_eq!(lex.next(), Some(Ok(Token::Eight)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_nine() {
            let mut lex = Token::lexer("NINE Nine nine");

            assert_eq!(lex.next(), Some(Ok(Token::Nine)));
            assert_eq!(lex.next(), Some(Ok(Token::Nine)));
            assert_eq!(lex.next(), Some(Ok(Token::Nine)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_ten() {
            let mut lex = Token::lexer("TEN Ten ten");

            assert_eq!(lex.next(), Some(Ok(Token::Ten)));
            assert_eq!(lex.next(), Some(Ok(Token::Ten)));
            assert_eq!(lex.next(), Some(Ok(Token::Ten)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_all() {
            let mut lex = Token::lexer("ALL All all");

            assert_eq!(lex.next(), Some(Ok(Token::All)));
            assert_eq!(lex.next(), Some(Ok(Token::All)));
            assert_eq!(lex.next(), Some(Ok(Token::All)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_and() {
            let mut lex = Token::lexer("AND And and");

            assert_eq!(lex.next(), Some(Ok(Token::And)));
            assert_eq!(lex.next(), Some(Ok(Token::And)));
            assert_eq!(lex.next(), Some(Ok(Token::And)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_clear() {
            let mut lex = Token::lexer("CLEAR Clear clear");

            assert_eq!(lex.next(), Some(Ok(Token::Clear)));
            assert_eq!(lex.next(), Some(Ok(Token::Clear)));
            assert_eq!(lex.next(), Some(Ok(Token::Clear)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_end() {
            let mut lex = Token::lexer("END End end");

            assert_eq!(lex.next(), Some(Ok(Token::End)));
            assert_eq!(lex.next(), Some(Ok(Token::End)));
            assert_eq!(lex.next(), Some(Ok(Token::End)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_group() {
            let mut lex = Token::lexer("GROUP Group group");

            assert_eq!(lex.next(), Some(Ok(Token::Group)));
            assert_eq!(lex.next(), Some(Ok(Token::Group)));
            assert_eq!(lex.next(), Some(Ok(Token::Group)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_invoke() {
            let mut lex = Token::lexer("INVOKE Invoke invoke");

            assert_eq!(lex.next(), Some(Ok(Token::Invoke)));
            assert_eq!(lex.next(), Some(Ok(Token::Invoke)));
            assert_eq!(lex.next(), Some(Ok(Token::Invoke)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_list() {
            let mut lex = Token::lexer("LIST List list");

            assert_eq!(lex.next(), Some(Ok(Token::List)));
            assert_eq!(lex.next(), Some(Ok(Token::List)));
            assert_eq!(lex.next(), Some(Ok(Token::List)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_load() {
            let mut lex = Token::lexer("LOAD Load load");

            assert_eq!(lex.next(), Some(Ok(Token::Load)));
            assert_eq!(lex.next(), Some(Ok(Token::Load)));
            assert_eq!(lex.next(), Some(Ok(Token::Load)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_lookup() {
            let mut lex = Token::lexer("LOOKUP Lookup lookup");

            assert_eq!(lex.next(), Some(Ok(Token::Lookup)));
            assert_eq!(lex.next(), Some(Ok(Token::Lookup)));
            assert_eq!(lex.next(), Some(Ok(Token::Lookup)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_modify() {
            let mut lex = Token::lexer("MODIFY Modify modify");

            assert_eq!(lex.next(), Some(Ok(Token::Modify)));
            assert_eq!(lex.next(), Some(Ok(Token::Modify)));
            assert_eq!(lex.next(), Some(Ok(Token::Modify)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_next() {
            let mut lex = Token::lexer("NEXT Next next");

            assert_eq!(lex.next(), Some(Ok(Token::Next)));
            assert_eq!(lex.next(), Some(Ok(Token::Next)));
            assert_eq!(lex.next(), Some(Ok(Token::Next)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_on() {
            let mut lex = Token::lexer("ON On on");

            assert_eq!(lex.next(), Some(Ok(Token::On)));
            assert_eq!(lex.next(), Some(Ok(Token::On)));
            assert_eq!(lex.next(), Some(Ok(Token::On)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_output() {
            let mut lex = Token::lexer("Output Output output");

            assert_eq!(lex.next(), Some(Ok(Token::Output)));
            assert_eq!(lex.next(), Some(Ok(Token::Output)));
            assert_eq!(lex.next(), Some(Ok(Token::Output)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_roll() {
            let mut lex = Token::lexer("ROLL Roll roll ROLLS Rolls rolls");

            assert_eq!(lex.next(), Some(Ok(Token::Roll)));
            assert_eq!(lex.next(), Some(Ok(Token::Roll)));
            assert_eq!(lex.next(), Some(Ok(Token::Roll)));
            assert_eq!(lex.next(), Some(Ok(Token::Roll)));
            assert_eq!(lex.next(), Some(Ok(Token::Roll)));
            assert_eq!(lex.next(), Some(Ok(Token::Roll)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_script() {
            let mut lex = Token::lexer("SCRIPT Script script");

            assert_eq!(lex.next(), Some(Ok(Token::Script)));
            assert_eq!(lex.next(), Some(Ok(Token::Script)));
            assert_eq!(lex.next(), Some(Ok(Token::Script)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_set() {
            let mut lex = Token::lexer("SET Set set");

            assert_eq!(lex.next(), Some(Ok(Token::Set)));
            assert_eq!(lex.next(), Some(Ok(Token::Set)));
            assert_eq!(lex.next(), Some(Ok(Token::Set)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_show() {
            let mut lex = Token::lexer("SHOW Show show");

            assert_eq!(lex.next(), Some(Ok(Token::Show)));
            assert_eq!(lex.next(), Some(Ok(Token::Show)));
            assert_eq!(lex.next(), Some(Ok(Token::Show)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_table() {
            let mut lex = Token::lexer("TABLE Table table");

            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_tag() {
            let mut lex = Token::lexer("TAG Tag tag TAGS Tags tags");

            assert_eq!(lex.next(), Some(Ok(Token::Tag)));
            assert_eq!(lex.next(), Some(Ok(Token::Tag)));
            assert_eq!(lex.next(), Some(Ok(Token::Tag)));
            assert_eq!(lex.next(), Some(Ok(Token::Tag)));
            assert_eq!(lex.next(), Some(Ok(Token::Tag)));
            assert_eq!(lex.next(), Some(Ok(Token::Tag)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_time() {
            let mut lex = Token::lexer("TIME Time time TIMES Times times");

            assert_eq!(lex.next(), Some(Ok(Token::Time)));
            assert_eq!(lex.next(), Some(Ok(Token::Time)));
            assert_eq!(lex.next(), Some(Ok(Token::Time)));
            assert_eq!(lex.next(), Some(Ok(Token::Time)));
            assert_eq!(lex.next(), Some(Ok(Token::Time)));
            assert_eq!(lex.next(), Some(Ok(Token::Time)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn kw_to() {
            let mut lex = Token::lexer("TO To to");

            assert_eq!(lex.next(), Some(Ok(Token::To)));
            assert_eq!(lex.next(), Some(Ok(Token::To)));
            assert_eq!(lex.next(), Some(Ok(Token::To)));
            assert_eq!(lex.next(), None);
        }
    }
}
