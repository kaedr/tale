use logos::{Lexer, Logos};

fn die_roll(lex: &mut Lexer<Token>) -> Option<(usize, usize)> {
    let parts: Vec<_> = lex.slice().split("d").collect();
    println!("{:?}", parts);
    Some((
        parts.get(0)?.parse().ok().or(Some(1))?,
        parts.get(1)?.parse().ok()?,
    ))
}

fn digits(lex: &mut Lexer<Token>) -> Option<usize> {
    lex.slice().parse().ok()
}

fn verbatim(lex: &mut Lexer<Token>) -> Option<String> {
    Some(lex.slice().to_string())
}

#[rustfmt::skip]
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ ]+")]
pub enum Token {
    // General Tokens
    #[regex(r"\d*d\d+",die_roll)]       DieRoll((usize, usize)),
    #[regex(r"\d+",digits,priority=3)]  Digits(usize),
    #[regex(r"\w+",verbatim)]           Word(String),


    // Whitespace
    #[regex(r"[\f\n\r\v\u{2028}\u{2029}\u{85}]+")]
                                        NewLines,
    #[regex(r"\t+")]                    Tabs,

    // Symbols/Punctuation
    #[token("&")]                       Ampersand,
    #[token("*")]                       Asterisk,
    #[token("@")]                       At,
    #[token(r"\")]                      BackSlash,
    #[token("!")]                       Bang,
    #[token("|")]                       Bar,
    #[token("^")]                       Caret,
    #[token(":")]                       Colon,
    #[token(",")]                       Comma,
    #[token("\u{2013}")] // En Dash
    #[token("\u{2014}")] /* Em Dash */  Dash,
    #[token("$")]                       Dollar,
    #[token("\u{2026}")] // Horizontal Ellipsis
    #[token("..")] #[token("...")]      Ellipsis,
    #[token("=")]                       Equals,
    #[token("#")]                       Hash,
    #[token("-")] #[token("\u{2212}")]  Minus,
    #[token("%")]                       Percent,
    #[token(".")]                       Period,
    #[token("+")]                       Plus,
    #[token("?")]                       Question,
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

    // Quotes
    #[token("\u{FF02}")]
    #[token(r#"""#)]                    DQuote,
    #[token("`")]                       GQuote,
    #[token("'")]                       SQuote,
    #[token("\u{2018}")]                LeftSQuote,
    #[token("\u{201C}")]                LeftDQuote,
    #[token("\u{2019}")]                RightSQuote,
    #[token("\u{201D}")]                RightDQuote,

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

#[cfg(test)]
mod tests {

    #[cfg(test)]
    mod test_sample_files {
        use std::fs::read_to_string;

        use crate::Token;
        use logos::Logos;

        #[test]
        fn table_minimal() {
            let contents = read_to_string("../../samples/01_table_minimal.tale").unwrap();
            let mut lex = Token::lexer(&contents);

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
            let contents = read_to_string("../../samples/02_table_roll_def.tale").unwrap();
            let mut lex = Token::lexer(&contents);

            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::Colon)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("Attack".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::Roll)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Roll)));
            assert_eq!(lex.next(), Some(Ok(Token::Colon)));
            assert_eq!(lex.next(), Some(Ok(Token::DieRoll((1, 20)))));
            assert_eq!(lex.next(), Some(Ok(Token::Plus)));
            assert_eq!(lex.next(), Some(Ok(Token::Digits(7))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::End)));
            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn table_keyed_numeric() {
            let contents = read_to_string("../../samples/03_table_keyed_numeric.tale").unwrap();
            let mut lex = Token::lexer(&contents);

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
            let contents = read_to_string("../../samples/04_table_keyed_word.tale").unwrap();
            let mut lex = Token::lexer(&contents);

            assert_eq!(lex.next(), Some(Ok(Token::Table)));
            assert_eq!(lex.next(), Some(Ok(Token::Colon)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("TextKeys".into()))));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), Some(Ok(Token::Once)));
            assert_eq!(lex.slice(), "once");
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::Word("upon".into()))));
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
    }

    #[cfg(test)]
    mod test_general_tokens {
        use crate::Token;
        use logos::Logos;

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
            assert_eq!(lex.next(), Some(Ok(Token::Digits(8675309))));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn words() {
            let mut lex = Token::lexer(
                r#"This is some text:
                It spans three or four lines.
                Potato!
            "#,
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
            assert_eq!(lex.next(), Some(Ok(Token::Bang)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines)));
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn whitespace() {
            let mut lex = Token::lexer(
                "\t\t\u{0C}\t\u{0B}\t\t\t\n\n\t\r\n\t\n\r\t\r\t\t\u{2028}\t\u{2029}\t\u{85}",
            );

            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Form Feed
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Vertical tab (VT)
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Multiple line feeds (LF)
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Carriage Return Line Feed (CRLF)
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Line Feed Carriage Return (LFCR)
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Carriage Return (CR)
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Line Separator (LS)
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Page Separator (PS)
            assert_eq!(lex.next(), Some(Ok(Token::Tabs)));
            assert_eq!(lex.next(), Some(Ok(Token::NewLines))); // Next Line (NEL)
            assert_eq!(lex.next(), None);
        }
    }

    #[cfg(test)]
    mod test_symbols {
        use crate::Token;
        use logos::Logos;

        #[test]
        fn sym_individuals() {
            let mut lex = Token::lexer(r"~!@#$%^&*_+=|\:;,.?/");

            assert_eq!(lex.next(), Some(Ok(Token::Tilde)));
            assert_eq!(lex.next(), Some(Ok(Token::Bang)));
            assert_eq!(lex.next(), Some(Ok(Token::At)));
            assert_eq!(lex.next(), Some(Ok(Token::Hash)));
            assert_eq!(lex.next(), Some(Ok(Token::Dollar)));
            assert_eq!(lex.next(), Some(Ok(Token::Percent)));
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
            assert_eq!(lex.next(), Some(Ok(Token::Comma)));
            assert_eq!(lex.next(), Some(Ok(Token::Period)));
            assert_eq!(lex.next(), Some(Ok(Token::Question)));
            assert_eq!(lex.next(), Some(Ok(Token::Slash)));
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
            let mut lex = Token::lexer(r#"<[{()}]>"#);

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

        #[test]
        fn sym_quotes() {
            let mut lex = Token::lexer(r#"‘“"`'”’"#);

            assert_eq!(lex.next(), Some(Ok(Token::LeftSQuote)));
            assert_eq!(lex.next(), Some(Ok(Token::LeftDQuote)));
            assert_eq!(lex.next(), Some(Ok(Token::DQuote)));
            assert_eq!(lex.next(), Some(Ok(Token::GQuote)));
            assert_eq!(lex.next(), Some(Ok(Token::SQuote)));
            assert_eq!(lex.next(), Some(Ok(Token::RightDQuote)));
            assert_eq!(lex.next(), Some(Ok(Token::RightSQuote)));
            assert_eq!(lex.next(), None);
        }
    }

    #[cfg(test)]
    mod test_keywords {
        use crate::Token;
        use logos::Logos;

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
