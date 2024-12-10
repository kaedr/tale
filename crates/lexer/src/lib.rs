use logos::{Logos, Skip};

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\f]+")]
enum Token {
    // Newline character
    #[token("\n")]
    Newline,

    // The Numeric type
    #[regex(r"\d+[_\d]*\.?[_\d]*", |lex| lex.slice().replace("_", "").parse().ok())]
    Numeric(f64),

    // An inline comment
    #[regex(r"#.*", |_| Skip)]
    Comment,
}

#[cfg(test)]
mod tests {
    use super::*;

    mod test_numerics {
        use super::*;
        #[test]
        fn simple_integers() {
            let mut lex = Token::lexer("9 8 7 6 5 4 3 2 1 0");

            assert_eq!(lex.next(), Some(Ok(Token::Numeric(9.0))));
            assert_eq!(lex.next(), Some(Ok(Token::Numeric(8.0))));
            assert_eq!(lex.next(), Some(Ok(Token::Numeric(7.0))));
            assert_eq!(lex.next(), Some(Ok(Token::Numeric(6.0))));
            assert_eq!(lex.next(), Some(Ok(Token::Numeric(5.0))));
            assert_eq!(lex.next(), Some(Ok(Token::Numeric(4.0))));
            assert_eq!(lex.next(), Some(Ok(Token::Numeric(3.0))));
            assert_eq!(lex.next(), Some(Ok(Token::Numeric(2.0))));
            assert_eq!(lex.next(), Some(Ok(Token::Numeric(1.0))));
            assert_eq!(lex.next(), Some(Ok(Token::Numeric(0.0))));
        }

        #[test]
        fn integers_with_underscores() {
            let mut lex = Token::lexer("525_600");

            assert_eq!(lex.next(), Some(Ok(Token::Numeric(525600.0))));
        }

        #[test]
        fn simple_floats() {
            let mut lex = Token::lexer("1.0 2.");
            assert_eq!(lex.next(), Some(Ok(Token::Numeric(1.0))));
            assert_eq!(lex.next(), Some(Ok(Token::Numeric(2.0))));
        }
    }

    mod test_comments {
        use super::*;
        #[test]
        fn simple_comment() {
            let mut lex = Token::lexer("# This is a comment!");
            // Because comments should be ignored, we should see nothing from the lexer
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn comment_ignores_numeric() {
            let mut lex = Token::lexer("# 8675309");
            assert_eq!(lex.next(), None);
        }

        #[test]
        fn multiline_comment() {
            let mut lex = Token::lexer("# This is a comment!\n# And another!");
            assert_eq!(lex.next(), Some(Ok(Token::Newline)));
            assert_eq!(lex.next(), None);
        }
    }
}
