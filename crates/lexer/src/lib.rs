use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ ]+")]
enum Token {
    // Symbols/Punctuation
    #[token("&")]                       Ampersand,
    #[token("*")]                       Asterisk,
    #[token("@")]                       At,
    #[token("\\")]                      BackSlash,
    #[token("!")]                       Bang,
    #[token("|")]                       Bar,
    #[token("^")]                       Caret,
    #[token(":")]                       Colon,
    #[token("-")]                       Dash,
    #[token("$")]                       Dollar,
    #[token("..")] #[token("...")]      Ellipsis,
    #[token("=")]                       EquaLS,
    #[token("#")]                       Hash,
    #[token("%")]                       Percent,
    #[token(".")]                       Period,
    #[token("+")]                       Plus,
    #[token("?")]                       Question,
    #[token(";")]                       SemiColon,
    #[token("/")]                       Slash,
    #[token("~")]                       Tilde,
    #[token("_")]                       Underscore,

    // Brackets/Braces

    // Quotes

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
    mod test_symbols {
        #[test]
        fn sym_hash () {

        }
    }

    #[cfg(test)]
    mod test_keywords {
        use logos::Logos;

        use crate::Token;

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
