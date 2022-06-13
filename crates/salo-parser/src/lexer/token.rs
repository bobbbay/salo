use super::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token("module")]
    Module,

    #[regex(r"---[^\n]*")]
    DocComment,

    #[regex(r"--[^\n]*", logos::skip)]
    Comment,

    #[regex(r"[ \t\n\r]", logos::skip)]
    Whitespace,

    #[error]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keywords() {
        let mut text = Token::lexer("unknown_keyword module");

        for _ in 0..15 {
            assert_eq!(text.next(), Some(Token::Error));
        }
        assert_eq!(text.next(), Some(Token::Module));
    }

    #[test]
    fn comments() {
        let mut text = Token::lexer(
            "-- Hello, world! This is a comment.
            module -- the previous word is not commented, but this is.",
        );

        assert_eq!(text.next(), Some(Token::Module));
        assert_eq!(text.next(), None);
    }

    #[test]
    fn full_syntax() {
        let _text = Token::lexer(include_str!("../../../examples/syntax.sl"));

        // TODO: fill in
    }
}
