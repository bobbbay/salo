mod error;
mod token;

use error::LexerError as Error;
pub use logos::{Logos, SpannedIter};
use token::Token;

pub fn lex(string: &str) -> Result<SpannedIter<Token>, Error> {
    Ok(Token::lexer(string).spanned())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keywords() {
        use super::token::Token;

        let mut text = lex("unknown_keyword module").unwrap();

        for x in 0..16 {
            assert_eq!(text.next(), Some((Token::Error, (x)..(x + 1))));
        }
        assert_eq!(text.next(), Some((Token::Module, 16..22)));
    }
}
