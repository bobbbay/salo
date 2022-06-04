mod error;
mod token;

use error::LexerError as Error;
pub use logos::{Logos, SpannedIter};
use token::Token;

pub fn lex(string: &str) -> Result<SpannedIter<Token>, Error> {
    Ok(Token::lexer(string).spanned())
}
