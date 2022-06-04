#![doc = include_str!("../README.md")]

pub mod lexer;
pub mod parser;

mod prelude {
    pub use crate::lexer::lex;
    pub use crate::parser::parse;

    const fn lex_after_parse(source: &str) {}
}
