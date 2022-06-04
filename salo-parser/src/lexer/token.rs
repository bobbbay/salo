use super::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[token("module")]
    Module,

    #[error]
    Error,
}
