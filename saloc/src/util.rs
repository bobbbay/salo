//! Crate-wide important structs.

use crate::ast::Expr;

/// Struct that contains code.
#[derive(Debug)]
pub struct Code<'life> {
    pub content: &'life str,
    pub filename: &'life str,

    pub ast: Option<Vec<Expr<'life>>>,
}

impl<'life> Code<'life> {
    pub fn new(code: &'life str, filename: &'life str) -> Self {
        Self {
            content: code,
            filename,
            ast: None,
        }
    }
}
