use std::{collections::HashMap, path::PathBuf};

#[derive(Debug, PartialEq)]
pub enum Type {
    Bool(Option<bool>),
    Str(Option<String>),
    Int(Option<i32>),
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Import(PathBuf),   // import ./profiles/dev.nix
    ImportLib(String), // import std.rustPlatform.buildRustPackage

    Declaration(String, Box<Node>), // pname = "ripgrep"

    Derivation {
        name: String,
        content: Box<Node>,
    },

    AttrSet(HashMap<String, Box<Node>>),

    Value(Type),

    BinaryExpr {
        op: BinaryOp,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },

    Comment(String),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add, // x + y
    Sub, // x - y
}
