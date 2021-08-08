use std::fmt::Debug;

/// A whole expression. An AST is usually a `Vec` of these.
#[derive(Debug, PartialEq)]
pub enum Expr<'life> {
    /// A variable declaration
    Var {
        name: Ident<'life>,
        t: Option<Type>,
        value: Option<Box<Value<'life>>>,
    },

    /// A value (see: [`Value`])
    Value(Value<'life>),

    /// Nothing
    Skip(),
}

/// Any identifier, with its content in the tuple.
#[derive(Debug, PartialEq)]
pub struct Ident<'life>(pub &'life str);

/// Type names.
#[derive(Debug, PartialEq)]
pub enum Type {
    Bool,
    Num,
    Str,
    Fn,
}

/// A value. Can be seen as an "implementation" of each type name (see [`Type`]).
#[derive(Debug, PartialEq)]
pub enum Value<'life> {
    Bool(bool),
    Num(i32),
    Str(&'life str),

    Fn {
        name: Ident<'life>,
        args: Option<Vec<Type>>,
        value: Option<Type>,

        matches: Option<Vec<Ident<'life>>>,
        body: Option<Box<Expr<'life>>>,
    },
}
