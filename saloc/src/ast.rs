use std::fmt::Debug;

#[derive(Debug, PartialEq)]
pub enum Expr<'life> {
    Var {
        name: Ident<'life>,
        t: Option<Type>,
        value: Option<Box<Value<'life>>>,
    },

    Value(Value<'life>),

    Skip(),
}

#[derive(Debug, PartialEq)]
pub struct Ident<'life>(pub &'life str);

#[derive(Debug, PartialEq)]
pub enum Type {
    Bool,
    Num,
    Str,
    Fn,
}

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
