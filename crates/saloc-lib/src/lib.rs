//! An implementation of the Salo standard in Rust.

#![feature(format_args_capture, crate_visibility_modifier, test)]
// [TODO] I would like to deny missing_docs too, but LALRPOP does not write docs to generated files.
#![deny(unsafe_code, unused_import_braces)]

#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate typestate;

pub mod ast;
pub mod compiler;
pub mod util;

lalrpop_mod!(pub parser);

#[cfg(test)]
mod tests {
    extern crate test;
    use color_eyre::Result;
    use test::Bencher;

    use crate::ast::*;
    use crate::compiler::compiler::*;

    #[bench]
    /// Test the parsing ([`Source`] -> [`AST`]) capability.
    fn parse_1(b: &mut Bencher) -> Result<()> {
        b.iter(|| -> Result<()> {
            let content = r#"
            description : Str;
            description = "A simple example of Salo's syntax";
            "#;

            let parser = Parser::<Source>::new("<testin>", content);

            let ast = match parser.parse() {
                MaybeAST::AST(ast) => ast,
                MaybeAST::Error(error) => {
                    error.report();
                }
            };

            assert_eq!(
                ast.state.0,
                [
                    Expr::Var {
                        name: Ident("description",),
                        t: Some(Type::Str),
                        value: None,
                    },
                    Expr::Var {
                        name: Ident("description",),
                        t: None,
                        value: Some(Box::new(Value::Str(
                            "\"A simple example of Salo's syntax\""
                        ))),
                    },
                ]
            );

            Ok(())
        });

        Ok(())
    }
}
