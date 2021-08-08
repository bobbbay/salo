//! An implementation of the Salo standard in Rust.

#![feature(format_args_capture, crate_visibility_modifier, test)]
// [TODO] I would like to deny missing_docs too, but LALRPOP does not write docs to generated files.
#![deny(unsafe_code, unused_import_braces)]

#[macro_use]
extern crate lalrpop_util;

mod ast;
mod parser;
mod repl;
mod util;

use clap::{load_yaml, App};
use color_eyre::{Result, eyre::WrapErr};
use color_eyre::Help;
use tracing::info;

use crate::repl::repl;
use crate::util::{setup, Code};

fn main() -> Result<()> {
    setup()?;

    let yaml = load_yaml!("../cli.yaml");
    let matches = App::from(yaml).get_matches();

    match matches.value_of("FILE") {
        Some(filename) => {
            let content = std::fs::read_to_string(filename)
                .wrap_err("Unable to read config")
                .suggestion("Try using a file that exists")?;

            info!("Evaluating file {}", filename);

            let mut code = Code::new(&content, "stdin");
            code.parse()?;

            info!("Finished evaluation");
        }
        None => repl()?,
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    extern crate test;
    use test::Bencher;

    use super::*;
    use crate::ast::*;

    #[bench]
    fn parse_1(b: &mut Bencher) -> Result<()> {
        b.iter(|| -> Result<()> {
            let content = r#"
            description : Str;
            description = "A simple example of Salo's syntax";
            "#;

            let mut code = Code::new(content, "stdin");
            code.parse()?;

            assert_eq!(
                code.ast.unwrap(),
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
