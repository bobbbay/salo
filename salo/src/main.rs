//! An implementation of the Salo standard in Rust.

#![feature(format_args_capture)]
#![feature(crate_visibility_modifier)]
// [TODO] I would like to deny missing_docs too, but LALRPOP does not write docs to generated files.
#![deny(unsafe_code, unused_import_braces)]

#[macro_use] extern crate lalrpop_util;

mod ast;
mod parser;
mod repl;
mod util;
lalrpop_mod!(pub salo);

use clap::{load_yaml, App};
use tracing::{error, info};

use crate::util::{setup, Code, Result};
use crate::parser::parse;
use crate::repl::repl;

fn main() -> Result<()> {
    setup()?;

    let yaml = load_yaml!("../cli.yaml");
    let matches = App::from(yaml).get_matches();

    match matches.subcommand() {
        Some(("eval", subcommands)) => {
            let filename = subcommands.value_of("FILE").unwrap();
            let content = std::fs::read_to_string(filename);

            let content = match content {
                Ok(content) => content,
                Err(_) => {
                    eprintln!("Error: File not found.");
                    error!("File not found");
                    std::process::exit(1);
                }
            };

            info!("Evaluating file {}", filename);

            let code = Code::new(&content, "stdin");
            let _ast = parse(code)?;

            info!("Finished evaluation");
        }
        _ => repl(),
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use crate::parser::parse;

    #[test]
    fn parse_simple_1() -> Result<()> {
        setup()?;

        let content = r#"
        1 + 1;
        2 - 1;
        "#;

        let code = Code::new(content, "stdin");

        let res = parse(code)?;

        assert_eq!(
            res,
            [
                Node::BinaryExpr {
                    op: BinaryOp::Add,
                    lhs: Box::new(Node::Value(Type::Int(Some(1)))),
                    rhs: Box::new(Node::Value(Type::Int(Some(1)))),
                },
                Node::BinaryExpr {
                    op: BinaryOp::Sub,
                    lhs: Box::new(Node::Value(Type::Int(Some(2)))),
                    rhs: Box::new(Node::Value(Type::Int(Some(1)))),
                },
            ]
        );

        Ok(())
    }
}
