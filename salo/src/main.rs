#![feature(format_args_capture)]
#![feature(crate_visibility_modifier)]

#[macro_use]
extern crate lalrpop_util;

mod ast;
mod parser;
mod repl;
mod util;
lalrpop_mod!(pub salo);

use crate::repl::repl;
use crate::util::Result;
use clap::{load_yaml, App};
use color_eyre::owo_colors::OwoColorize;
use tracing::{error, info};

fn setup() -> Result<()> {
    if std::env::var("SALO_LOG").unwrap_or("0".to_string()) == "1" {
        if std::env::var("RUST_BACKTRACE").is_err() {
            std::env::set_var("RUST_BACKTRACE", "1")
        }
        color_eyre::install()?;

        if std::env::var("RUST_LOG").is_err() {
            std::env::set_var("RUST_LOG", "info")
        }
        tracing_subscriber::fmt::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .init();
    } else {
        // We're in a non-debug environment
        color_eyre::config::HookBuilder::default()
            .issue_url(concat!(env!("CARGO_PKG_REPOSITORY"), "/issues/new"))
            .add_issue_metadata("version", env!("CARGO_PKG_VERSION"))
            .panic_section(format!("{}", "This is a compiler error.".red()))
            .install()?;
    }

    info!("Setup complete");
    Ok(())
}

#[derive(Debug)]
crate struct Code<'a> {
    content: &'a str,
    filename: &'a str,
}

impl<'a> Code<'a> {
    crate fn new(code: &'a str, filename: &'a str) -> Self {
        Self {
            content: code,
            filename,
        }
    }
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

use crate::parser::parse;

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
        Some(("remote", _subcommands)) => {
            println!("[TODO]");
        }
        _ => repl(),
    }

    Ok(())
}
