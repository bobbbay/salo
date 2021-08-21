//! An implementation of the Salo standard in Rust.

#![feature(format_args_capture, crate_visibility_modifier, test)]
// [TODO] I would like to deny missing_docs too, but LALRPOP does not write docs to generated files.
#![deny(unsafe_code, unused_import_braces)]

#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate typestate;

pub mod ast;
pub mod util;

lalrpop_mod!(pub parser);

use crate::parser::SaloParser;
use ariadne::{ColorGenerator, Label, Report, ReportKind};
use lalrpop_util::ParseError;
use tracing::{error, info, warn};

#[cfg(test)]
mod tests {
    extern crate test;
    use color_eyre::Result;
    use test::Bencher;

    use crate::ast::*;
    use crate::util::Code;

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

    #[test]
    fn typestate_test() {
        use crate::compiler::*;

        let parser = Parser::<Source>::new("<stdin>", "a : Str;");

        let ast = match parser.parse() {
            MaybeAST::AST(ast) => ast,
            MaybeAST::Error(error) => {
                error.report();
            }
        };

        let output = match ast.evaluate() {
            MaybeOutput::Output(output) => output,
            MaybeOutput::Error(error) => error.report(),
        };

        output.export();
    }
}

#[typestate(enumerate)]
pub mod compiler {
    //! Compiler for the Salo language (enforced by typestate)
    use ariadne::Report;

    use crate::ast::Expr;

    #[automaton]
    #[derive(Debug)]
    pub struct Parser {
        pub filename: &'static str,
        pub content: &'static str,
    }

    #[state]
    pub struct Source;
    #[state]
    #[derive(Debug)]
    pub struct AST(pub Vec<Expr<'static>>);
    #[state]
    pub struct Output(pub &'static str);
    #[state]
    pub struct Error(pub Report);

    pub enum MaybeAST {
        #[metadata(label = "Parsed into AST successfully.")]
        AST,
        #[metadata(label = "Failed to parse into AST.")]
        Error,
    }

    pub enum MaybeOutput {
        #[metadata(label = "Evaluated succesfully.")]
        Output,
        #[metadata(label = "Failed to evaluate.")]
        Error,
    }

    pub trait Source {
        /// Initial state.
        fn new(filename: &'static str, content: &'static str) -> Source;

        /// Turn [`Source`] into an [`AST`].
        fn parse(self) -> MaybeAST;
    }

    pub trait AST {
        /// Turn [`AST`] into an [`Output`].
        fn evaluate(self) -> MaybeOutput;
    }

    pub trait Output {
        fn export(self) -> &'static str;
    }

    pub trait Error {
        fn report(self) -> !;
    }
}

use crate::compiler::*;
impl SourceState for Parser<Source> {
    fn new(filename: &'static str, content: &'static str) -> compiler::Parser<compiler::Source> {
        Parser {
            state: Source,
            filename,
            content,
        }
    }

    fn parse(self) -> MaybeAST {
        let expr = SaloParser::new().parse(self.content);

        match expr {
            Ok(expr) => {
                info!("Succesfully parsed");
                return MaybeAST::AST(Parser {
                    filename: self.filename,
                    content: self.content,
                    state: AST(expr),
                })
            }
            Err(e) => {
                warn!("Encountered error, will abort soon");

                let _colors = ColorGenerator::new();

                error!("{:#?}", &e);

                let report = match &e {
                    ParseError::InvalidToken { location } => {
                        Report::build(ReportKind::Error, (), *location)
                            .with_code(1)
                            .with_message("Encountered invalid token")
                            .with_label(
                                Label::new(*location..*location + 1)
                                    .with_message("I don't know what this character is"),
                            )
                    }
                    ParseError::UnrecognizedEOF { location, expected } => {
                        Report::build(ReportKind::Error, (), *location)
                            .with_code(3)
                            .with_message("Encountered unexpected end-of-file")
                            .with_label(
                                Label::new(*location..*location + 1)
                                    .with_message("Why did you stop here?"),
                            )
                            .with_note(format!("Expected one of: {}", vec_str(expected)))
                    }
                    ParseError::UnrecognizedToken { token, expected } => {
                        Report::build(ReportKind::Error, (), token.0)
                            .with_code(2)
                            .with_message("Encountered unrecognized token")
                            .with_label(
                                Label::new(token.0..token.2)
                                    .with_message("I don't know what this is"),
                            )
                            .with_note(format!("Expected {}", vec_str(expected)))
                    }
                    ParseError::ExtraToken { token } => {
                        Report::build(ReportKind::Error, (), token.0)
                            .with_code(3)
                            .with_message("Encountered extra token")
                            .with_label(Label::new(token.0..token.2))
                    }
                    ParseError::User { error } => {
                        Report::build(ReportKind::Error, (), 0)
                            .with_code(9999)
                            .with_message(format!("Unspecified error {}", error))
                    }
                };

                return MaybeAST::Error(Parser {
                    filename: self.filename,
                    content: self.content,
                    state: Error(report.finish())
                })
            }
        }
    }
}

impl ASTState for Parser<AST> {
    fn evaluate(self) -> MaybeOutput {
        MaybeOutput::Output(Parser {
            filename: self.filename,
            content: self.content,
            state: Output("A"),
        })
    }
}

impl OutputState for Parser<Output> {
    fn export(self) -> &'static str {
        "A"
    }
}

impl ErrorState for Parser<Error> {
    fn report(self) -> ! {
        self.state
            .0
            .eprint(ariadne::Source::from(self.content))
            .unwrap();
        std::process::exit(0);
    }
}

/// [HACK] Format a vec so that it's easier on our eyes.
fn vec_str(v: &Vec<String>) -> String {
    let mut res = String::new();

    for i in &v[0..v.len() - 1] {
        res.push_str(i);
        res.push_str(", ");
    }
    res.push_str(&v[v.len() - 1].to_string());

    res
}
