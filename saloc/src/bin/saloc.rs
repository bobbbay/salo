//! Executable for saloc

#![feature(format_args_capture)]
#![deny(unsafe_code, unused_import_braces)]

use crate::repl::repl;

use clap::{load_yaml, App};
use color_eyre::Help;
use color_eyre::{eyre::WrapErr, Result};
use tracing::info;

fn main() -> Result<()> {
    setup()?;

    let yaml = load_yaml!("../../cli.yaml");
    let matches = App::from(yaml).get_matches();

    match matches.value_of("FILE") {
        Some(filename) => {
            todo!();

            let content = std::fs::read_to_string(filename)
                .wrap_err("Unable to read config")
                .suggestion("Try using a file that exists")?;

            info!("Evaluating file {}", filename);

            use saloc::compiler::*;

            let parser = Parser::<Source>::new("<stdin>", &content);

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

            info!("Finished evaluation");
        }
        None => repl()?,
    }

    Ok(())
}

/// Enables/disables logging hooks and levels depending on the value of `SALO_LOG`.
///  "0" | anything else: don't log, errors provide an issue URL.
///  "1": Short logs
///  "2": Long logs
pub fn setup() -> Result<()> {
    let loglevel = std::env::var("SALO_LOG").unwrap_or("0".to_string());
    let hook = color_eyre::config::HookBuilder::blank();

    let hook = match &*loglevel {
        "1" => {
            std::env::set_var("RUST_BACKTRACE", "1");
            std::env::set_var("RUST_LOG", "info");

            tracing_subscriber::fmt::fmt()
                .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
                .init();

            hook
        }
        "2" => {
            std::env::set_var("RUST_BACKTRACE", "full");
            std::env::set_var("RUST_LOG", "full");
            std::env::set_var("COLORBT_SHOW_HIDDEN", "1");

            tracing_subscriber::fmt::fmt()
                .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
                .init();

            hook
        }
        _ => hook
            .issue_url(concat!(env!("CARGO_PKG_REPOSITORY"), "/issues/new"))
            .display_env_section(false),
    };

    hook.install()?;

    info!("Setup complete");
    Ok(())
}

pub mod repl {
    use color_eyre::Result;
    use saloc::util::Code;

    /// Invokes the Linefeed REPL.
    pub fn repl() -> Result<()> {
        todo!();
        
        let interface = Arc::new(Interface::new("salo-repl").unwrap());

        println!("Dropping to the REPL");

        interface.set_completer(Arc::new(SaloCompleter));
        interface.set_prompt("> ").unwrap();

        while let ReadResult::Input(line) = interface.read_line().unwrap() {
            if !line.trim().is_empty() {
                interface.add_history_unique(line.clone());
            }

            let line = line.trim();

            let (cmd, args) = match line.find(|ch: char| ch.is_whitespace()) {
                Some(pos) => (&line[..pos], line[pos..].trim_start()),
                None => (line, ""),
            };

            split_first_word(&line);

            match cmd {
                ":help" | ":h" => {
                    println!("Salo REPL-specific commands:");
                    println!();
                    for &(cmd, help) in COMMANDS {
                        println!("  {:15} - {}", cmd, help);
                    }
                    println!();
                }
                ":type" | ":t" => {
                    unimplemented!()
                }
                ":ast" | ":a" => {
                    use saloc::compiler::*;

                    let parser = Parser::<Source>::new("<stdin>", args);

                    let ast = match parser.parse() {
                        MaybeAST::AST(ast) => ast,
                        MaybeAST::Error(error) => {
                            error.report();
                        }
                    };

                    println!("{:#?}", ast.state.0);
                }
                ":quit" | ":q" => break,
                _ => {
                    let mut code = Code::new(&line, "stdin");
                }
            }
        }

        println!("Goodbye.");
        Ok(())
    }

    use linefeed::complete::{Completer, Completion};
    use linefeed::terminal::Terminal;
    use linefeed::{Interface, Prompter, ReadResult};
    use std::sync::Arc;

    static COMMANDS: &[(&str, &str)] = &[
        (":h", "You're looking at it"),
        (":t <expr>", "Get the type of <expr>"),
        (":a <expr>", "Get the generated AST of <expr>"),
        (":q", "Goodbye"),
    ];

    /// [HACK] Splits the first word
    fn split_first_word(s: &str) -> (&str, &str) {
        let s = s.trim();

        match s.find(|ch: char| ch.is_whitespace()) {
            Some(pos) => (&s[..pos], s[pos..].trim_start()),
            None => (s, ""),
        }
    }

    /// Default completion for the language.
    struct SaloCompleter;

    impl<Term: Terminal> Completer<Term> for SaloCompleter {
        fn complete(
            &self,
            word: &str,
            prompter: &Prompter<Term>,
            start: usize,
            _end: usize,
        ) -> Option<Vec<Completion>> {
            let line = prompter.buffer();

            let mut words = line[..start].split_whitespace();

            match words.next() {
                // Complete command name
                None => {
                    let mut compls = Vec::new();

                    for &(cmd, _) in COMMANDS {
                        if cmd.starts_with(word) {
                            compls.push(Completion::simple(cmd.to_owned()));
                        }
                    }

                    Some(compls)
                }
                _ => None,
            }
        }
    }
}
