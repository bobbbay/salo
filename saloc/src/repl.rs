use color_eyre::eyre::Result;
use crate::parser::parse;
use crate::Code;
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

crate fn repl() -> Result<()> {
    let interface = Arc::new(Interface::new("salo-repl").unwrap());

    println!("Dropping to the REPL");

    interface.set_completer(Arc::new(SaloCompleter));
    interface.set_prompt("> ").unwrap();

    while let ReadResult::Input(line) = interface.read_line().unwrap() {
        if !line.trim().is_empty() {
            interface.add_history_unique(line.clone());
        }

        let (cmd, args) = split_first_word(&line);

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
                let code = Code::new(args, "stdin");
                println!("{:#?}", parse(code).unwrap());
            }
            ":quit" | ":q" => break,
            _ => {
                let code = Code::new(&line, "stdin");
                parse(code);
            }
        }
    }

    println!("Goodbye.");
    Ok(())
}

fn split_first_word(s: &str) -> (&str, &str) {
    let s = s.trim();

    match s.find(|ch: char| ch.is_whitespace()) {
        Some(pos) => (&s[..pos], s[pos..].trim_start()),
        None => (s, ""),
    }
}

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
