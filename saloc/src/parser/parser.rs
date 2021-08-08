use crate::parser::language::SaloParser;
use crate::util::Code;
use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use color_eyre::eyre::{eyre, Result};
use lalrpop_util::ParseError;
use tracing::{error, info, warn};

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

impl<'life> Code<'life> {
    /// Parse code
    crate fn parse(&mut self) -> Result<()> {
        let expr = SaloParser::new().parse(self.content);

        match expr {
            Ok(expr) => {
                info!("Succesfully parsed");
                self.ast = Some(expr);
                Ok(())
            }
            Err(e) => {
                warn!("Encountered error, will abort soon");

                let _colors = ColorGenerator::new();

                error!("{:#?}", &e);

                let report = match &e {
                    ParseError::InvalidToken { location } => {
                        Report::build(ReportKind::Error, self.filename, *location)
                            .with_code(1)
                            .with_message("Encountered invalid token")
                            .with_label(
                                Label::new((self.filename, *location..*location + 1))
                                    .with_message("I don't know what this character is"),
                            )
                    }
                    ParseError::UnrecognizedEOF { location, expected } => {
                        Report::build(ReportKind::Error, self.filename, *location)
                            .with_code(3)
                            .with_message("Encountered unexpected end-of-file")
                            .with_label(
                                Label::new((self.filename, *location..*location + 1))
                                    .with_message("Why did you stop here?"),
                            )
                            .with_note(format!("Expected one of: {}", vec_str(expected)))
                    }
                    ParseError::UnrecognizedToken { token, expected } => {
                        Report::build(ReportKind::Error, self.filename, token.0)
                            .with_code(2)
                            .with_message("Encountered unrecognized token")
                            .with_label(
                                Label::new((self.filename, token.0..token.2))
                                    .with_message("I don't know what this is"),
                            )
                            .with_note(format!("Expected {}", vec_str(expected)))
                    }
                    ParseError::ExtraToken { token } => {
                        Report::build(ReportKind::Error, self.filename, token.0)
                            .with_code(3)
                            .with_message("Encountered extra token")
                            .with_label(Label::new((self.filename, token.0..token.2)))
                    }
                    ParseError::User { error } => {
                        Report::build(ReportKind::Error, self.filename, 0)
                            .with_code(9999)
                            .with_message(format!("Unspecified error {}", error))
                    }
                };

                report
                    .finish()
                    .print((self.filename, Source::from(self.content)))?;

                return Err(eyre!("An error was encountered during parsing"));
            }
        }
    }
}
