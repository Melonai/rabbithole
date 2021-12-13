use crate::{interpret::walker::WalkerError, lex::token::Location, parse::parser::ParserError};
use colored::Colorize;
use std::fmt::Display;

pub struct RHError {
    pub at: ErrorLocation,
    pub kind: RHErrorKind,
}

impl RHError {
    pub fn hydrate_source<'s>(self, source: &'s str) -> RHErrorWithSource<'s> {
        RHErrorWithSource {
            error: self,
            source,
        }
    }
}

pub enum RHErrorKind {
    Parse(ParserError),
    Run(WalkerError),
}

pub enum ErrorLocation {
    Specific(Location),
    Eof,
}

pub struct RHErrorWithSource<'source> {
    error: RHError,
    source: &'source str,
}

impl Display for RHErrorWithSource<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (error_title, error_string) = match &self.error.kind {
            RHErrorKind::Parse(parser_error) => (
                "Encountered error during parsing",
                format!("{}", parser_error),
            ),
            RHErrorKind::Run(walker_error) => {
                ("Encountered runtime error", format!("{}", walker_error))
            }
        };

        let weird_source_error_msg =
            "Found error but could not get it's location in source, how does this even happen?";

        let (line_number, line_column, source_line) = match self.error.at {
            ErrorLocation::Specific(location) => (
                location.row,
                location.col,
                self.source
                    .lines()
                    .nth(location.row)
                    .expect(weird_source_error_msg),
            ),
            ErrorLocation::Eof => {
                let lines = self.source.lines();
                let (line_count, _) = lines.size_hint();
                let last_line = lines.last().expect(weird_source_error_msg);

                (line_count, last_line.len(), last_line)
            }
        };

        let arrow_pad_len = line_column + 3 + line_number.to_string().len();

        writeln!(f, "{} {}:\n", " Error ".on_red(), error_title)?;
        writeln!(
            f,
            "{}{}",
            format!("{} | ", line_number).dimmed(),
            source_line
        )?;
        writeln!(f, "{}{}", " ".repeat(arrow_pad_len), "^".red())?;
        writeln!(f, "{}", error_string)?;

        Ok(())
    }
}
