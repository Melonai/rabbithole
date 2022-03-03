use crate::{
    interpret::walker::WalkerError,
    lex::token::Location,
    parse::parser::{ParserError, ParserErrorLocation},
};
use colored::Colorize;
use std::fmt::Display;

pub enum RHError {
    Parse(ParserError),
    Run(WalkerError),
}

impl RHError {
    pub fn hydrate_source<'s>(self, source: &'s str) -> RHErrorWithSource<'s> {
        RHErrorWithSource {
            error: self,
            source,
        }
    }
}

pub struct RHErrorWithSource<'source> {
    error: RHError,
    source: &'source str,
}

impl Display for RHErrorWithSource<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (error_title, error_string) = match &self.error {
            RHError::Parse(parser_error) => (
                "Encountered error during parsing",
                format!("{}", parser_error.kind),
            ),
            RHError::Run(walker_error) => (
                "Encountered runtime error",
                format!("{}", walker_error.kind),
            ),
        };

        let at = MappedErrorLocation::new(self);

        let arrow_pad_len = at.line_column + 3 + at.line_number.to_string().len();

        writeln!(f, "{} {}:\n", " Error ".on_red(), error_title)?;
        writeln!(
            f,
            "{}{}",
            format!("{} | ", at.line_number).dimmed(),
            at.source_line
        )?;
        writeln!(f, "{}{}", " ".repeat(arrow_pad_len), "^".red())?;
        writeln!(f, "{}", error_string)?;

        Ok(())
    }
}

pub struct MappedErrorLocation<'s> {
    line_number: usize,
    line_column: usize,
    source_line: &'s str,
}

impl<'s> MappedErrorLocation<'s> {
    const SOURCE_MISSING_ERROR_LOCATION: &'static str =
        "Found error but could not get it's location in source, how does this even happen?";

    fn new(from: &'s RHErrorWithSource<'s>) -> MappedErrorLocation<'s> {
        match &from.error {
            RHError::Parse(error) => match &error.at {
                ParserErrorLocation::Specific(at) => Self::at_location(from, at),
                ParserErrorLocation::Eof => Self::last_line(from),
            },
            RHError::Run(error) => Self::at_location(from, &error.at),
        }
    }

    fn last_line(from: &'s RHErrorWithSource<'s>) -> MappedErrorLocation<'s> {
        let lines = from.source.lines();
        let (line_count, _) = lines.size_hint();
        let last_line = lines.last().expect(Self::SOURCE_MISSING_ERROR_LOCATION);

        MappedErrorLocation {
            line_number: line_count,
            line_column: last_line.len(),
            source_line: last_line,
        }
    }

    fn at_location(
        from: &'s RHErrorWithSource<'s>,
        location: &'s Location,
    ) -> MappedErrorLocation<'s> {
        MappedErrorLocation {
            line_number: location.row,
            line_column: location.col,
            source_line: from
                .source
                .lines()
                .nth(location.row)
                .expect(Self::SOURCE_MISSING_ERROR_LOCATION),
        }
    }
}
