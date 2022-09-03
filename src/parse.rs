//! Parsing wscript.

use crate::ast::{self, Span, Program};
use crate::lex::{Token, TokenError};
use crate::error;

use std::{fmt, path};

pub fn parse(source: &str, source_id: usize) -> Result<Program, ParseError> {
    Err(ParseError {
        kind: ParseErrorKind::OhGolly,
        span: (source_id, 0..source.len()),
    })
}

#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    span: Span,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    LexError(TokenError),
    OhGolly,
}

impl ParseError {
    pub fn report(&self) -> error::Report {
        use ariadne::{Report, ReportKind};

        let (source_id, range) = self.span.clone();
        let mut builder = Report::build(ReportKind::Error, source_id, range.start);
        match self.kind {
            ParseErrorKind::LexError(ref lex_error) => {
                lex_error.build_report(&mut builder);
            }
            ParseErrorKind::OhGolly => {
                builder.set_message("Oh, golly!");
                builder.add_label(ariadne::Label::new((source_id, 142..150))
                                  .with_message("no computies"));
                builder.add_label(ariadne::Label::new((source_id, 7..16))
                                  .with_message("nor groupies"));
            }
        }
        builder.finish()
    }
}
