//! Parsing wscript.

use crate::ast::{self, Program, Span};
use crate::error;
use crate::lex::{Input, Token, TokenError};

pub fn parse(source: &str, source_id: usize) -> Result<Program, ParseError> {
    let mut context = Context::new(source, source_id).map_err(|token_err| ParseError {
        kind: ParseErrorKind::LexError(token_err.clone()),
        span: token_err.span,
    })?;

    let mut program = vec![];
    loop {
        if let Token::End = context.next {
            break;
        }
        program.push(context.parse_statement()?);
    }

    Ok(program)
}

/// Ambient arguments for the recursive descent parser.
#[derive(Debug)]
struct Context<'s> {
    /// The source we're parsing.
    input: Input<'s>,

    /// The next token. At end of file, this is `Token::End`.
    next: Token,

    /// The span of `next`.
    next_span: Span,
}

impl<'a> Context<'a> {
    fn new(source: &'a str, source_id: usize) -> Result<Self, TokenError> {
        let mut input = Input::new(source, source_id);
        let crate::lex::TokenOk { token, span } = input.get_token()?;
        Ok(Context {
            input,
            next: token,
            next_span: span,
        })
    }

    fn peek(&self) -> &Token {
        &self.next
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParseError> {
        todo!()
    }
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
                builder.add_label(
                    ariadne::Label::new((source_id, 142..150)).with_message("no computies"),
                );
                builder.add_label(
                    ariadne::Label::new((source_id, 7..16)).with_message("nor groupies"),
                );
            }
        }
        builder.finish()
    }
}
