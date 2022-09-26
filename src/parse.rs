//! Parsing wscript.

use crate::ast::{self, Program, Span};
use crate::lex::{Input, Token, TokenError, TokenKind};
use error::{ParseError, ParseErrorKind};

use std::borrow::Cow;

mod error;
mod expr;
mod stmt;
mod ty;

#[cfg(test)]
mod tests;

pub fn parse(source: &str, source_id: usize) -> Result<Program, ParseError> {
    let mut context = Context::new(source, source_id)?;

    let mut program = vec![];
    loop {
        if let TokenKind::End = context.next.kind {
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

    /// The next token. At end of file, this is `TokenKind::End`.
    next: Token<'s>,
}

impl<'a> Context<'a> {
    fn new<'s>(source: &'s str, source_id: usize) -> Result<Context<'s>, TokenError> {
        let mut input = Input::new(source, source_id);
        let token = input.get_token()?;
        Ok(Context { input, next: token })
    }

    fn peek(&self) -> &Token {
        &self.next
    }

    fn next(&mut self) -> Result<Token<'a>, TokenError> {
        Ok(std::mem::replace(&mut self.next, self.input.get_token()?))
    }

    /// Return `Some(span)` if the next token is `token`, and consume it.
    ///
    /// If the next token isn't `token`, return `None` and leave the
    /// state of the input unchanged.
    fn take_if(&mut self, token: &TokenKind) -> Result<Option<Span>, ParseError> {
        if &self.peek().kind != token {
            return Ok(None);
        }
        let Token { span, .. } = self.next()?;
        Ok(Some(span))
    }

    /// Require the next token to be `token`, and consume it.
    ///
    /// If something else is next, then complain about it using `error`.
    fn expect(&mut self, expected: &TokenKind, error: &ParseErrorKind) -> Result<Span, ParseError> {
        if let Some(span) = self.take_if(expected)? {
            Ok(span)
        } else {
            Err(self.error_at_next(error.clone()))
        }
    }

    fn expect_unsigned_integer(
        &mut self,
        get_message_and_help: impl FnOnce() -> (Cow<'static, str>, Cow<'static, str>),
    ) -> Result<(u32, Span), ParseError> {
        if let Token {
            kind: TokenKind::Number(n),
            ..
        } = *self.peek()
        {
            if n as u32 as f64 == n {
                let token = self.next()?;
                return Ok((n as u32, token.span));
            }
        }

        let (message, help) = get_message_and_help();
        Err(self.error_at_next(ParseErrorKind::ExpectedInteger {
            unsigned: true,
            message,
            help,
        }))
    }

    fn error_at_next(&self, kind: ParseErrorKind) -> ParseError {
        ParseError {
            span: self.next.span.clone(),
            kind,
        }
    }
}

struct ScalarAndSpan {
    kind: ast::ScalarKind,
    span: Span,
}

impl From<ScalarAndSpan> for ast::Type {
    fn from(ss: ScalarAndSpan) -> Self {
        ast::Type {
            span: ss.span,
            kind: ast::TypeKind::Scalar(ss.kind),
        }
    }
}
