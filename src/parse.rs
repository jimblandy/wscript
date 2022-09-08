//! Parsing wscript.

use crate::ast::{self, Program, Span};
use crate::lex::{Input, Token, TokenError};
use error::{ParseError, ParseErrorKind};

mod error;
mod tests;

pub fn parse(source: &str, source_id: usize) -> Result<Program, ParseError> {
    let mut context = Context::new(source, source_id)?;

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

    fn next(&mut self) -> Result<(Token, Span), TokenError> {
        let crate::lex::TokenOk { token, span } = self.input.get_token()?;
        let token = std::mem::replace(&mut self.next, token);
        let span = std::mem::replace(&mut self.next_span, span);
        Ok((token, span))
    }

    /// Return true if the next token is `token`, and consume it.
    ///
    /// If the next token isn't `token`, return false and leave the
    /// state of the input unchanged.
    fn take_if(&mut self, token: &Token) -> Result<bool, ParseError> {
        if self.peek() != token {
            return Ok(false);
        }
        self.next()?;
        Ok(true)
    }

    fn expect(&mut self, expected: &Token, error: ParseErrorKind) -> Result<(), ParseError> {
        if !self.take_if(expected)? {
            return Err(self.next_unexpected(error));
        }
        Ok(())
    }

    fn expect_unsigned_integer(&mut self, message: &'static str, help: &'static str) -> Result<u64, ParseError> {
        if let Token::Number(n) = *self.peek() {
            if n as u64 as f64 == n {
                self.next()?;
                return Ok(n as u64);
            }
        }

        Err(self.next_unexpected(ParseErrorKind::ExpectedInteger {
            unsigned: true,
            message,
            help,
        }))
    }

    fn error(&self, span: Span, kind: ParseErrorKind) -> ParseError {
        ParseError {
            span,
            kind,
        }
    }

    fn next_unexpected(&self, error: ParseErrorKind) -> ParseError {
        self.error(self.next_span.clone(), error)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParseError> {
        if self.take_if(&Token::Buffer)? {
            self.parse_buffer()
        } else {
            Err(self.next_unexpected(ParseErrorKind::ExpectedStatement))
        }
    }

    fn parse_buffer(&mut self) -> Result<ast::Statement, ParseError> {
        let mut group = None;
        let mut binding = None;
        
        while self.take_if(&Token::Symbol('@'))? {
            if self.take_if(&Token::Group)? {
                self.expect(&Token::Symbol('('), ParseErrorKind::ExpectedGroupAttrParameter)?;
                group = Some(self.expect_unsigned_integer(
                    "binding group number must be a positive integer or zero",
                    "The value `n` in `@group(n)` must be a positive integer or zero.")?);
                self.expect(&Token::Symbol(')'), ParseErrorKind::ExpectedGroupAttrParameter)?;
            } else if self.take_if(&Token::Binding)? {
                self.expect(&Token::Symbol('('), ParseErrorKind::ExpectedBindingAttrParameter)?;
                binding = Some(self.expect_unsigned_integer(
                    "binding index must be a positive integer or zero",
                    "The value `n` in `@binding(n)` must be a positive integer or zero.")?);
                self.expect(&Token::Symbol(')'), ParseErrorKind::ExpectedBindingAttrParameter)?;
            }
        }

        dbg!(&group);
        dbg!(&binding);

        self.expect(&Token::Symbol(':'), ParseErrorKind::ExpectedBufferAttributeOrType)?;

        todo!()
    }
}
