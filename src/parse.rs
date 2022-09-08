//! Parsing wscript.

use crate::ast::{self, join_spans, Program, Span};
use crate::lex::{Input, Token, TokenError};
use error::{ParseError, ParseErrorKind};

use std::borrow::Cow;

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

    fn start(&self) -> usize {
        self.next_span.1.start
    }

    /// Return `Some(span)` if the next token is `token`, and consume it.
    ///
    /// If the next token isn't `token`, return `None` and leave the
    /// state of the input unchanged.
    fn take_if(&mut self, token: &Token) -> Result<Option<Span>, ParseError> {
        if self.peek() != token {
            return Ok(None);
        }
        let (_, span) = self.next()?;
        Ok(Some(span))
    }

    fn expect(&mut self, expected: &Token, error: &ParseErrorKind) -> Result<Span, ParseError> {
        if let Some(span) = self.take_if(expected)? {
            Ok(span)
        } else {
            Err(self.next_unexpected(error.clone()))
        }
    }

    fn expect_unsigned_integer(
        &mut self,
        get_message_and_help: impl FnOnce() -> (Cow<'static, str>, Cow<'static, str>),
    ) -> Result<(u64, Span), ParseError> {
        if let Token::Number(n) = *self.peek() {
            if n as u64 as f64 == n {
                let (_, span) = self.next()?;
                return Ok((n as u64, span));
            }
        }

        let (message, help) = get_message_and_help();
        Err(self.next_unexpected(ParseErrorKind::ExpectedInteger {
            unsigned: true,
            message,
            help,
        }))
    }

    fn error(&self, span: Span, kind: ParseErrorKind) -> ParseError {
        ParseError { span, kind }
    }

    fn next_unexpected(&self, error: ParseErrorKind) -> ParseError {
        self.error(self.next_span.clone(), error)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParseError> {
        if let Some(span) = self.take_if(&Token::Buffer)? {
            self.parse_buffer(span)
        } else {
            Err(self.next_unexpected(ParseErrorKind::ExpectedStatement))
        }
    }

    fn parse_buffer(&mut self, keyword: Span) -> Result<ast::Statement, ParseError> {
        use error::Attribute as At;
        use error::BufferAttribute as Ba;

        let mut group: Option<(u64, Span)> = None;
        let mut binding: Option<(u64, Span)> = None;

        while let Some(at) = self.take_if(&Token::Symbol('@'))? {
            if self.take_if(&Token::Group)?.is_some() {
                self.parse_unsigned_attribute(At::Buffer(Ba::Group), at, &mut group)?;
            } else if self.take_if(&Token::Binding)?.is_some() {
                self.parse_unsigned_attribute(At::Buffer(Ba::Binding), at, &mut binding)?;
            }
        }

        dbg!(&group);
        dbg!(&binding);

        self.expect(
            &Token::Symbol(':'),
            &ParseErrorKind::ExpectedBufferAttributeOrType,
        )?;

        todo!()
    }

    fn parse_unsigned_attribute(
        &mut self,
        attr: error::Attribute,
        at: Span,
        value: &mut Option<(u64, Span)>,
    ) -> Result<(), ParseError> {
        let error = ParseErrorKind::ExpectedAttrParameter(attr);
        self.expect(&Token::Symbol('('), &error)?;
        let mut new = self.expect_unsigned_integer(|| {
            let message = format!(
                "the {} must be a positive integer or zero",
                attr.description()
            );
            let help = format!(
                "The value `n` in `@{}(n)` must be a positive integer or zero.",
                attr.token()
            );
            (message.into(), help.into())
        })?;
        let close = self.expect(&Token::Symbol(')'), &error)?;
        new.1 = join_spans(&at, &close);
        check_duplicate_attr(value, &new, attr)?;
        *value = Some(new);
        Ok(())
    }
}

fn check_duplicate_attr<T>(
    prior: &Option<(T, Span)>,
    new: &(T, Span),
    attr: error::Attribute,
) -> Result<(), ParseError> {
    if let Some((_, ref prior_span)) = *prior {
        return Err(ParseError {
            span: new.1.clone(),
            kind: ParseErrorKind::DuplicateAttribute {
                attr,
                prior: prior_span.clone(),
            },
        });
    }

    Ok(())
}
