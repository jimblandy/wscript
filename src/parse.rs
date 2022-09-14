//! Parsing wscript.

use crate::ast::{self, join_spans, Program, Span};
use crate::lex::{BracketPosition, Input, Token, TokenError, TokenKind};
use error::{ParseError, ParseErrorKind};

use std::borrow::Cow;

mod error;

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
    next: Token,
}

impl<'a> Context<'a> {
    fn new(source: &'a str, source_id: usize) -> Result<Self, TokenError> {
        let mut input = Input::new(source, source_id);
        let token = input.get_token()?;
        Ok(Context { input, next: token })
    }

    fn peek(&self) -> &Token {
        &self.next
    }

    fn next(&mut self) -> Result<Token, TokenError> {
        Ok(std::mem::replace(&mut self.next, self.input.get_token()?))
    }

    fn start(&self) -> usize {
        self.next.span.1.start
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

    fn parse_statement(&mut self) -> Result<ast::Statement, ParseError> {
        match *self.peek() {
            Token {
                kind: TokenKind::Buffer,
                ..
            } => {
                let Token { span, .. } = self.next()?;
                self.parse_buffer(span)
            }
            _ => {
                let Token { span, .. } = self.next()?;
                Err(ParseError {
                    span,
                    kind: ParseErrorKind::ExpectedStatement,
                })
            }
        }
    }

    fn parse_buffer(&mut self, keyword: Span) -> Result<ast::Statement, ParseError> {
        use error::Attribute as At;
        use error::BufferAttribute as Ba;

        let mut group: Option<(u32, Span)> = None;
        let mut binding: Option<(u32, Span)> = None;

        while let Some(at) = self.take_if(&TokenKind::Symbol('@'))? {
            if self.take_if(&TokenKind::Group)?.is_some() {
                self.parse_unsigned_attribute(At::Buffer(Ba::Group), at, &mut group)?;
            } else if self.take_if(&TokenKind::Binding)?.is_some() {
                self.parse_unsigned_attribute(At::Buffer(Ba::Binding), at, &mut binding)?;
            }
        }

        let group = group.ok_or_else(|| ParseError {
            span: keyword.clone(),
            kind: ParseErrorKind::MissingAttr(At::Buffer(Ba::Group)),
        })?;
        let binding = binding.ok_or_else(|| ParseError {
            span: keyword.clone(),
            kind: ParseErrorKind::MissingAttr(At::Buffer(Ba::Group)),
        })?;

        let binding = ast::Binding { group, binding };

        self.expect(
            &TokenKind::Symbol(':'),
            &ParseErrorKind::ExpectedBufferAttributeOrType,
        )?;

        let ty = self.parse_type()?;

        self.expect(
            &TokenKind::Symbol('='),
            &ParseErrorKind::ExpectedBufferAttributeOrType,
        )?;

        let value = self.parse_expr()?;

        Ok(ast::Statement {
            span: join_spans(&keyword, &value.span),
            kind: ast::StatementKind::Buffer { binding, ty, value },
        })
    }

    fn parse_type(&mut self) -> Result<ast::Type, ParseError> {
        if let Some(ss) = self.take_if_scalar_type()? {
            return Ok(ss.into());
        }

        match self.peek().kind {
            TokenKind::Vec(size) => {
                let token = self.next()?;
                self.parse_vector_type(size, token)
            }
            TokenKind::Mat { columns, rows } => {
                let token = self.next()?;
                self.parse_matrix_type(columns, rows, token)
            }
            TokenKind::Array => {
                let token = self.next()?;
                self.parse_array_type(token)
            }
            _ => todo!(),
        }
    }

    fn take_if_scalar_type(&mut self) -> Result<Option<ScalarAndSpan>, ParseError> {
        let kind = match self.peek().kind {
            TokenKind::I32 => ast::ScalarKind::I32,
            TokenKind::U32 => ast::ScalarKind::U32,
            TokenKind::F32 => ast::ScalarKind::F32,
            TokenKind::Bool => ast::ScalarKind::Bool,
            _ => return Ok(None),
        };

        let token = self.next()?;
        Ok(Some(ScalarAndSpan {
            kind,
            span: token.span,
        }))
    }

    fn parse_vector_type(
        &mut self,
        size: ast::VectorSize,
        constructor: Token,
    ) -> Result<ast::Type, ParseError> {
        self.expect_type_parameter_bracket(&constructor, BracketPosition::Open)?;

        let component_ty = self.parse_type()?;

        let close = self.expect_type_parameter_bracket(&constructor, BracketPosition::Close)?;
        let span = join_spans(&constructor.span, &close);

        if let Some(kind) = component_ty.is_scalar() {
            Ok(ast::Type {
                kind: ast::TypeKind::Vector {
                    size,
                    component: kind,
                    component_span: component_ty.span,
                },
                span,
            })
        } else {
            Err(ParseError {
                span: component_ty.span,
                kind: ParseErrorKind::ExpectedScalarType {
                    constructor: constructor.kind.description(),
                    constructor_span: constructor.span.clone(),
                },
            })
        }
    }

    fn parse_matrix_type(
        &mut self,
        columns: ast::VectorSize,
        rows: ast::VectorSize,
        constructor: Token,
    ) -> Result<ast::Type, ParseError> {
        self.expect_type_parameter_bracket(&constructor, BracketPosition::Open)?;

        let ty = self.parse_type()?;

        let close = self.expect_type_parameter_bracket(&constructor, BracketPosition::Close)?;
        let span = join_spans(&constructor.span, &close);

        match ty {
            ast::Type {
                kind: ast::TypeKind::Scalar(ast::ScalarKind::F32),
                ..
            } => (),
            ast::Type {
                span: ref component_span,
                ..
            } => {
                return Err(ParseError {
                    span,
                    kind: ParseErrorKind::TypeMatrixF32 {
                        parameter: component_span.clone(),
                    },
                });
            }
        }

        Ok(ast::Type {
            kind: ast::TypeKind::Matrix { columns, rows },
            span,
        })
    }

    fn parse_array_type(&mut self, constructor: Token) -> Result<ast::Type, ParseError> {
        self.expect_type_parameter_bracket(&constructor, BracketPosition::Open)?;

        let element_type = Box::new(self.parse_type()?);

        let length = if self.take_if(&TokenKind::Symbol(','))?.is_some() {
            let (length, _span) = self.expect_unsigned_integer(|| {
                (
                    "array length not an integer".into(),
                    "The length of an array must be a non-negative integer.".into(),
                )
            })?;
            Some(length as usize)
        } else {
            None
        };

        let close = self.expect_type_parameter_bracket(&constructor, BracketPosition::Close)?;
        let span = join_spans(&constructor.span, &close);

        Ok(ast::Type {
            kind: ast::TypeKind::Array {
                element_type,
                length,
            },
            span,
        })
    }

    fn expect_type_parameter_bracket(
        &mut self,
        constructor: &Token,
        position: BracketPosition,
    ) -> Result<Span, ParseError> {
        self.expect(
            &position.angle_token(),
            &ParseErrorKind::ExpectedTypeParameterBracket {
                constructor: constructor.kind.description(),
                constructor_span: constructor.span.clone(),
                position,
            },
        )
    }

    fn parse_expr(&mut self) -> Result<ast::Expression, ParseError> {
        todo!()
    }

    fn parse_unsigned_attribute(
        &mut self,
        attr: error::Attribute,
        at: Span,
        value: &mut Option<(u32, Span)>,
    ) -> Result<(), ParseError> {
        let error = ParseErrorKind::ExpectedAttrParameter(attr);
        self.expect(&TokenKind::Symbol('('), &error)?;
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
        let close = self.expect(&TokenKind::Symbol(')'), &error)?;
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
