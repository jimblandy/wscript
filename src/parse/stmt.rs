use super::error::{self, ParseError, ParseErrorKind};
use super::Context;
use crate::ast::{self, join_spans, Span};
use crate::lex::{Token, TokenKind};

impl<'a> Context<'a> {
    pub fn parse_statement(&mut self) -> Result<ast::Statement, ParseError> {
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

    pub fn parse_buffer(&mut self, keyword: Span) -> Result<ast::Statement, ParseError> {
        use super::error::Attribute as At;
        use super::error::BufferAttribute as Ba;

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

        let ty = self.take_if_type()?.ok_or_else(|| ParseError {
            span: self.peek().span.clone(),
            kind: ParseErrorKind::ExpectedType {
                introducing_span: keyword.clone(),
                thing: "`buffer` statement",
                help: "A `buffer` statement has the form: `buffer ATTRIBUTES : TYPE = VALUE`.\n\
                     For example: `buffer @group(0) @binding(3): u32 = 1729`",
            },
        })?;

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
