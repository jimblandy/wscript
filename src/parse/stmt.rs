use super::error::{ParseError, ParseErrorKind};
use super::Context;
use crate::ast::{self, join_spans, Span};
use crate::lex::{Token, TokenKind};

impl<'a> Context<'a> {
    pub fn parse_statement(&mut self) -> Result<ast::Statement, ParseError> {
        match self.peek().kind {
            TokenKind::Init => {
                let Token { span, .. } = self.next()?;
                self.parse_init(span)
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

    pub fn parse_init(&mut self, keyword: Span) -> Result<ast::Statement, ParseError> {
        let (id, span) = match self.next()? {
            Token {
                kind: TokenKind::Ident(name),
                span,
            } => (name.to_owned(), span),
            Token { span, .. } => {
                return Err(ParseError {
                    span,
                    kind: ParseErrorKind::ExpectedBufferId,
                });
            }
        };

        self.expect(&TokenKind::Symbol('='), &ParseErrorKind::ExpectedInitValue)?;

        let value = self.parse_expr()?;

        let buffer = ast::BufferId::Name { id, span };
        Ok(ast::Statement {
            span: join_spans(&keyword, &value.span),
            kind: ast::StatementKind::Init { buffer, value },
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
