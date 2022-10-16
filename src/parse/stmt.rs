use super::error::{self, ParseError, ParseErrorKind};
use super::Context;
use crate::ast::{self, join_spans, Span};
use crate::lex::{Token, TokenKind};

impl<'a> Context<'a> {
    pub fn parse_statement(&mut self) -> Result<ast::Statement, ParseError> {
        match self.peek().kind {
            TokenKind::Module => {
                let Token { span, .. } = self.next()?;
                self.parse_module(span)
            }
            TokenKind::Init => {
                let Token { span, .. } = self.next()?;
                self.parse_init(span)
            }
            TokenKind::Dispatch => {
                let Token { span, .. } = self.next()?;
                self.parse_dispatch(span)
            }
            TokenKind::Check => {
                let Token { span, .. } = self.next()?;
                self.parse_check(span)
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

    pub fn parse_module(&mut self, keyword: Span) -> Result<ast::Statement, ParseError> {
        let (text, span) = match self.next()? {
            Token {
                kind: TokenKind::Code(code),
                span,
            } => (code, span),
            Token { span, .. } => {
                return Err(ParseError {
                    span,
                    kind: ParseErrorKind::ExpectedModuleCode,
                });
            }
        };

        Ok(ast::Statement {
            span: join_spans(&keyword, &span),
            kind: ast::StatementKind::Module {
                wgsl: ast::Wgsl { text, span },
            },
        })
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

        self.expect(&TokenKind::Symbol('='), || {
            ParseErrorKind::ExpectedInitValue
        })?;

        let value = self.parse_expr()?;

        let buffer = ast::BufferId::Name { id, span };
        Ok(ast::Statement {
            span: join_spans(&keyword, &value.span),
            kind: ast::StatementKind::Init { buffer, value },
        })
    }

    pub fn parse_dispatch(&mut self, keyword: Span) -> Result<ast::Statement, ParseError> {
        let entry_point = match self.next()? {
            Token {
                kind: TokenKind::Ident(name),
                span,
            } => ast::EntryPoint {
                name: name.to_owned(),
                span,
            },
            Token { span, .. } => {
                return Err(ParseError {
                    span,
                    kind: ParseErrorKind::ExpectedEntryPointName,
                });
            }
        };

        let count = self.parse_workgroup_count(&keyword)?;

        let span = join_spans(&keyword, &count.span);
        Ok(ast::Statement {
            kind: ast::StatementKind::Dispatch { entry_point, count },
            span,
        })
    }

    pub fn parse_workgroup_count(
        &mut self,
        command_span: &Span,
    ) -> Result<ast::WorkgroupCount, ParseError> {
        let start_span = self.expect(&TokenKind::Symbol('('), || {
            ParseErrorKind::ExpectedWorkgroupCount {
                command_span: command_span.clone(),
            }
        })?;
        let mut counts = [1, 1, 1];
        let mut i = 0;
        let end_span;
        loop {
            counts[i] = self
                .expect_unsigned_integer(|| {
                    (
                        "Expected number in workgroup count".into(),
                        "A workgroup count is a triplet of sizes, like `(16,32,64)`.".into(),
                    )
                })?
                .0 as usize;
            i += 1;

            match self.next()? {
                Token {
                    kind: TokenKind::Symbol(')'),
                    span,
                } => {
                    end_span = span;
                    break;
                }
                Token {
                    kind: TokenKind::Symbol(','),
                    span,
                } => {
                    if i >= 3 {
                        return Err(ParseError {
                            span,
                            kind: ParseErrorKind::WorkgroupCountTooLong,
                        });
                    }
                }
                Token { span, .. } => {
                    return Err(ParseError {
                        kind: ParseErrorKind::UnexpectedToken {
                            place: "in workgroup count",
                            expected: "a comma or a closing parenthesis",
                        },
                        span,
                    });
                }
            }
        }

        Ok(ast::WorkgroupCount {
            size: (counts[0], counts[1], counts[2]),
            span: join_spans(&start_span, &end_span),
        })
    }

    pub fn parse_check(&mut self, keyword: Span) -> Result<ast::Statement, ParseError> {
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

        self.expect(&TokenKind::Symbol('='), || {
            ParseErrorKind::ExpectedCheckValue
        })?;

        let value = self.parse_expr()?;

        let buffer = ast::BufferId::Name { id, span };
        Ok(ast::Statement {
            span: join_spans(&keyword, &value.span),
            kind: ast::StatementKind::Check { buffer, value },
        })
    }

    #[allow(dead_code)]
    fn parse_unsigned_attribute(
        &mut self,
        attr: error::Attribute,
        at: Span,
        value: &mut Option<(u32, Span)>,
    ) -> Result<(), ParseError> {
        self.expect(&TokenKind::Symbol('('), || {
            ParseErrorKind::ExpectedAttrParameter(attr)
        })?;
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
        let close = self.expect(&TokenKind::Symbol(')'), || {
            ParseErrorKind::ExpectedAttrParameter(attr)
        })?;
        new.1 = join_spans(&at, &close);
        check_duplicate_attr(value, &new, attr)?;
        *value = Some(new);
        Ok(())
    }
}

#[allow(dead_code)]
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
