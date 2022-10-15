#![allow(dead_code)]

use super::error::{ParseError, ParseErrorKind};
use super::{Context, ScalarAndSpan};
use crate::ast::{self, join_spans, Span};
use crate::lex::{BracketPosition, Token, TokenKind};

impl<'a> Context<'a> {
    pub fn take_if_type(&mut self) -> Result<Option<ast::Type>, ParseError> {
        if let Some(ss) = self.take_if_scalar_type()? {
            return Ok(Some(ss.into()));
        }

        let ty = match self.peek().kind {
            TokenKind::Vec(size) => {
                let token = self.next()?;
                self.parse_vector_type(size, token)?
            }
            TokenKind::Mat { columns, rows } => {
                let token = self.next()?;
                self.parse_matrix_type(columns, rows, token)?
            }
            TokenKind::Array => {
                let token = self.next()?;
                self.parse_array_type(token)?
            }
            _ => return Ok(None),
        };

        Ok(Some(ty))
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

        let component_ty = self.take_if_type()?.ok_or_else(|| ParseError {
            span: self.peek().span.clone(),
            kind: ParseErrorKind::ExpectedType {
                introducing_span: constructor.span.clone(),
                thing: "vector type",
                help: "All vector types take a component type as a parameter.\n\
                       For example: `vec4<u32>`",
            },
        })?;

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

        let ty = self.take_if_type()?.ok_or_else(|| ParseError {
            span: self.peek().span.clone(),
            kind: ParseErrorKind::ExpectedType {
                introducing_span: constructor.span.clone(),
                thing: "matrix type",
                help: "All matrix types take a component type as a parameter.\n\
                       For example: `mat3x4<f32>`",
            },
        })?;

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

        let element_type = self.take_if_type()?.ok_or_else(|| ParseError {
            span: self.peek().span.clone(),
            kind: ParseErrorKind::ExpectedType {
                introducing_span: constructor.span.clone(),
                thing: "array type",
                help: "All array types take a component type and a length as parameters.\n\
                       For example: `array<f32, 10>`",
            },
        })?;

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
                element_type: Box::new(element_type),
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
            || ParseErrorKind::ExpectedTypeParameterBracket {
                constructor: constructor.kind.description(),
                constructor_span: constructor.span.clone(),
                position,
            },
        )
    }
}
