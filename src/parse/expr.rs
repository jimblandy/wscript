use super::error::{ParseError, ParseErrorKind};
use super::Context;
use crate::ast::{self, join_spans, BinaryOp};
use crate::lex::{Token, TokenKind};

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
enum Tightness {
    Outermost,
    Additive,
    Multiplicative,
}

impl<'a> Context<'a> {
    pub fn parse_expr(&mut self) -> Result<ast::Expression, ParseError> {
        self.parse_binary_expr(Tightness::Outermost)
    }

    fn parse_binary_expr(&mut self, enclosing: Tightness) -> Result<ast::Expression, ParseError> {
        let left = self.parse_primary_expr()?;

        // Is the next token a binary operator that binds more tightly
        // than our enclosing expression? If so, then recurse.
        if let Some(op) = BinaryOp::from_token(self.peek()) {
            let next = op.tightness();
            if next > enclosing {
                let Token { span: op_span, .. } = self.next()?;
                let right = self.parse_binary_expr(next)?;
                let span = join_spans(&left.span, &right.span);
                let (left, right) = (Box::new(left), Box::new(right));
                return Ok(ast::Expression {
                    span,
                    kind: ast::ExpressionKind::Binary {
                        left,
                        op,
                        op_span,
                        right,
                    },
                });
            }
        }

        Ok(left)
    }

    fn parse_primary_expr(&mut self) -> Result<ast::Expression, ParseError> {
        match self.peek().kind {
            TokenKind::Number(n) => {
                let Token { span, .. } = self.next()?;
                Ok(ast::Expression {
                    kind: ast::ExpressionKind::Literal(n),
                    span,
                })
            }
            TokenKind::Symbol('{') => todo!(),
            TokenKind::Symbol('-') => todo!(),
            TokenKind::Symbol('(') => {
                let Token { span, .. } = self.next()?;
                let expr = self.parse_expr()?;
                self.expect(
                    &TokenKind::Symbol(')'),
                    &ParseErrorKind::MissingCloseParen { opening: span },
                )?;
                Ok(expr)
            }
            TokenKind::Symbol('^') => todo!(),
            TokenKind::Vec(_) => todo!(),
            TokenKind::Mat { .. } => todo!(),
            _ => todo!(),
        }
    }
}

impl BinaryOp {
    fn from_token(token: &Token) -> Option<BinaryOp> {
        match token.kind {
            TokenKind::Symbol('+') => Some(Self::Add),
            TokenKind::Symbol('-') => Some(Self::Subtract),
            TokenKind::Symbol('*') => Some(Self::Multiply),
            TokenKind::Symbol('/') => Some(Self::Divide),
            TokenKind::Symbol('%') => Some(Self::Remainder),
            _ => None,
        }
    }

    fn tightness(self) -> Tightness {
        match self {
            BinaryOp::Add => Tightness::Additive,
            BinaryOp::Subtract => Tightness::Additive,
            BinaryOp::Multiply => Tightness::Multiplicative,
            BinaryOp::Divide => Tightness::Multiplicative,
            BinaryOp::Remainder => Tightness::Multiplicative,
        }
    }
}
