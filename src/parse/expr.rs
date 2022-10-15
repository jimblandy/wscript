use super::error::{ParseError, ParseErrorKind};
use super::Context;
use crate::ast::{self, join_spans, BinaryOp, Span};
use crate::lex::{Token, TokenKind};

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq)]
enum Tightness {
    Range,
    Additive,
    Multiplicative,
}

impl<'a> Context<'a> {
    pub fn parse_expr(&mut self) -> Result<ast::Expression, ParseError> {
        let mut expr = self.parse_primary_expr()?;

        if let Some((op, op_span)) = is_binary_op(self.peek()) {
            self.next()?;
            // A stack of binary operators awaiting right operands,
            // with each operator's precedence strictly greater than
            // its predecessor. Since every element has a different
            // precedence, the stack is never deeper than the number
            // of precedence levels in the language.
            let mut stack = vec![(expr, op, op_span)];

            loop {
                assert!(!stack.is_empty());

                expr = self.parse_primary_expr()?;
                if let Some((next_op, next_op_span)) = is_binary_op(self.peek()) {
                    self.next()?;
                    // Pop any higher-or-equal precedence operators from the stack and build
                    // binary expressions from them.
                    while stack
                        .last()
                        .filter(|&(_left, op, _op_span)| op.tightness() >= next_op.tightness())
                        .is_some()
                    {
                        let (left, op, op_span) = stack.pop().unwrap();
                        expr = make_binary(left, op, op_span, expr);
                    }

                    // Now we know that the stack is either empty, or topped by
                    // an operator of lower tightness than `next_op`. That means
                    // that the new `expr` is `next_op`'s left operand, and we
                    // must push it back on the stack.
                    stack.push((expr, next_op, next_op_span));
                } else {
                    break;
                }
            }

            // We've reached the end of the chain of binary operator
            // applications. Draw down the stack.
            while let Some((left, left_op, left_op_span)) = stack.pop() {
                expr = make_binary(left, left_op, left_op_span, expr);
            }
        }

        Ok(expr)
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
                    || ParseErrorKind::MissingCloseParen { opening: span })?;
                Ok(expr)
            }
            TokenKind::Symbol('^') => todo!(),
            TokenKind::Vec(_) => todo!(),
            TokenKind::Mat { .. } => todo!(),
            _ => {
                let Token { span, .. } = self.next()?;
                return Err(ParseError {
                    span,
                    kind: ParseErrorKind::UnexpectedToken {
                        place: "in expression",
                        expected: "a token that could begin an expression",
                    },
                });
            }
        }
    }
}

fn is_binary_op(token: &Token) -> Option<(BinaryOp, Span)> {
    let op = match token.kind {
        TokenKind::Range => Some(BinaryOp::Range),
        TokenKind::Symbol('+') => Some(BinaryOp::Add),
        TokenKind::Symbol('-') => Some(BinaryOp::Subtract),
        TokenKind::Symbol('*') => Some(BinaryOp::Multiply),
        TokenKind::Symbol('/') => Some(BinaryOp::Divide),
        TokenKind::Symbol('%') => Some(BinaryOp::Remainder),
        _ => None,
    };
    op.map(|op| (op, token.span.clone()))
}

impl BinaryOp {
    fn tightness(self) -> Tightness {
        match self {
            BinaryOp::Range => Tightness::Range,
            BinaryOp::Add => Tightness::Additive,
            BinaryOp::Subtract => Tightness::Additive,
            BinaryOp::Multiply => Tightness::Multiplicative,
            BinaryOp::Divide => Tightness::Multiplicative,
            BinaryOp::Remainder => Tightness::Multiplicative,
        }
    }
}

fn make_binary(
    left: ast::Expression,
    op: BinaryOp,
    op_span: Span,
    right: ast::Expression,
) -> ast::Expression {
    ast::Expression {
        span: join_spans(&left.span, &right.span),
        kind: ast::ExpressionKind::Binary {
            left: Box::new(left),
            op,
            op_span,
            right: Box::new(right),
        },
    }
}
