//! Expression evaluation.

use crate::ast;
use super::{RunResult, Error, ErrorKind};

use fallible_iterator::FallibleIterator;

pub enum Value<'expr> {
    Scalar(f64),
    Stream(Box<Stream<'expr>>),
}

pub type Stream<'expr> = dyn FallibleIterator<Item = Value<'expr>, Error = Error> + Send + Sync + 'expr;

pub struct Context;

impl ast::Expression {
    pub fn eval<'e>(&'e self, ctx: &'e Context) -> RunResult<Value<'e>> {
        Ok(match self.kind {
            ast::ExpressionKind::Literal(n) => Value::Scalar(n),
            ast::ExpressionKind::Sequence(ref elements) => {
                Value::Stream(Box::new(elements.iter().map(|element| {
                    element.eval(ctx)
                })))
            }
            ast::ExpressionKind::Unary { ref op, ref operand } => todo!(),
            ast::ExpressionKind::Binary { ref left, op, ref op_span, ref right } => {
                match op {
                    ast::BinaryOp::Range => {
                        let start = left.eval(ctx)?;
                        let end = right.eval(ctx)?;
                        match (start, end) {
                            (Value::Scalar(start), Value::Scalar(end)) => {
                                todo!()
                            }
                            (_, _) => {
                                return Err(Error { kind: ErrorKind::, span: self.span.clone() });
                            }
                        }
                    }
                    ast::BinaryOp::Add => todo!(),
                    ast::BinaryOp::Subtract => todo!(),
                    ast::BinaryOp::Multiply => todo!(),
                    ast::BinaryOp::Divide => todo!(),
                    ast::BinaryOp::Remainder => todo!(),
                }
            }
            ast::ExpressionKind::Nullary(_) => todo!(),
            ast::ExpressionKind::Vec(_) => todo!(),
        })
    }
}
