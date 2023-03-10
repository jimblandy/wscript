//! Planning expression evaluation.
#![allow(unused_imports)]

use super::{Module, Planner};
use crate::{ast, error, run};
use ast::Span;

/// A plan that can construct a [`ByteSource`].
pub type BytesPlan = dyn Fn(&mut run::Context) -> Result<Box<dyn ByteSource + 'static>> + 'static;

/// A source of data to initialize a GPU buffer, or check its contents.
pub trait ByteSource {
    /// Return the remaining number of bytes this plan will generate.
    fn len(&self) -> usize;

    /// Fill `buf` with the next `buf.len()` bytes from this source.
    ///
    /// For simplicity of code, `buf` is probably a subsection of some
    /// larger buffer. For error reporting, `offset` is the offset
    /// within that larger buffer at which `buf` occurs.
    fn fill(&mut self, buf: &mut [u8], offset: usize) -> Result<()>;

    /// Check `buf` against the next `buf.len()` bytes from this source.
    ///
    /// For simplicity of code, `buf` is probably a subsection of some
    /// larger buffer. For error reporting, `offset` is the offset
    /// within that larger buffer at which `buf` occurs.
    fn check(&mut self, buf: &[u8], offset: usize) -> Result<Comparison>;
}

pub enum Comparison {
    /// This plan's bytes have the given length, and the corresponding
    /// prefix of the input matches.
    Matches { len: usize },

    /// This plan's bytes do not match the input, at the given byte offset.
    Mismatch { pos: usize },
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    span: Span,
}

#[derive(Debug)]
pub enum ErrorKind {
    /// The buffer named `label` was not large enough to hold `value`.
    ///
    /// The error's [`span`] indicates the expression that produced
    /// the value that doesn't fit.
    ///
    /// [`span`]: Error::span
    BufferTooShort {
        /// The kind of value we're trying to store in the buffer.
        type_name: &'static str,

        /// The number of bytes needed to store the value.
        needed: usize,

        /// The number of bytes actually available.
        available: usize,

        /// The offset at which we were trying to store the value.
        offset: usize,
    },
}

struct ExprPlanner<'p, 'e> {
    planner: &'p mut Planner,
    expr_span: &'e Span,
    module: &'p Module,
    expected_type: naga::Handle<naga::Type>,
}

impl Planner {
    pub(super) fn plan_expression(
        &mut self,
        expr: &ast::Expression,
        module: &Module,
        expected_type: naga::Handle<naga::Type>,
    ) -> super::Result<Box<BytesPlan>> {
        ExprPlanner {
            planner: self,
            expr_span: &expr.span,
            module,
            expected_type,
        }.plan(expr)
    }
}

impl ExprPlanner<'_, '_> {
    fn plan(&mut self, expr: &ast::Expression) -> super::Result<Box<BytesPlan>> {
        match expr.kind {
            ast::ExpressionKind::Literal(n) => {
                self.plan_literal(n)
            }
            ast::ExpressionKind::Sequence(_) => todo!(),
            ast::ExpressionKind::Unary {
                ref op,
                ref operand,
            } => todo!(),
            ast::ExpressionKind::Binary {
                ref left,
                op,
                ref op_span,
                ref right,
            } => self.plan_binary(left, op, op_span, right),
            ast::ExpressionKind::Nullary(_) => todo!(),
            ast::ExpressionKind::Vec(_) => todo!(),
        }
    }

    fn plan_literal(
        &mut self,
        n: f64,
    ) -> super::Result<Box<BytesPlan>> {
        use naga::ScalarKind as Sk;
        use naga::TypeInner as Ti;

        match self.module.naga.types[self.expected_type].inner {
            Ti::Scalar {
                kind: Sk::Float,
                width,
            } => match width {
                4 => Ok(Literal::<f32>::plan(n as f32, self.expr_span)),
                _ => Err(todo!()),
            },
            Ti::Scalar {
                kind: Sk::Uint,
                width,
            } => match width {
                4 => Ok(Literal::<u32>::plan(n as u32, self.expr_span)),
                _ => Err(todo!()),
            },
            Ti::Scalar {
                kind: Sk::Sint,
                width,
            } => match width {
                4 => Ok(Literal::<i32>::plan(n as i32, self.expr_span)),
                _ => Err(todo!()),
            },
            _ => Err(todo!()),
        }
    }

    fn plan_binary(
        &mut self,
        left: &ast::Expression,
        op: ast::BinaryOp,
        op_span: &Span,
        right: &ast::Expression,
    ) -> super::Result<Box<BytesPlan>> {
        match op {
            ast::BinaryOp::Range => todo!(),
            ast::BinaryOp::Add => todo!(),
            ast::BinaryOp::Subtract => todo!(),
            ast::BinaryOp::Multiply => todo!(),
            ast::BinaryOp::Divide => todo!(),
            ast::BinaryOp::Remainder => todo!(),
        }
    }
}

#[derive(Clone)]
struct Literal<T> {
    value: T,

    /// The span of the expression that produced this value.
    span: Span,
}

impl<T: Clone + 'static> Literal<T>
where
    Literal<T>: ByteSource,
{
    fn plan(n: T, span: &Span) -> Box<BytesPlan> {
        let byte_source = Literal {
            value: n,
            span: span.clone(),
        };
        Box::new(move |ctx: &mut run::Context| Ok(Box::new(byte_source.clone())))
    }
}

impl<T> ByteSource for Literal<T>
where
    T: LeBytes + Copy,
{
    fn len(&self) -> usize {
        std::mem::size_of_val(&self.value)
    }

    fn fill(&mut self, buf: &mut [u8], offset: usize) -> Result<()> {
        self.value.check(buf, offset, &self.span)?;
        buf[..self.len()].copy_from_slice(self.value.to_le_bytes().as_ref());
        Ok(())
    }

    fn check(&mut self, buf: &[u8], offset: usize) -> Result<Comparison> {
        todo!()
    }
}

trait LeBytes: Sized {
    type Bytes: AsRef<[u8]>;
    const NAME: &'static str;

    fn to_le_bytes(self) -> Self::Bytes;
    fn from_le_bytes(bytes: Self::Bytes) -> Self;

    fn check(&self, buf: &[u8], offset: usize, span: &Span) -> Result<()> {
        let needed = std::mem::size_of::<Self>();
        if buf.len() < needed {
            return Err(Error {
                span: span.clone(),
                kind: ErrorKind::BufferTooShort {
                    type_name: Self::NAME,
                    needed,
                    available: buf.len(),
                    offset,
                },
            });
        }
        Ok(())
    }
}

macro_rules! impl_to_le_bytes {
    ( $( $t:ty ),* ) => {
        $(
            impl LeBytes for $t {
                type Bytes = [u8; std::mem::size_of::<$t>()];
                const NAME: &'static str = stringify!($t);

                fn to_le_bytes(self) -> Self::Bytes {
                    self.to_le_bytes()
                }

                fn from_le_bytes(bytes: Self::Bytes) -> Self {
                    todo!()
                }
            }
        )*
    }
}

impl_to_le_bytes!(f32, i32, u32);

impl error::AriadneReport for Error {
    fn write_with_config<W>(
        &self,
        stream: W,
        cache: &mut error::Cache,
        config: ariadne::Config,
    ) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        use ariadne::{Report, ReportKind};

        let (source_id, range) = self.span.clone();
        let mut builder =
            Report::<Span>::build(ReportKind::Error, source_id, range.start).with_config(config);

        match self.kind {
            ErrorKind::BufferTooShort {
                type_name,
                needed,
                available,
                offset,
            } => {
                builder.set_message(format!(
                    "not enough room in buffer to hold {type_name} value"
                ));
                builder.add_label(
                    ariadne::Label::new(self.span.clone()).with_message("this value doesn't fit"),
                );
                builder.set_help(format!("value needs {needed} bytes, but only {available} bytes are available at offset {offset} in the buffer"));
            }
        }

        let report = builder.finish();
        report.write(cache, stream)
    }
}
