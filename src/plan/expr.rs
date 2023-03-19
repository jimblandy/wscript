//! Planning expression evaluation.
#![allow(unused_variables)]

use super::{LeBytes, Module};
use crate::{ast, error, run};
use ast::Span;

struct ExprPlanner<'p> {
    module: &'p Module,
    expected_type: naga::Handle<naga::Type>,
}

/// A plan that can construct a value of type `T`.
pub type ValuePlan<T> = dyn Fn(&mut run::Context) -> Result<T>;

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

    /// Compare `buf` against the next `buf.len()` bytes from this source.
    ///
    /// For simplicity of code, `buf` is probably a subsection of some
    /// larger buffer. For error reporting, `offset` is the offset
    /// within that larger buffer at which `buf` occurs.
    fn compare(&mut self, buf: &[u8], offset: usize) -> Result<Comparison>;
}

pub enum Comparison {
    /// This plan's bytes have the given length, and the corresponding
    /// prefix of the input matches.
    Matches { len: usize },

    /// This plan's bytes do not match the input, at the given byte offset.
    Mismatch { offset: usize },
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

pub(super) fn plan_expression_bytes(
    expr: &ast::Expression,
    module: &Module,
    expected_type: naga::Handle<naga::Type>,
) -> super::Result<Box<BytesPlan>> {
    ExprPlanner {
        module,
        expected_type,
    }
    .plan_bytes(expr)
}

impl<'p> ExprPlanner<'p> {
    pub fn plan_bytes(&mut self, expr: &ast::Expression) -> super::Result<Box<BytesPlan>> {
        use naga::ScalarKind as Sk;
        use naga::TypeInner as Ti;

        // Scalar and vector values we can just evaluate as values and
        // then convert to bytes.
        match self.module.naga.types[self.expected_type].inner {
            Ti::Scalar {
                kind: Sk::Float,
                width: 4,
            } => return self.plan_scalar_bytes::<f32>(expr),
            Ti::Scalar {
                kind: Sk::Sint,
                width: 4,
            } => return self.plan_scalar_bytes::<i32>(expr),
            Ti::Scalar {
                kind: Sk::Uint,
                width: 4,
            } => return self.plan_scalar_bytes::<u32>(expr),
            _ => (),
        }

        match expr.kind {
            ast::ExpressionKind::Literal(_) => todo!(),
            ast::ExpressionKind::Sequence(_) => todo!(),
            ast::ExpressionKind::Unary { op: _, operand: _ } => todo!(),
            ast::ExpressionKind::Binary {
                ref left,
                op,
                ref op_span,
                ref right,
            } => self.plan_binary_bytes(left, op, op_span, right),
            ast::ExpressionKind::Nullary(_) => todo!(),
            ast::ExpressionKind::Vec(_) => todo!(),
        }
    }

    fn plan_scalar_bytes<T>(&mut self, expr: &ast::Expression) -> super::Result<Box<BytesPlan>>
    where
        T: LeBytes + Scalar + 'static,
    {
        let value_plan: Box<ValuePlan<T>> = self.plan_scalar_value(expr)?;
        let span = expr.span.clone();
        Ok(Box::new(move |ctx: &mut run::Context| {
            let value = value_plan(ctx)?;
            Ok(Box::new(Bytes {
                bytes: value.to_le_bytes(),
                span: span.clone(),
                type_name: T::WGSL_NAME,
            }))
        }))
    }

    fn plan_scalar_value<T>(&mut self, expr: &ast::Expression) -> super::Result<Box<ValuePlan<T>>>
    where
        T: Scalar + 'static,
    {
        match expr.kind {
            ast::ExpressionKind::Literal(n) => Ok(plan_value::<T>(T::from_literal(n))),
            ast::ExpressionKind::Sequence(_) => todo!(),
            ast::ExpressionKind::Unary { op: _, operand: _ } => todo!(),
            ast::ExpressionKind::Binary {
                ref left,
                op,
                ref op_span,
                ref right,
            } => self.plan_binary_scalar_value(left, op, op_span, right),
            ast::ExpressionKind::Nullary(_) => todo!(),
            ast::ExpressionKind::Vec(_) => todo!(),
        }

        // Scalar and vector values we can just evaluate as values and
        // then convert to bytes.
    }

    fn plan_binary_scalar_value<T>(
        &mut self,
        left: &ast::Expression,
        op: ast::BinaryOp,
        _op_span: &Span,
        right: &ast::Expression,
    ) -> super::Result<Box<ValuePlan<T>>>
    where
        T: Scalar + 'static,
    {
        match op {
            ast::BinaryOp::Range => todo!(),
            ast::BinaryOp::Add => {
                let left_plan = self.plan_scalar_value(left)?;
                let right_plan = self.plan_scalar_value(right)?;
                Ok(Box::new(move |ctx: &mut run::Context| {
                    let left = left_plan(ctx)?;
                    let right = right_plan(ctx)?;
                    Ok(T::add(left, right))
                }))
            }
            ast::BinaryOp::Subtract => todo!(),
            ast::BinaryOp::Multiply => todo!(),
            ast::BinaryOp::Divide => todo!(),
            ast::BinaryOp::Remainder => todo!(),
        }
    }

    fn plan_binary_bytes(
        &mut self,
        left: &ast::Expression,
        op: ast::BinaryOp,
        op_span: &Span,
        right: &ast::Expression,
    ) -> super::Result<Box<BytesPlan>> {
        match op {
            ast::BinaryOp::Range => {
                let left_plan = todo!();
            }
            ast::BinaryOp::Add => todo!(),
            ast::BinaryOp::Subtract => todo!(),
            ast::BinaryOp::Multiply => todo!(),
            ast::BinaryOp::Divide => todo!(),
            ast::BinaryOp::Remainder => todo!(),
        }
    }
}

trait Scalar: Copy {
    fn from_literal(n: f64) -> Self;
    fn add(left: Self, right: Self) -> Self;
}

macro_rules! implement_scalar {
    ( $( $t:ty ),* ) => {
        $(
            impl Scalar for $t {
                fn from_literal(n: f64) -> Self {
                    n as $t
                }

                fn add(left: Self, right: Self) -> Self {
                    left + right
                }
            }
        )*
    }
}

implement_scalar!(f32, i32, u32);

#[derive(Clone)]
struct Bytes<B: AsRef<[u8]>> {
    bytes: B,

    /// The span of the expression that produced this value.
    span: Span,

    /// The name of the type of value that said expression produces.
    type_name: &'static str,
}

fn plan_value<T>(value: T) -> Box<ValuePlan<T>>
where
    T: Copy + 'static,
{
    Box::new(move |_: &mut run::Context| Ok(value))
}

impl<B> ByteSource for Bytes<B>
where
    B: AsRef<[u8]>,
{
    fn len(&self) -> usize {
        self.bytes.as_ref().len()
    }

    fn fill(&mut self, buf: &mut [u8], offset: usize) -> Result<()> {
        self.check(buf, offset)?;
        buf[..self.len()].copy_from_slice(self.bytes.as_ref());
        Ok(())
    }

    fn compare(&mut self, buf: &[u8], offset: usize) -> Result<Comparison> {
        self.check(buf, offset)?;
        for (i, (actual, expected)) in buf[..self.len()]
            .iter()
            .copied()
            .zip(self.bytes.as_ref().iter().copied())
            .enumerate()
        {
            if actual != expected {
                return Ok(Comparison::Mismatch { offset: i });
            }
        }
        Ok(Comparison::Matches { len: self.len() })
    }
}

impl<B: AsRef<[u8]>> Bytes<B> {
    fn check(&self, buf: &[u8], offset: usize) -> Result<()> {
        let needed = std::mem::size_of::<B>();
        if buf.len() < needed {
            return Err(Error {
                span: self.span.clone(),
                kind: ErrorKind::BufferTooShort {
                    type_name: self.type_name,
                    needed,
                    available: buf.len(),
                    offset,
                },
            });
        }
        Ok(())
    }
}

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
