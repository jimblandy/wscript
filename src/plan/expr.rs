//! Planning expression evaluation.
#![allow(unused_variables)]

use super::{LeBytes, Module};
use crate::{ast, error, plan, run};
use ast::Span;

struct ExprPlanner<'a, 'p> {
    expr: &'a ast::Expression,
    module: &'p Module,
    expected_type: naga::Handle<naga::Type>,
}

/// A plan that can construct a value of type `T`.
pub type ValuePlan<T> = dyn Fn(&mut run::Context) -> run::ExprResult<T>;

/// A plan that can construct a [`ByteSource`].
pub type BytesPlan = dyn Fn(&mut run::Context) -> run::ExprResult<Box<dyn ByteSource + 'static>> + 'static;

/// A source of data to initialize a GPU buffer, or check its contents.
pub trait ByteSource {
    /// Return the remaining number of bytes this plan will generate.
    fn len(&self) -> usize;

    /// Fill `buf` with the next `buf.len()` bytes from this source.
    ///
    /// For simplicity of code, `buf` is probably a subsection of some
    /// larger buffer. For error reporting, `offset` is the offset
    /// within that larger buffer at which `buf` occurs.
    fn fill(&mut self, buf: &mut [u8], offset: usize) -> run::ExprResult<()>;

    /// Compare `buf` against the next `buf.len()` bytes from this source.
    ///
    /// For simplicity of code, `buf` is probably a subsection of some
    /// larger buffer. For error reporting, `offset` is the offset
    /// within that larger buffer at which `buf` occurs.
    fn compare(&mut self, buf: &[u8], offset: usize) -> run::ExprResult<Comparison>;
}

pub enum Comparison {
    /// This plan's bytes have the given length, and the corresponding
    /// prefix of the input matches.
    Matches { len: usize },

    /// This plan's bytes do not match the input, at the given byte offset.
    Mismatch { offset: usize },
}

pub type Result<T> = std::result::Result<T, Error>;

/// An error detecting while planning an expression.
#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ErrorKind {
}

pub(super) fn plan_expression_bytes<'a, 'p>(
    expr: &'a ast::Expression,
    module: &'p Module,
    expected_type: naga::Handle<naga::Type>,
) -> plan::Result<Box<BytesPlan>> {
    let plan = ExprPlanner::<'a, 'p> {
        expr,
        module,
        expected_type,
    }
    .plan_bytes()?;

    Ok(plan)
}

impl<'a, 'p> ExprPlanner<'a, 'p> {
    pub fn plan_bytes(&mut self) -> Result<Box<BytesPlan>> {
        use naga::ScalarKind as Sk;
        use naga::TypeInner as Ti;

        // Scalar and vector values we can just evaluate as values and
        // then convert to bytes.
        match self.module.naga.types[self.expected_type].inner {
            Ti::Scalar {
                kind: Sk::Float,
                width: 4,
            } => return self.plan_scalar_bytes::<f32>(),
            Ti::Scalar {
                kind: Sk::Sint,
                width: 4,
            } => return self.plan_scalar_bytes::<i32>(),
            Ti::Scalar {
                kind: Sk::Uint,
                width: 4,
            } => return self.plan_scalar_bytes::<u32>(),
            _ => (),
        }

        match self.expr.kind {
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

    fn plan_scalar_bytes<T>(&mut self) -> Result<Box<BytesPlan>>
    where
        T: LeBytes + Scalar + 'static,
    {
        let value_plan: Box<ValuePlan<T>> = self.plan_scalar_value()?;
        let span = self.expr.span.clone();
        Ok(Box::new(move |ctx: &mut run::Context| {
            let value = value_plan(ctx)?;
            Ok(Box::new(Bytes {
                bytes: value.to_le_bytes(),
                span: span.clone(),
                type_name: T::WGSL_NAME,
            }))
        }))
    }

    fn plan_subexpression_value<T>(
        &mut self,
        subexpression: &'a ast::Expression,
        expected_type: naga::Handle<naga::Type>,
    ) -> Result<Box<ValuePlan<T>>>
    where
        T: Scalar + 'static,
    {
        ExprPlanner {
            expr: subexpression,
            expected_type,
            module: &*self.module,
        }
        .plan_scalar_value()
    }

    fn plan_scalar_value<T>(&mut self) -> Result<Box<ValuePlan<T>>>
    where
        T: Scalar + 'static,
    {
        match self.expr.kind {
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
        left: &'a ast::Expression,
        op: ast::BinaryOp,
        _op_span: &Span,
        right: &'a ast::Expression,
    ) -> Result<Box<ValuePlan<T>>>
    where
        T: Scalar + 'static,
    {
        match op {
            ast::BinaryOp::Range => todo!(),
            ast::BinaryOp::Add => {
                let left_plan = self.plan_subexpression_value(left, self.expected_type)?;
                let right_plan = self.plan_subexpression_value(right, self.expected_type)?;
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
    ) -> Result<Box<BytesPlan>> {
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

    fn fill(&mut self, buf: &mut [u8], offset: usize) -> run::ExprResult<()> {
        self.check(buf, offset)?;
        buf[..self.len()].copy_from_slice(self.bytes.as_ref());
        Ok(())
    }

    fn compare(&mut self, buf: &[u8], offset: usize) -> run::ExprResult<Comparison> {
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
    fn check(&self, buf: &[u8], offset: usize) -> run::ExprResult<()> {
        let needed = std::mem::size_of::<B>();
        if buf.len() < needed {
            return Err(run::ExprError {
                span: self.span.clone(),
                kind: run::ExprErrorKind::BufferTooShort {
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

impl ErrorKind {
    pub fn build_report(&self, builder: &mut error::ReportBuilder) {
        todo!()
    }
}
