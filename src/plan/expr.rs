//! Planning expression evaluation.
#![allow(unused_variables, unreachable_code)]

use super::{LeBytes, Module};
use crate::wgsl::Wgsl;
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
pub type BytesPlan =
    dyn Fn(&mut run::Context) -> run::ExprResult<Box<dyn ByteSource + 'static>> + 'static;

/// A source of data to initialize a GPU buffer, or check its contents.
pub trait ByteSource {
    /// Return the number of bytes this plan will generate.
    fn byte_length(&self) -> usize;

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

    /// Return the span of the expression that generates these bytes.
    fn span(&self) -> &Span;

    /// The name of the type of value that said expression produces.
    fn type_name(&self) -> &'static str;

    fn check(&self, buf: &[u8], offset: usize) -> run::ExprResult<()> {
        let needed = self.byte_length();
        if buf.len() != needed {
            return Err(run::ExprError {
                span: self.span().clone(),
                kind: run::ExprErrorKind::BufferTooShort {
                    type_name: self.type_name(),
                    needed,
                    available: buf.len(),
                    offset,
                },
            });
        }
        Ok(())
    }
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

    /// The expression we're complaining about.
    pub span: Span,
}

#[derive(Debug)]
pub enum ErrorKind {
    /// The expected type of a range expression was not an array.
    RangeNotArray {
        /// The expected type.
        expected_type_span: Option<Span>,

        /// The name of the expected type, as a string.
        expected_type_name: String,
    },

    /// The expected type of a range expression was not an array of scalars.
    RangeNotScalarArray {
        /// The expected type.
        expected_type_span: Option<Span>,

        /// The name of the expected type, as a string.
        expected_type_name: String,
    },

    /// We can't use range expressions to generate arrays of this type.
    UnsupportedScalarRangeType {
        ty: String,
    },

    UnsupportedScalarType {
        ty: String,
    },
}

pub(super) fn plan_expression_bytes<'p, 'a>(
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
        match *self.expected_type_inner() {
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
                let left_plan = self.plan_subexpression_value::<T>(left, self.expected_type)?;
                let right_plan = self.plan_subexpression_value::<T>(right, self.expected_type)?;
                Ok(Box::new(move |ctx: &mut run::Context| {
                    let left = left_plan(ctx)?;
                    let right = right_plan(ctx)?;
                    Ok(left + right)
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
        left: &'a ast::Expression,
        op: ast::BinaryOp,
        op_span: &Span,
        right: &'a ast::Expression,
    ) -> Result<Box<BytesPlan>> {
        match op {
            ast::BinaryOp::Range => self.plan_range_bytes(left, op_span, right),
            ast::BinaryOp::Add => todo!(),
            ast::BinaryOp::Subtract => todo!(),
            ast::BinaryOp::Multiply => todo!(),
            ast::BinaryOp::Divide => todo!(),
            ast::BinaryOp::Remainder => todo!(),
        }
    }

    fn plan_range_bytes(
        &mut self,
        left: &'a ast::Expression,
        op_span: &Span,
        right: &'a ast::Expression,
    ) -> Result<Box<BytesPlan>> {
        use naga::TypeInner as Ti;

        let Ti::Array { base, size, stride } = *self.expected_type_inner() else {
            return Err(Error {
                span: self.expr.span.clone(),
                kind: ErrorKind::RangeNotArray {
                    expected_type_span: self.expected_type_span(),
                    expected_type_name: Wgsl((self.expected_type, &self.module.naga)).to_string(),
                }
            });
        };

        let Ti::Scalar { kind, width } = *self.type_inner(base) else {
            return Err(Error {
                span: self.expr.span.clone(),
                kind: ErrorKind::RangeNotScalarArray {
                    expected_type_span: self.expected_type_span(),
                    expected_type_name: Wgsl((self.expected_type, &self.module.naga)).to_string(),
                }
            });
        };

        use naga::ScalarKind as Sk;
        match (kind, width) {
            (Sk::Sint, 4) => self.range::<i32>(base, left, right),
            (Sk::Uint, 4) => self.range::<u32>(base, left, right),
            (Sk::Float, 4) => self.range::<f32>(base, left, right),
            (Sk::Sint | Sk::Uint | Sk::Float, _) => {
                return Err(Error {
                    span: self.expr.span.clone(),
                    kind: ErrorKind::UnsupportedScalarType {
                        ty: Wgsl((base, &self.module.naga)).to_string(),
                    },
                });
            }
            (Sk::Bool, _) => {
                return Err(Error {
                    span: self.expr.span.clone(),
                    kind: ErrorKind::UnsupportedScalarRangeType {
                        ty: Wgsl((base, &self.module.naga)).to_string(),
                    },
                });
            }
        }
    }

    fn range<T>(
        &mut self,
        expected_type: naga::Handle<naga::Type>,
        start: &'a ast::Expression,
        end: &'a ast::Expression,
    ) -> Result<Box<BytesPlan>>
    where
        T: LeBytes + Scalar + 'static,
    {
        let span = self.expr.span.clone();
        let start_plan = self.plan_subexpression_value::<T>(start, expected_type)?;
        let end_plan = self.plan_subexpression_value::<T>(end, expected_type)?;
        Ok(Box::new(move |ctx: &mut run::Context| {
            let start = start_plan(ctx)?;
            let end = end_plan(ctx)?;
            Ok(Box::new(RangeBytes {
                range: start..end,
                span: span.clone(),
                type_name: T::WGSL_NAME,
            }))
        }))
    }

    fn type_inner(&self, handle: naga::Handle<naga::Type>) -> &'p naga::TypeInner {
        &self.module.naga.types[handle].inner
    }

    fn expected_type_inner(&self) -> &'p naga::TypeInner {
        self.type_inner(self.expected_type)
    }

    fn expected_type_span(&self) -> Option<Span> {
        let wgsl_span = self
            .module
            .naga
            .types
            .get_span(self.expected_type)
            .to_range()?;
        let wscript_span = self.module.source.span_from_text_range(wgsl_span);
        let source_id = self.expr.span.0;
        Some((source_id, wscript_span))
    }
}

trait Scalar:
    Copy
    + std::ops::Add<Self, Output = Self>
    + std::ops::Sub<Self, Output = Self>
    + std::cmp::PartialOrd
{
    fn from_literal(n: f64) -> Self;
    fn as_usize(self) -> usize;
    fn one() -> Self;
}

macro_rules! implement_scalar {
    ( $( $t:ty ),* ) => {
        $(
            impl Scalar for $t {
                fn from_literal(n: f64) -> Self {
                    n as $t
                }

                fn as_usize(self) -> usize {
                    self as usize
                }

                fn one() -> Self {
                    1 as Self
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
    fn byte_length(&self) -> usize {
        self.bytes.as_ref().len()
    }

    fn fill(&mut self, buf: &mut [u8], offset: usize) -> run::ExprResult<()> {
        self.check(buf, offset)?;
        buf[..self.byte_length()].copy_from_slice(self.bytes.as_ref());
        Ok(())
    }

    fn compare(&mut self, buf: &[u8], offset: usize) -> run::ExprResult<Comparison> {
        self.check(buf, offset)?;
        for (i, (actual, expected)) in buf[..self.byte_length()]
            .iter()
            .copied()
            .zip(self.bytes.as_ref().iter().copied())
            .enumerate()
        {
            if actual != expected {
                return Ok(Comparison::Mismatch { offset: i });
            }
        }
        Ok(Comparison::Matches {
            len: self.byte_length(),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }

    fn type_name(&self) -> &'static str {
        self.type_name
    }
}

struct RangeBytes<T> {
    range: std::ops::Range<T>,
    span: Span,
    type_name: &'static str,
}

impl<T: Scalar> RangeBytes<T> {
    fn value_count(&self) -> usize {
        (self.range.end - self.range.start).as_usize()
    }
}

impl<T: LeBytes + Scalar> ByteSource for RangeBytes<T> {
    fn byte_length(&self) -> usize {
        self.value_count() * std::mem::size_of::<T>()
    }

    fn fill(&mut self, buf: &mut [u8], offset: usize) -> run::ExprResult<()> {
        let len = self.byte_length();
        self.check(buf, offset)?;

        let mut i = self.range.start;
        let values = std::iter::from_fn(|| {
            if i >= self.range.end {
                return None;
            }

            let next = i;
            i = i + Scalar::one();
            Some(next)
        });
        let chunks = buf[..len].chunks_exact_mut(std::mem::size_of::<T>());
        assert_eq!(chunks.len(), self.value_count());

        for (chunk, value) in chunks.zip(values) {
            chunk.copy_from_slice(value.to_le_bytes().as_ref());
        }

        Ok(())
    }

    fn compare(&mut self, buf: &[u8], offset: usize) -> run::ExprResult<Comparison> {
        todo!()
    }

    fn span(&self) -> &Span {
        &self.span
    }

    fn type_name(&self) -> &'static str {
        self.type_name
    }
}

impl ErrorKind {
    pub fn build_report(&self, span: &Span, builder: &mut error::ReportBuilder) {
        match *self {
            ErrorKind::RangeNotArray {
                ref expected_type_span,
                ref expected_type_name,
            } => {
                builder.set_message(format!(
                    "Range expressions must have array types, but this is expected to be of type `{}`",
                    expected_type_name));
                builder.add_label(
                    ariadne::Label::new(span.clone())
                        .with_message("this range expression will produce an array"),
                );
                if let Some(span) = expected_type_span.as_ref() {
                    builder.add_label(
                        ariadne::Label::new(span.clone())
                            .with_message("the expression must produce a value of this type"),
                    );
                }
            }
            ErrorKind::RangeNotScalarArray {
                ref expected_type_span,
                ref expected_type_name,
            } => {
                todo!()
            }
            ErrorKind::UnsupportedScalarRangeType { .. } => todo!(),
            ErrorKind::UnsupportedScalarType { .. } => todo!(),
        }
    }
}
