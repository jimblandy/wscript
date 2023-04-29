//! Errors reported from script execution.

use crate::ast::Span;
use crate::error;
use std::io;

/// An error that occurs at runtime.
#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum ErrorKind {
    /// The error `inner` occurred while evaluating an `init` statement's expression.
    InitExpression {
        /// The error that occurred during expression evaluation.
        inner: Box<ExprError>,

        /// The id of the buffer we were trying to initialize.
        buffer: String,

        /// The span of the buffer's declaration in the module, if
        /// Naga knows it
        buffer_decl: Option<Span>,
    },

    /// The error `inner` occurred while executing a `check` statement.
    Check {
        /// The error that occurred during expression evaluation.
        inner: Box<ExprError>,

        /// The id of the buffer whose contents we were trying to check.
        buffer: String,
    },

    /// A `check` statement failed.
    CheckFailed {
        /// The id of the buffer whose contents we were trying to check.
        buffer: String,

        /// The byte offset that didn't match.
        offset: usize,
    },

    /// An `anyhow` error.
    //
    // It's a little odd to use `anyhow::Error` this way, since its
    // whole purpose is to used as a universal error type: one might
    // think it should wrap `run::Error`, not the other way around.
    //
    // But we can't really wrap up Ariadne-style errors in
    // `anyhow::Error`: `anyhow::Error` can only encapsulate types
    // that implement `std::error::Error`, which assumes that printing
    // the error message requires no information beyond the error
    // value itself---and producing an Ariadne error requires a
    // `Cache`.
    //
    // Instead, we supply a span as context for where the
    // `anyhow::Error` occurred, and then use Ariadne to format
    // everything.
    Anyhow {
        error: anyhow::Error,
        context: &'static str,
    },
}

pub type ExprResult<T> = std::result::Result<T, ExprError>;

/// An error that occurs while evaluating an expression at runtime.
#[derive(Debug)]
pub struct ExprError {
    pub span: Span,
    pub kind: ExprErrorKind,
}

#[derive(Debug)]
pub enum ExprErrorKind {
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

    /// A range expression doesn't evaluate to the right number of
    /// elements to initialize an array.
    BadRangeCount {
        /// The number of values that were needed.
        needed: usize,

        /// The number of values the range expression was going to produce.
        actual: usize,
    }
}

/// Types that can be converted into a `run::error::Result` by
/// providing a span and context.
pub trait IntoRunResult<T> {
    fn at(self, span: &Span, context: &'static str) -> Result<T>;
}

impl<T> IntoRunResult<T> for anyhow::Result<T> {
    fn at(self, span: &Span, context: &'static str) -> Result<T> {
        match self {
            Ok(v) => Ok(v),
            Err(error) => Err(Error {
                kind: ErrorKind::Anyhow { error, context },
                span: span.clone(),
            }),
        }
    }
}

impl error::AriadneReport for Error {
    fn write_with_config<W>(
        &self,
        stream: W,
        cache: &mut error::Cache,
        config: ariadne::Config,
    ) -> io::Result<()>
    where
        W: io::Write,
    {
        use ariadne::{Report, ReportKind};

        let (source_id, range) = self.span.clone();
        let mut builder =
            Report::<Span>::build(ReportKind::Error, source_id, range.start).with_config(config);

        match self.kind {
            ErrorKind::Anyhow {
                ref error,
                ref context,
            } => {
                builder.set_message(format!("Error in {context}: {error}"));
            }

            ErrorKind::InitExpression {
                ref inner,
                ref buffer,
                ref buffer_decl,
            } => {
                inner.build(&mut builder);
                builder.add_label(
                    ariadne::Label::new(buffer_decl.unwrap_or(self.span).clone())
                        .with_message(format!("while initializing buffer {buffer}")),
                );
            }

            ErrorKind::Check {
                ref inner,
                ref buffer,
            } => {
                inner.build(&mut builder);
                builder.add_label(
                    ariadne::Label::new(self.span.clone())
                        .with_message(format!("while checking contents of buffer {buffer}")),
                );
            }

            ErrorKind::CheckFailed { ref buffer, offset } => {
                builder.set_message(format!(
                    "buffer {buffer} does not have the expected contents at byte {offset}"
                ));
                builder.add_label(
                    ariadne::Label::new(self.span.clone())
                        .with_message("while executing this statement"),
                );
            }
        }

        let report = builder.finish();
        report.write(cache, stream)
    }
}

impl ExprError {
    fn build(
        &self,
        builder: &mut error::ReportBuilder,
    )
    {
        match self.kind {
            ExprErrorKind::BufferTooShort {
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
                builder.set_help(format!("value needs {needed} bytes, but only {available} bytes \
                                          are available at offset {offset} in the buffer"));
            }
            ExprErrorKind::BadRangeCount { needed, actual } => {
                builder.set_message("range expression returned the wrong number of elements for array");
                builder.add_label(
                    ariadne::Label::new(self.span.clone()).with_message("this range expression"),
                );
                builder.set_help(format!("the array being initialized has {needed} elements, \
                                          but the range expression only produced {actual} elements"));
            }
        }
    }
}
