//! Errors reported from script execution.

use crate::ast::Span;
use crate::error;
use std::io;

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum ErrorKind {
    /// The error `inner` occurred while executing an `init` statement.
    Init {
        /// The error that occurred during expression evaluation.
        inner: Box<crate::plan::ExprError>,

        /// The id of the buffer we were trying to initialize.
        buffer: String,
    },

    /// The error `inner` occurred while executing a `check` statement.
    Check {
        /// The error that occurred during expression evaluation.
        inner: Box<crate::plan::ExprError>,

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
        mut stream: W,
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
            ErrorKind::Init {
                ref inner,
                ref buffer,
            } => {
                inner.write_with_config(&mut stream, cache, config)?;
                builder.add_label(
                    ariadne::Label::new(self.span.clone())
                        .with_message(format!("while initializing buffer {buffer}")),
                );
                builder.add_label(
                    ariadne::Label::new(self.span.clone())
                        .with_message("while executing this statement"),
                );
            }
            ErrorKind::Check {
                ref inner,
                ref buffer,
            } => {
                inner.write_with_config(&mut stream, cache, config)?;
                builder.add_label(
                    ariadne::Label::new(self.span.clone())
                        .with_message(format!("while checking contents of buffer {buffer}")),
                );
                builder.add_label(
                    ariadne::Label::new(self.span.clone())
                        .with_message("while executing this statement"),
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
