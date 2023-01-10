//! Errors reported from script execution.

use crate::ast::Span;

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

pub type RunResult<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum ErrorKind {
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
    fn at(self, span: &Span, context: &'static str) -> RunResult<T>;
}

impl<T> IntoRunResult<T> for anyhow::Result<T> {
    fn at(self, span: &Span, context: &'static str) -> RunResult<T> {
        match self {
            Ok(v) => Ok(v),
            Err(error) => Err(Error {
                kind: ErrorKind::Anyhow { error, context },
                span: span.clone(),
            }),
        }
    }
}
