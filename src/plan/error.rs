//! Errors produced in the planning stage.

use crate::ast::{self, Span};
use crate::error;

use std::io;

pub type PlanResult<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ErrorKind {
    NoModule {
        victim: StatementKind,
    },
    NoSuchBuffer {
        module: Span,
        buffer_id: ast::BufferId,
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

#[derive(Clone, Copy, Debug)]
pub enum StatementKind {
    Init,
}

impl error::AriadneReport for Error {
    fn write_with_config<W>(
        &self,
        stream: W,
        cache: &mut crate::error::Cache,
        config: ariadne::Config,
    ) -> io::Result<()>
    where
        W: io::Write,
    {
        use ariadne::{Report, ReportKind};

        let (source_id, range) = self.span.clone();
        let mut builder =
            Report::build(ReportKind::Error, source_id, range.start).with_config(config);
        let b = &mut builder;

        fn label<M: ToString>(b: &mut crate::error::ReportBuilder, span: &Span, message: M) {
            b.add_label(ariadne::Label::new(span.clone()).with_message(message));
        }

        match self.kind {
            ErrorKind::NoModule { victim } => {
                b.set_message("No current module established");
                label(b, &self.span, "this statement requires a current module");
                b.set_help(format!(
                    "{} statement must be preceded by a `module` statement.",
                    victim.indefinite()
                ));
            }
            ErrorKind::NoSuchBuffer {
                ref module,
                buffer_id,
            } => {
                b.set_message(format!("No buffer {} in current module.", buffer_id.kind));
                match buffer_id.kind {
                    ast::BufferIdKind::Name(_) => {
                        label(
                            b,
                            &buffer_id.span,
                            "The module has no global variable by this name.",
                        );
                    }
                    ast::BufferIdKind::Binding(_) => {
                        label(
                            b,
                            &buffer_id.span,
                            "The module has no global variable at this binding group and index.",
                        );
                    }
                };
                label(b, &module, "This is the current shader module.");
                b.set_help("Buffer identifiers must refer to globals in the current shader module by name \
                            or by binding group and index.");
            }
            ErrorKind::Anyhow {
                ref error,
                ref context,
            } => {
                b.set_message(format!("Error in {}: {}", context, error));
            }
        }

        let report = builder.finish();
        report.write(cache, stream)
    }
}

impl StatementKind {
    fn indefinite(&self) -> &'static str {
        match *self {
            StatementKind::Init => "An `init` statement",
        }
    }
}

/// Types that can be converted into a [`PlanResult<T>`] by
/// providing a span and context.
pub trait IntoPlanResult<T> {
    fn at(self, span: &Span, context: &'static str) -> PlanResult<T>;
}

impl<T> IntoPlanResult<T> for anyhow::Result<T> {
    fn at(self, span: &Span, context: &'static str) -> PlanResult<T> {
        match self {
            Ok(v) => Ok(v),
            Err(error) => Err(Error {
                kind: ErrorKind::Anyhow { error, context },
                span: span.clone(),
            }),
        }
    }
}