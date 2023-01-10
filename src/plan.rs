//! Producing an execution plan from a parsed program.
//!
//! In wscript, a 'plan' is, roughly speaking, the compiled form of a script.
//! We've done everything we can do in advance to prepare the script for
//! execution; all static analysis is done.
//!
//! There are two main types of plan:
//!
//! - A [`StatementPlan`] is a closure you can call to execute a
//!   wscript statement.
//!
//! - An [`ExpressionPlan`] is a closure you can call to evaluate a wscript
//!   expression, to produce a value of some specific type.
//!
//! Both operate on [`Context`]s.
///
/// [`Context`]: run::Context

mod error;
mod expr;
mod module;

use crate::ast;
use crate::run;
use error::IntoPlanResult as _;
pub use error::{Error, ErrorKind, PlanResult};
pub use module::Module;

use std::io;
use std::sync::Arc;

/// An execution plan for producing a block of bytes on the CPU.
///
/// A bytes plan is a trait object that can either write the bytes it
/// represents to a `&mut [u8]` or compare the bytes in a `&[u8]`
/// against the bytes it represents.
pub trait ByteSource {
    fn write(&self, stream: &mut dyn io::Write) -> io::Result<()>;
    fn compare(&self, stream: &mut dyn io::Read) -> io::Result<Comparison>;
}

pub enum Comparison {
    /// This plan's bytes have the given length, and the corresponding
    /// prefix of the input matches.
    Matches { len: usize },

    /// This plan's bytes do not match the input, at the given byte offset.
    Mismatch { pos: usize },
}

pub type BytesPlan = dyn ByteSource;

/// A statement plan is a closure that executes a wscript statement.
/// It may fail, returning a `run::Error`. It may use and modify the
/// state of a [`Context`].
///
/// [`Context`]: run::Context
pub type StatementPlan = dyn Fn(&mut run::Context) -> Result<(), run::Error>;

/// State for planning.
///
/// A `Planner` holds the state used to statically analyze the script:
/// information known before execution begins.
#[derive(Debug, Default)]
struct Planner {
    module: Option<Arc<Module>>,
}

impl Planner {
    pub fn plan(
        &mut self,
        program: &ast::Program,
        source_id: usize,
    ) -> Result<Box<StatementPlan>, Error> {
        let plans = program
            .iter()
            .map(|stmt| self.plan_statement(stmt))
            .collect::<Result<Vec<Box<StatementPlan>>, _>>()?;
        Ok(Box::new(move |ctx| {
            for plan in plans.iter() {
                plan(ctx)?;
            }
            Ok(())
        }))
    }

    fn plan_statement(&mut self, statement: &ast::Statement) -> Result<Box<StatementPlan>, Error> {
        match statement.kind {
            ast::StatementKind::Module { ref wgsl } => self.plan_module(wgsl, statement),
            ast::StatementKind::Init {
                ref buffer,
                ref value,
            } => self.plan_init(buffer, value, statement),
            ast::StatementKind::Dispatch { entry_point, count } => todo!(),
            ast::StatementKind::Check { buffer, value } => todo!(),
        }
    }

    fn plan_module(
        &mut self,
        wgsl: &ast::Wgsl,
        statement: &ast::Statement,
    ) -> Result<Box<StatementPlan>, Error> {
        let module = module::Module::new(&wgsl.text, statement.span.clone())
            .at(&statement.span, "in this `module` statement")?;
        let module = Arc::new(module);
        self.module = Some(module.clone());
        Ok(Box::new(move |ctx| ctx.run_module(&module)))
    }

    fn plan_init(
        &mut self,
        buffer: &ast::BufferId,
        value: &ast::Expression,
        statement: &ast::Statement,
    ) -> PlanResult<Box<StatementPlan>> {
        let Some(module) = self.module else {
            return Err(Error {
                kind: ErrorKind::NoModule { victim: error::StatementKind::Init },
                span: statement.span.clone() 
            });
        };
        let buffer = module.find_buffer(buffer);
        todo!()
    }
}
