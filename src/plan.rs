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
//!
//! [`Context`]: run::Context

#![allow(unreachable_code, unused_variables)]

mod error;
mod module;

use crate::ast;
use crate::run;
use error::IntoPlanResult as _;
pub use error::{Error, ErrorKind, Result};
pub use module::Module;

use indexmap::IndexSet;
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

pub type BytesPlan = dyn ByteSource + 'static;

/// A `Plan` is a closure that runs a script, given a [`Context`].
/// It may fail, returning a `run::Error`.
///
/// [`Context`]: run::Context
pub type Plan = dyn Fn(&mut run::Context) -> run::Result<()> + 'static;

/// Context for planning script execution.
///
/// This holds static information about the current statement being
/// planned.
struct Planner {
    /// Static information about the current Naga module, if any.
    module: Option<Arc<Module>>,

    /// A map from a buffer's global handle to its index in [`run::Context::buffers`].
    buffers: IndexSet<naga::Handle<naga::GlobalVariable>>,
}

pub fn plan(
    program: &ast::Program,
    source_id: usize,
) -> Result<Box<Plan>> {
    Planner::plan(program, source_id)
}

impl Planner {
    fn plan(
        program: &ast::Program,
        source_id: usize,
    ) -> Result<Box<Plan>> {
        let mut planner = Self {
            module: None,
            buffers: Default::default(),
        };

        let plans = program
            .iter()
            .map(|statement| planner.plan_statement(statement))
            .collect::<Result<Vec<_>>>()?;

        Ok(Box::new(move |ctx| {
            for plan in plans.iter() {
                plan(ctx)?;
            }
            Ok(())
        }))
    }

    fn plan_statement(&mut self, statement: &ast::Statement) -> Result<Box<Plan>> {
        match statement.kind {
            ast::StatementKind::Module { ref wgsl } => {
                self.plan_module(statement, wgsl)
            }
            ast::StatementKind::Init {
                ref buffer,
                ref value,
            } => self.plan_init(statement, buffer, value),
            ast::StatementKind::Dispatch { ref entry_point, ref count } => todo!(),
            ast::StatementKind::Check { ref buffer, ref value } => todo!(),
        }
    }


    fn plan_module(
        &mut self,
        statement: &ast::Statement,
        wgsl: &ast::Wgsl,
    ) -> Result<Box<Plan>> {
        let module = Module::new(&wgsl.text, statement.span.clone())
            .at(&statement.span, "in this `module` statement")?;
        let module = Arc::new(module);
        self.module = Some(module.clone());
        Ok(Box::new(move |ctx| ctx.run_module(&module)))
    }

    fn plan_init(
        &mut self,
        statement: &ast::Statement,
        buffer: &ast::BufferId,
        value: &ast::Expression,
    ) -> Result<Box<Plan>> {
        let module = self.require_module(statement)?;
        let buffer = module.find_buffer(buffer)?;
        let value_plan = self.plan_expression(value)?;

        // Find the buffer's index in `run::Context::buffers`,
        // assigning it a new one if needed.
        let (index, prior) = self.buffers.insert_full(buffer);
        
        let plan: Box<Plan> = if prior {
            Box::new(move |ctx: &mut run::Context| {
                ctx.run_init_buffer(index, &*value_plan)
            })
        } else {
            let descriptor = wgpu::BufferDescriptor {
                label: todo!(),
                size: todo!(),
                usage: todo!(),
                mapped_at_creation: todo!(),
            };
            Box::new(move |ctx: &mut run::Context| {
                ctx.run_create_buffer(index, &descriptor)?;
                ctx.run_init_buffer(index, &*value_plan)
            })
        };

        Ok(plan)
    }

    fn plan_expression(
        &mut self,
        expr: &ast::Expression
    ) -> Result<Box<BytesPlan>> {
        todo!()
    }

    fn require_module(&self, victim: &ast::Statement) -> Result<&Module> {
        match self.module {
            Some(ref module) => Ok(module),
            None => Err(Error {
                kind: ErrorKind::NoModule { victim: error::StatementKind::from_ast(victim) },
                span: victim.span.clone() 
            })
        }
    }
}


/*

        // Compute the buffer's size from its type.
        let var = &naga.global_variables[global];
        let inner = &naga.types[var.ty].inner;
        let size = wgpu::BufferAddress::try_from(inner.try_size(&naga.constants)?)?;

        // Guess a usage for the buffer from its address space.
        let usage = match var.space {
            naga::AddressSpace::Uniform => Bu::UNIFORM | Bu::COPY_DST,
            naga::AddressSpace::Storage { access } => {
                let mut usage = Bu::STORAGE;
                // If the shader is going to read from it, then we'd
                // better be able to write to it.
                if access.contains(Sa::LOAD) {
                    usage |= Bu::COPY_DST;
                }

                // Conversely, if the shader is going to write to it,
                // then we'd better be able to read from it.
                if access.contains(Sa::STORE) {
                    usage |= Bu::COPY_SRC;
                }

                usage
            }

            naga::AddressSpace::Handle
            | naga::AddressSpace::PushConstant
            | naga::AddressSpace::Function
            | naga::AddressSpace::Private
            | naga::AddressSpace::WorkGroup => todo!("error message using spans from statement"),
        };

 */
