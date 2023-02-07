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
mod buffer;

use crate::ast;
use crate::run;
use error::IntoPlanResult as _;
pub use error::{Error, ErrorKind, Result};
pub use module::Module;

use indexmap::IndexSet;
use std::sync::Arc;

/// A source of data to initialize a GPU buffer, or check its contents.
pub trait ByteSource {
    /// Return the remaining number of bytes this plan will generate.
    fn len(&self) -> usize;

    /// Fill `buf` with the next `buf.len()` bytes from this source.
    fn fill(&mut self, buf: &mut [u8]) -> run::Result<()>;

    /// Check `buf` against the next `buf.len()` bytes from this source.
    fn check(&mut self, buf: &[u8]) -> run::Result<Comparison>;
}

/// A plan that can construct a [`ByteSource`]
pub type BytesPlan = dyn Fn(&mut run::Context) -> run::Result<Box<dyn ByteSource + 'static>> + 'static;

pub enum Comparison {
    /// This plan's bytes have the given length, and the corresponding
    /// prefix of the input matches.
    Matches { len: usize },
 
    /// This plan's bytes do not match the input, at the given byte offset.
    Mismatch { pos: usize },
}

/// A `Plan` is a closure that runs a script, given a [`Context`].
/// It may fail, returning a `run::Error`.
///
/// [`Context`]: run::Context
pub type Plan = dyn Fn(&mut run::Context) -> run::Result<()> + 'static;

/// Information used by plans that we can't know until we've seen the
/// whole script.
///
/// Some information, like buffer usage flags, can't be determined
/// without looking over the entire script. We could make one pass
/// over the script to gather this information, and then make a second
/// pass where we build execution plans, but that means a bunch of
/// decisions need to be either repeated or recorded, and that's a
/// pain. Alternatively, we could use interior mutability to go back
/// and update data captured by plans after we've constructed them,
/// but that's not very Rustic.
///
/// Instead, planning makes a single pass that accumulates this data,
/// and constructs plans. Then, this data is returned along with the
/// plan, and incorporated into the `run::Context`, from which plans
/// retrieve it.
#[derive(Debug, Default)]
pub struct Summary {
    /// Usage flags for each buffer.
    pub buffer_usage: Vec<wgpu::BufferUsages>,
}

/// Context for planning script execution.
///
/// This holds static information about the current statement being
/// planned.
struct Planner {
    /// Static information about the current Naga module, if any.
    module: Option<Arc<Module>>,

    /// A map from a buffer's global handle to its index in [`run::Context::buffers`].
    buffers: IndexSet<naga::Handle<naga::GlobalVariable>>,

    /// Information we're accumulating about the script.
    summary: Summary,
}

pub fn plan(
    program: &ast::Program,
    source_id: usize,
) -> Result<(Box<Plan>, Summary)> {
    Planner::plan(program, source_id)
}

impl Planner {
    fn plan(
        program: &ast::Program,
        source_id: usize,
    ) -> Result<(Box<Plan>, Summary)> {
        let mut planner = Self {
            module: None,
            buffers: Default::default(),
            summary: Default::default(),
        };

        let plans = program
            .iter()
            .map(|statement| planner.plan_statement(statement))
            .collect::<Result<Vec<_>>>()?;

        let plan = Box::new(move |ctx: &mut run::Context| {
            for plan in plans.iter() {
                plan(ctx)?;
            }
            Ok(())
        });
        Ok((plan, planner.summary))
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
        buffer_id: &ast::BufferId,
        value: &ast::Expression,
    ) -> Result<Box<Plan>> {
        let module = self.require_module(statement)?.clone();
        let types = &module.naga.types;
        let buffer = module.find_buffer(buffer_id)?;
        let buffer_global = &module.naga.global_variables[buffer];

        let value_plan = self.plan_expression(value, &module, buffer_global.ty)?;

        // Find the buffer's index in `run::Context::buffers`,
        // assigning it a new one if needed.
        let (index, prior) = self.buffers.insert_full(buffer);
        
        let plan: Box<Plan> = if prior {
            // We've used this buffer already, so we don't need to
            // create it, just fill it with the given value.
            Box::new(move |ctx: &mut run::Context| {
                let bytes = value_plan(ctx)?;
                ctx.run_init_buffer(index, bytes)
            })
        } else {
            // We've never used this buffer before, so create it first
            // and save it in `Context::buffers`, and then initialize
            // it.

            // Choose the initial usage flags for the buffer. We'll
            // add to these as we find other uses of the buffer in the
            // script.
            use wgpu::BufferUsages as Bu;
            let usage = match buffer_global.space {
                naga::AddressSpace::Uniform => Bu::UNIFORM,
                naga::AddressSpace::Storage { access } => Bu::STORAGE,
                naga::AddressSpace::Function |
                naga::AddressSpace::Private |
                naga::AddressSpace::WorkGroup |
                naga::AddressSpace::Handle |
                naga::AddressSpace::PushConstant => {
                    Bu::empty()
                }
            };
            assert_eq!(index, self.summary.buffer_usage.len());
            self.summary.buffer_usage.push(usage);
            
            let label = buffer_id.kind.to_string();
            Box::new(move |ctx: &mut run::Context| {
                let bytes = value_plan(ctx)?;
                let descriptor = wgpu::BufferDescriptor {
                    label: Some(&label),
                    size: bytes.len() as wgpu::BufferAddress,
                    usage: ctx.summary.buffer_usage[index],
                    mapped_at_creation: true,
                };

                ctx.run_create_buffer(index, &descriptor)?;
                ctx.run_init_buffer(index, bytes)
            })
        };

        Ok(plan)
    }

    fn plan_expression(
        &mut self,
        expr: &ast::Expression,
        module: &Module,
        expected_type: naga::Handle<naga::Type>,
    ) -> Result<Box<BytesPlan>> {
        todo!()
    }

    fn require_module(&self, victim: &ast::Statement) -> Result<&Arc<Module>> {
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
