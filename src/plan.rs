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

#![allow(unreachable_code, unused_variables, dead_code)]

mod error;
mod expr;
mod little_endian_bytes;
mod module;

use crate::ast;
use crate::run;
use error::IntoPlanResult as _;
pub use error::{Error, ErrorKind, Result};
pub use expr::ByteSource;
pub use expr::Error as ExprError;
use indexmap::map::Entry;
use little_endian_bytes::LeBytes;
pub use module::Module;

use indexmap::IndexMap;
use std::sync::Arc;

/// A `Plan` is a closure that runs a script, given a [`Context`].
/// It may fail, returning a `run::Error`.
///
/// [`Context`]: run::Context
pub type Plan = dyn Fn(&mut run::Context) -> run::Result<()> + 'static;

/// Data accumulated over the course of planning the entire scripts.
///
/// Planning a particular operation sometimes needs information that
/// can only be determined by looking over the entire script. For
/// example, to create a buffer, we need usage flags that cover
/// everything the rest of the script may do with it. There are a few
/// approaches:
///
/// - We could make an initial pass over the script to gather that
///   data, and then consult it when we build execution plans. But
///   that means a bunch of decisions need to be either repeated or
///   recorded, and that's a pain.
///
/// - We could build execution plans, and then mutate them as we
///   discover more details about what they need to do. But this isn't
///   very Rustic.
///
/// - What we actually do is build this `Summary` along with the
///   execution plans, and then pass the completed `Summary` to the
///   plans in the `Context` and have the plans get the data they need
///   from there.
#[derive(Debug, Default)]
pub struct Summary {
    /// Usage flags for each buffer.
    ///
    /// Indices parallel those in [`Planner::buffers`].
    pub buffer_usage: Vec<wgpu::BufferUsages>,
}

/// Context for planning script execution.
struct Planner {
    /// Static information about the current Naga module, if any.
    module: Option<Arc<Module>>,

    /// A map from a buffer's global handle to planning information about it.
    ///
    /// This index is used in tables like [`run::Context::buffers`]
    /// and [`Summary::buffer_usage`].
    buffers: IndexMap<naga::Handle<naga::GlobalVariable>, Buffer>,
}

/// Planning information about GPU buffers.
struct Buffer {
    global: naga::Handle<naga::GlobalVariable>,
    ty: naga::Handle<naga::Type>,
    usage: wgpu::BufferUsages,
}

pub fn plan(program: &ast::Program, source_id: usize) -> Result<(Box<Plan>, Summary)> {
    Planner::plan(program, source_id)
}

impl Planner {
    fn plan(program: &ast::Program, source_id: usize) -> Result<(Box<Plan>, Summary)> {
        let mut planner = Self {
            module: None,
            buffers: Default::default(),
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

        let summary = Summary {
            buffer_usage: planner
                .buffers
                .iter()
                .map(|(index, buffer)| buffer.usage)
                .collect(),
        };

        Ok((plan, summary))
    }

    fn plan_statement(&mut self, statement: &ast::Statement) -> Result<Box<Plan>> {
        match statement.kind {
            ast::StatementKind::Module { ref wgsl } => self.plan_module(statement, wgsl),
            ast::StatementKind::Init {
                ref buffer,
                ref value,
            } => self.plan_init(statement, buffer.clone(), value),
            ast::StatementKind::Dispatch {
                ref entry_point,
                ref count,
            } => todo!(),
            ast::StatementKind::Check {
                ref buffer,
                ref value,
            } => todo!(),
        }
    }

    fn plan_module(&mut self, statement: &ast::Statement, wgsl: &ast::Wgsl) -> Result<Box<Plan>> {
        let module = Module::new(&wgsl.text, statement.span.clone())
            .at(&statement.span, "in this `module` statement")?;
        let module = Arc::new(module);
        self.module = Some(module.clone());
        Ok(Box::new(move |ctx| ctx.run_module(&module)))
    }

    fn plan_init(
        &mut self,
        statement: &ast::Statement,
        buffer_id: ast::BufferId,
        value: &ast::Expression,
    ) -> Result<Box<Plan>> {
        let module = self.require_module(statement)?.clone();
        let handle = module.find_buffer_global(&buffer_id)?;
        let global = &module.naga.global_variables[handle];
        let bytes_plan = expr::plan_expression_bytes(self, value, &module, global.ty)
            .map_err(|inner| todo!())?;

        let span = statement.span.clone();
        Ok(match self.buffers.entry(handle) {
            Entry::Occupied(occupied) => {
                // We've already created this buffer, we're just overwriting its
                // contents.
                let buffer_index = occupied.index();
                Box::new(move |ctx: &mut run::Context| {
                    let bytes = bytes_plan(ctx).map_err(|inner| run::Error {
                        span: span.clone(),
                        kind: run::ErrorKind::Init {
                            inner: Box::new(inner),
                            buffer: buffer_id.kind.to_string(),
                        },
                    })?;
                    ctx.run_init_buffer(buffer_index, bytes, &span)
                })
            }
            Entry::Vacant(vacant) => {
                let buffer_index = vacant.index();

                // We're creating and initializing the buffer.
                //
                // Choose initial usage flags for the buffer. As we process the
                // rest of the script, we'll add more flags as we learn more
                // about how the buffer gets used.
                vacant.insert(Buffer {
                    global: handle,
                    ty: global.ty,
                    usage: initial_buffer_usage_from_space(global.space),
                });

                Box::new(move |ctx: &mut run::Context| {
                    let bytes: Box<dyn expr::ByteSource + 'static> =
                        bytes_plan(ctx).map_err(|inner| run::Error {
                            span: span.clone(),
                            kind: run::ErrorKind::Init {
                                inner: Box::new(inner),
                                buffer: buffer_id.kind.to_string(),
                            },
                        })?;
                    ctx.run_create_buffer(buffer_index, buffer_id.clone(), &*bytes)?;

                    ctx.run_init_buffer(buffer_index, bytes, &span)
                })
            }
        })
    }

    fn require_module(&self, victim: &ast::Statement) -> Result<&Arc<Module>> {
        match self.module {
            Some(ref module) => Ok(module),
            None => Err(Error {
                kind: ErrorKind::NoModule {
                    victim: error::StatementKind::from_ast(victim),
                },
                span: victim.span.clone(),
            }),
        }
    }
}

/// Given a buffer's address space, choose its initial usage flags.
fn initial_buffer_usage_from_space(space: naga::AddressSpace) -> wgpu::BufferUsages {
    use naga::StorageAccess as Sa;
    use wgpu::BufferUsages as Bu;

    match space {
        naga::AddressSpace::Uniform => Bu::UNIFORM,
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
        naga::AddressSpace::Function
        | naga::AddressSpace::Private
        | naga::AddressSpace::WorkGroup
        | naga::AddressSpace::Handle
        | naga::AddressSpace::PushConstant => Bu::empty(),
    }
}
