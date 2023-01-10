//! Convert an abstract syntax tree to an executable form, with type checks.
//!
//! "Elaboration" is the process of type-checking a wscript program,
//! and if type checking succeeds, producing an execution plan for it.

use crate::ast;
use super::StatementPlan;
use super::module::Module;

use super::error::IntoRunResult as _;

/// Information 
struct StaticContext {
}

pub fn elaborate(program: &ast::Program) -> Result<Box<StatementPlan>, Error> {
    todo!()
}

impl ast::Statement {
    fn elaborate(&self) -> Result<Box<StatementPlan>, Error> {
        match self.kind {
            ast::StatementKind::Module { ref wgsl } => self.elaborate_module(wgsl),
            ast::StatementKind::Init { ref buffer, ref value } => todo!(),
            ast::StatementKind::Dispatch { ref entry_point, ref count } => todo!(),
            ast::StatementKind::Check { ref buffer, ref value } => todo!(),
        }
    }

    fn elaborate_module(&self, wgsl: &ast::Wgsl) -> Result<Box<StatementPlan>, Error> {
        let module = Module::new(self, &wgsl.text)
            .at(&wgsl.span, "for this `module` statement")?;

        let module = Arc::new(module);
        Ok(Box::new(move |ctx| {
            let wgpu = ctx.device.create_shader_module(descriptor);
            ctx.module = Some(todo!());
        }))
    }
}

impl ast::Expression {
    //fn elaborate(&self, 
}

struct Error;
