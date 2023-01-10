//! Shader module planning.

use super::{error, PlanResult};
use crate::ast::{self, Span};

/// Static information about a WebGPU module.
#[derive(Debug)]
pub struct Module {
    pub naga: naga::Module,
    pub info: naga::valid::ModuleInfo,
    pub definition: Span,
}

impl Module {
    pub fn new(source: &str, definition: Span) -> anyhow::Result<Module> {
        let naga = naga::front::wgsl::parse_str(source)?;

        let mut validator = naga::valid::Validator::new(
            naga::valid::ValidationFlags::default(),
            naga::valid::Capabilities::default(),
        );
        let info = validator.validate(&naga)?;

        Ok(Module {
            naga,
            info,
            definition,
        })
    }

    /// Return the global variable to which `id` refers, if any.
    pub fn find_buffer(
        &self,
        id: &ast::BufferId,
    ) -> PlanResult<naga::Handle<naga::GlobalVariable>> {
        match id.kind {
            ast::BufferIdKind::Name(ref name) => {
                for (handle, var) in self.naga.global_variables.iter() {
                    if var.name.as_ref() == Some(name) {
                        return Ok(handle);
                    }
                }
            }
            ast::BufferIdKind::Binding(ref naga) => {
                for (handle, var) in self.naga.global_variables.iter() {
                    if var.binding.as_ref() == Some(naga) {
                        return Ok(handle);
                    }
                }
            }
        };

        Err(error::Error {
            span: reference.clone(),
            kind: error::ErrorKind::NoSuchBuffer {
                module: self.definition.clone(),
                buffer_id: id.clone(),
            },
        })
    }
}