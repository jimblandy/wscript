#![allow(unreachable_code)]

use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::Arc;

use crate::{ast, plan};
use anyhow::anyhow;
use pollster::block_on;

mod error;

pub use error::{Error, ErrorKind, IntoRunResult, RunResult};

/// An execution context for wscript programs.
#[derive(Debug)]
pub struct Context {
    /// The `wgpu` device to use for execution.
    pub device: wgpu::Device,

    /// The `wgpu` queue to use for submitting requests to the GPU.
    pub queue: wgpu::Queue,

    /// The current default module, if any.
    pub module: Option<Module>,

    /// Buffers we've created so far.
    pub buffers: HashMap<naga::Handle<naga::GlobalVariable>, Buffer>,
}

#[derive(Debug)]
struct Module {
    wgpu: wgpu::ShaderModule,

    /// Information about the module determined at script planning
    /// time (i.e., static information).
    planned: Arc<plan::Module>,
}

impl Context {
    pub fn create(label: &str) -> anyhow::Result<Context> {
        let backends = wgpu::util::backend_bits_from_env().unwrap_or_else(wgpu::Backends::all);
        let instance = wgpu::Instance::new(backends);
        let adapter = block_on(wgpu::util::initialize_adapter_from_env_or_default(
            &instance, backends, None,
        ))
        .ok_or_else(|| anyhow!("couldn't find suitable wgpu adapter"))?;

        let device_descriptor = wgpu::DeviceDescriptor {
            label: Some(label),
            features: wgpu::Features::default(),
            limits: wgpu::Limits::default(),
        };
        let (device, queue) = block_on(adapter.request_device(&device_descriptor, None))?;

        Ok(Context {
            device,
            queue,
            module: None,
            buffers: HashMap::new(),
        })
    }

    fn run_stmt(&mut self, stmt: &ast::Statement) -> RunResult<()> {
        match stmt.kind {
            ast::StatementKind::Module { ref wgsl } => todo!(),
            ast::StatementKind::Init {
                ref buffer,
                ref value,
            } => self.run_init(buffer, value, &stmt.span),
            ast::StatementKind::Dispatch {
                ref entry_point,
                ref count,
            } => self.run_dispatch(entry_point, count),
            ast::StatementKind::Check {
                ref buffer,
                ref value,
            } => self.run_check(buffer, value),
        }
    }

    pub fn run_module(&mut self, planned: &Arc<plan::Module>) -> RunResult<()> {
        let wgpu = self
            .device
            .create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some(&format!(
                    "wscript module defined at bytes {}..{}",
                    planned.definition.1.start, planned.definition.1.end
                )),
                source: wgpu::ShaderSource::Naga(Cow::Borrowed(&planned.naga)),
            });
        self.module = Some(Module {
            wgpu,
            planned: planned.clone(),
        });
        Ok(())
    }

    fn run_init(
        &mut self,
        buffer: &ast::BufferId,
        _value: &ast::Expression,
        stmt: &ast::Span,
    ) -> RunResult<()> {
        let module = match self.module {
            Some(ref m) => m,
            None => {
                return Err(Error {
                    kind: todo!(),
                    span: stmt.clone(),
                })
            }
        };

        // Find the global to which `buffer` refers.
        let global = module.planned.find_buffer(buffer)?;

        // Create the buffer, if necessary.
        if let std::collections::hash_map::Entry::Vacant(e) = self.buffers.entry(global) {
            let buffer = Buffer::new(&self.device, module, global, true)
                .at(stmt, "creating buffer for this `buffer` statement")?;
            e.insert(buffer);
        }

        let buffer = self.buffers.get(&global).unwrap();
        let slice = buffer
            .wait_until_mapped(self, .., wgpu::MapMode::Write)
            .at(stmt, "waiting to map buffer")?;
        let view = slice.get_mapped_range_mut();

        todo!()
    }

    fn run_dispatch(
        &mut self,
        _entry_point: &ast::EntryPoint,
        _count: &ast::WorkgroupCount,
    ) -> RunResult<()> {
        todo!()
    }

    fn run_check(&mut self, _buffer: &ast::BufferId, _value: &ast::Expression) -> RunResult<()> {
        todo!()
    }
}

/// A buffer we've created in a wscript program.
#[derive(Debug)]
pub struct Buffer {
    /// The underlying `wgpu` buffer.
    wgpu: wgpu::Buffer,

    /// The handle of its `GlobalVariable` in the current module.
    global: naga::Handle<naga::GlobalVariable>,

    /// True if currently mapped.
    mapped: bool,
}

impl Buffer {
    fn new(
        device: &wgpu::Device,
        module: &Module,
        global: naga::Handle<naga::GlobalVariable>,
        mapped_at_creation: bool,
    ) -> anyhow::Result<Buffer> {
        use naga::StorageAccess as Sa;
        use wgpu::BufferUsages as Bu;

        let naga = &module.planned.naga;

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

        let descriptor = wgpu::BufferDescriptor {
            label: var.name.as_deref(),
            size,
            usage,
            mapped_at_creation,
        };

        let wgpu = device.create_buffer(&descriptor);
        Ok(Buffer {
            wgpu,
            global,
            mapped: mapped_at_creation,
        })
    }

    pub fn wait_until_mapped<S>(
        &self,
        ctx: &Context,
        range: S,
        mode: wgpu::MapMode,
    ) -> anyhow::Result<wgpu::BufferSlice>
    where
        S: std::ops::RangeBounds<wgpu::BufferAddress>,
    {
        let slice = self.wgpu.slice(range);
        let error = std::sync::Arc::new(std::sync::Mutex::new(None));
        slice.map_async(mode, {
            let error = error.clone();
            move |result| {
                if let Err(e) = result {
                    *error.lock().unwrap() = Some(e);
                }
            }
        });
        ctx.device.poll(wgpu::Maintain::Wait);

        if let Some(error) = error.lock().unwrap().take() {
            return Err(error.into());
        }

        Ok(slice)
    }
}
