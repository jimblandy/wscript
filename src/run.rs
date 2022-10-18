use std::borrow::Cow;
use std::collections::HashMap;

use crate::{ast, error};
use anyhow::{anyhow, Result};
use pollster::block_on;

pub fn run(program: ast::Program, cache: error::Cache) -> Result<()> {
    let mut context = Context::create(cache, "wscript initial device")?;
    context.run_stmts(&program)
}

/// An execution context for wscript programs.
#[derive(Debug)]
pub struct Context {
    /// The `wgpu` device to use for execution.
    pub device: wgpu::Device,

    /// The `wgpu` queue to use for submitting requests to the GPU.
    pub queue: wgpu::Queue,

    /// The source cache to use to report errors.
    pub cache: error::Cache,

    /// The current default module, if any.
    pub module: Option<Module>,

    /// Buffers we've created so far.
    pub buffers: HashMap<naga::Handle<naga::GlobalVariable>, Buffer>,
}

impl Context {
    pub fn create(cache: error::Cache, label: &str) -> Result<Context> {
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
            cache,
            module: None,
            buffers: HashMap::new(),
        })
    }

    fn run_stmts(&mut self, stmts: &[ast::Statement]) -> Result<()> {
        for stmt in stmts {
            self.run_stmt(stmt)?
        }

        Ok(())
    }

    fn run_stmt(&mut self, stmt: &ast::Statement) -> Result<()> {
        match stmt.kind {
            ast::StatementKind::Module { ref wgsl } => self.run_module(wgsl),
            ast::StatementKind::Init {
                ref buffer,
                ref value,
            } => self.run_init(buffer, value),
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

    fn run_module(&mut self, wgsl: &ast::Wgsl) -> Result<()> {
        self.module = Some(Module::new(self, &wgsl.text)?);
        Ok(())
    }

    fn run_init(&mut self, buffer: &ast::BufferId, _value: &ast::Expression) -> Result<()> {
        let module = match self.module {
            Some(ref m) => m,
            None => todo!("error: no module set yet"),
        };

        // Find the global to which `buffer` refers.
        let global = module.find_buffer(buffer)?;

        // Create the buffer, if necessary.
        if let std::collections::hash_map::Entry::Vacant(e) = self.buffers.entry(global) {
            let buffer = Buffer::new(&self.device, module, global, true)?;
            e.insert(buffer);
        }
        
        let buffer = self.buffers.get_mut(&global);

        todo!()
    }

    fn run_dispatch(
        &mut self,
        _entry_point: &ast::EntryPoint,
        _count: &ast::WorkgroupCount,
    ) -> Result<()> {
        todo!()
    }

    fn run_check(&mut self, _buffer: &ast::BufferId, _value: &ast::Expression) -> Result<()> {
        todo!()
    }
}

#[derive(Debug)]
pub struct Module {
    pub naga: naga::Module,
    pub info: naga::valid::ModuleInfo,
    pub wgpu: wgpu::ShaderModule,
}

impl Module {
    fn new(ctx: &mut Context, source: &str) -> Result<Module> {
        let naga = naga::front::wgsl::parse_str(source)?;

        let mut validator = naga::valid::Validator::new(
            naga::valid::ValidationFlags::default(),
            naga::valid::Capabilities::default(),
        );
        let info = validator.validate(&naga)?;

        let descriptor = wgpu::ShaderModuleDescriptor {
            label: None,
            source: wgpu::ShaderSource::Naga(Cow::Owned(naga.clone())),
        };

        let wgpu = ctx.device.create_shader_module(descriptor);

        Ok(Module { naga, info, wgpu })
    }

    /// Return the global variable to which `id` refers, if any.
    fn find_buffer(&self, id: &ast::BufferId) -> Result<naga::Handle<naga::GlobalVariable>> {
        let error_span = match *id {
            ast::BufferId::Name { ref id, ref span } => {
                for (handle, var) in self.naga.global_variables.iter() {
                    if var.name.as_ref() == Some(id) {
                        return Ok(handle);
                    }
                }
                span.clone()
            }
            ast::BufferId::Binding { ref naga, ref span } => {
                for (handle, var) in self.naga.global_variables.iter() {
                    if var.binding.as_ref() == Some(naga) {
                        return Ok(handle);
                    }
                }
                span.clone()
            }
        };

        let _ = error_span;
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
    ) -> Result<Buffer> {
        use wgpu::BufferUsages as Bu;
        use naga::StorageAccess as Sa;

        let naga = &module.naga;

        // Compute the buffer's size from its type.
        let var = &naga.global_variables[global];
        let inner = &naga.types[var.ty].inner;
        let size = wgpu::BufferAddress::try_from(inner.try_size(&naga.constants)?)?;

        // Guess a usage for the buffer from its address space.
        let usage = match var.space{ 
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
            },

            naga::AddressSpace::Handle |
            naga::AddressSpace::PushConstant |
            naga::AddressSpace::Function |
            naga::AddressSpace::Private |
            naga::AddressSpace::WorkGroup => todo!("error message using spans from statement"),
        };

        let descriptor = wgpu::BufferDescriptor {
            label: var.name.as_deref(),
            size,
            usage,
            mapped_at_creation
        };

        let wgpu = device.create_buffer(&descriptor);
        Ok(Buffer { wgpu, global, mapped: mapped_at_creation })
    }
}
