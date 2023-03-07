#![allow(unreachable_code, unused_variables, dead_code)]

use std::borrow::Cow;

use crate::{ast, plan};
use anyhow::anyhow;
use pollster::block_on;

mod error;

pub use error::{Error, ErrorKind, IntoRunResult, Result};

/// Execution context for wscript programs.
#[derive(Debug)]
pub struct Context {
    /// Statically computed information needed at runtime, that can't be
    /// captured in the plans.
    pub summary: plan::Summary,

    /// The `wgpu` device to use for execution.
    pub device: wgpu::Device,

    /// The `wgpu` queue to use for submitting requests to the GPU.
    pub queue: wgpu::Queue,

    /// The current module.
    pub module: Option<wgpu::ShaderModule>,

    /// Buffers we've created so far.
    ///
    /// Indices parallel those in [`Planner::buffers`].
    ///
    /// [`Planner::buffers`]: crate::plan::PLanner::buffers
    pub buffers: Vec<Buffer>,
}

impl Context {
    pub fn create(summary: plan::Summary, label: &str) -> anyhow::Result<Context> {
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
            summary,
            device,
            queue,
            module: None,
            buffers: Vec::new(),
        })
    }

    pub fn expect_module(&self) -> &wgpu::ShaderModule {
        self.module
            .as_ref()
            .expect("No Naga module established. This should have been a static error.")
    }

    pub fn run_module(&mut self, planned: &plan::Module) -> Result<()> {
        let wgpu = self
            .device
            .create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some(&format!(
                    "wscript module defined at bytes {}..{}",
                    planned.definition.1.start, planned.definition.1.end
                )),
                source: wgpu::ShaderSource::Naga(Cow::Owned(planned.naga.clone())),
            });

        self.module = Some(wgpu);
        Ok(())
    }

    pub fn run_create_buffer(
        &mut self,
        buffer_index: usize,
        id: ast::BufferId,
        value: &dyn plan::ByteSource,
    ) -> Result<()> {
        // Newly created buffers should always be at the end of the array.
        assert_eq!(buffer_index, self.buffers.len());
        let label = id.kind.to_string();
        let desc = wgpu::BufferDescriptor {
            label: Some(&label),
            size: value.len() as wgpu::BufferAddress,
            usage: self.summary.buffer_usage[buffer_index],
            mapped_at_creation: true,
        };
        self.buffers.push(Buffer::new(&self.device, &desc, id));
        Ok(())
    }

    pub fn run_init_buffer(
        &mut self,
        buffer_index: usize,
        mut value: Box<dyn plan::ByteSource>,
        span: &ast::Span,
    ) -> Result<()> {
        let buffer = &self.buffers[buffer_index];
        let slice = buffer
            .wait_until_mapped(self, .., wgpu::MapMode::Write)
            .at(span, "mapping buffer for initialization")?;
        let mut view = slice.get_mapped_range_mut();
        value.fill(&mut view, 0).map_err(|inner| Error {
            span: span.clone(),
            kind: ErrorKind::Init {
                inner: Box::new(inner),
                buffer: buffer.id.kind.to_string(),
            },
        })
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

/// A buffer we've created in a wscript program.
#[derive(Debug)]
pub struct Buffer {
    /// The underlying `wgpu` buffer.
    wgpu: wgpu::Buffer,

    /// True if currently mapped.
    mapped: bool,

    /// The way the buffer was named when it was first initialized.
    id: ast::BufferId,
}

impl Buffer {
    fn new(
        device: &wgpu::Device,
        descriptor: &wgpu::BufferDescriptor,
        id: ast::BufferId,
    ) -> Buffer {
        let wgpu = device.create_buffer(descriptor);
        Buffer {
            wgpu,
            mapped: descriptor.mapped_at_creation,
            id,
        }
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
