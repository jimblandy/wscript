use std::borrow::Cow;

use crate::{ast, plan};
use anyhow::anyhow;
use pollster::block_on;
use std::fmt;

mod error;

pub use error::{Error, ErrorKind, ExprError, ExprErrorKind, ExprResult, IntoRunResult, Result};

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

    /// The command encoder to which we're currently contributing
    /// commands.
    pub commands: Option<wgpu::CommandEncoder>,

    /// Buffers for `module`'s globals.
    ///
    /// Indices parallel those in [`Planner::global_buffers`].
    ///
    /// [`Planner::buffers`]: crate::plan::Planner::buffers
    pub global_buffers: Vec<Buffer>,

    /// A readback buffer.
    pub readback_buffer: Option<Buffer>,
}

/// Runtime information about a WebGPU buffer.
#[derive(Debug)]
pub struct Buffer {
    /// The underlying `wgpu` buffer.
    wgpu: wgpu::Buffer,

    /// True if currently mapped.
    mapped: bool,

    /// The way the buffer was named when it was first initialized.
    id: RunBufferId,
}

#[derive(Debug)]
enum RunBufferId {
    Readback,
    Global(ast::BufferId),
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
            commands: None,
            global_buffers: Vec::new(),
            readback_buffer: None,
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
        assert_eq!(buffer_index, self.global_buffers.len());
        let label = id.kind.to_string();
        let desc = wgpu::BufferDescriptor {
            label: Some(&label),
            size: value.len() as wgpu::BufferAddress,
            usage: self.summary.buffer_usage[buffer_index],
            mapped_at_creation: true,
        };
        self.global_buffers
            .push(Buffer::new(&self.device, &desc, RunBufferId::Global(id)));
        Ok(())
    }

    pub fn run_init_buffer(
        &mut self,
        buffer_index: usize,
        mut value: Box<dyn plan::ByteSource>,
        span: &ast::Span,
    ) -> Result<()> {
        let buffer = &mut self.global_buffers[buffer_index];
        let slice = buffer
            .wait_until_mapped(&self.device, .., wgpu::MapMode::Write)
            .at(span, "mapping buffer for initialization")?;
        let result = {
            let mut view = slice.get_mapped_range_mut();
            value.fill(&mut view, 0)
        };
        if let Err(inner) = result {
            return Err(Error {
                span: span.clone(),
                kind: ErrorKind::InitExpression {
                    inner: Box::new(inner),
                    buffer: buffer.id.to_string(),
                },
            });
        }

        buffer.unmap();

        Ok(())
    }

    pub fn run_check_buffer(
        &mut self,
        buffer_index: usize,
        mut value: Box<dyn plan::ByteSource>,
        span: &ast::Span,
    ) -> Result<()> {
        self.copy_to_readback_buffer(buffer_index);
        let submission_index = self.flush_commands();
        self.device
            .poll(wgpu::Maintain::WaitForSubmissionIndex(submission_index));

        let readback_buffer = self.readback_buffer.as_mut().unwrap();
        let slice = readback_buffer
            .wait_until_mapped(&self.device, .., wgpu::MapMode::Read)
            .at(span, "mapping readback buffer for checking")?;
        let result = {
            let view = slice.get_mapped_range();
            value.compare(&view, 0)
        };
        match result {
            Err(inner) => {
                let global_buffer = &self.global_buffers[buffer_index];
                Err(Error {
                    span: span.clone(),
                    kind: ErrorKind::Check {
                        inner: Box::new(inner),
                        buffer: global_buffer.id.to_string(),
                    },
                })
            }
            Ok(plan::Comparison::Matches { .. }) => Ok(()),
            Ok(plan::Comparison::Mismatch { offset }) => {
                let global_buffer = &self.global_buffers[buffer_index];
                Err(Error {
                    span: span.clone(),
                    kind: ErrorKind::CheckFailed {
                        buffer: global_buffer.id.to_string(),
                        offset,
                    },
                })
            }
        }
    }

    /// Copy the global var buffer at `buffer_index` to the readback buffer.
    ///
    /// It would be convenient if this could return a mutable
    /// reference to the readback buffer, since it ensures it exists,
    /// but the borrow checker would think we'd mutably borrowed all
    /// of `self`.
    fn copy_to_readback_buffer(&mut self, buffer_index: usize) {
        self.ensure_commands();
        self.ensure_readback_buffer(buffer_index);

        let encoder = self.commands.as_mut().unwrap();
        let source_buffer = &self.global_buffers[buffer_index].wgpu;
        let readback_buffer = &self.readback_buffer.as_ref().unwrap().wgpu;

        encoder.copy_buffer_to_buffer(source_buffer, 0, readback_buffer, 0, source_buffer.size());
    }

    /// Ensure that we have a readback buffer large enough for the buffer at `buffer_index`.
    fn ensure_readback_buffer(&mut self, buffer_index: usize) {
        use wgpu::BufferUsages as Bu;

        let needed = self.global_buffers[buffer_index].wgpu.size();
        if let Some(ref buf) = self.readback_buffer {
            if buf.wgpu.size() >= needed {
                return;
            }
        }

        let desc = wgpu::BufferDescriptor {
            label: Some("wscript readback buffer"),
            size: needed,
            usage: Bu::COPY_DST | Bu::MAP_READ,
            mapped_at_creation: false,
        };
        self.readback_buffer = Some(Buffer::new(&self.device, &desc, RunBufferId::Readback));
    }

    /// Ensure that we have a command encoder ready to use.
    fn ensure_commands(&mut self) {
        if self.commands.is_none() {
            let desc = wgpu::CommandEncoderDescriptor {
                label: Some("wscript default command encoder"),
            };
            self.commands = Some(self.device.create_command_encoder(&desc));
        }
    }

    fn flush_commands(&mut self) -> wgpu::SubmissionIndex {
        let command_buffer = self.commands.take().map(|encoder| encoder.finish());
        self.queue.submit(command_buffer)
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        // Without this, we get complaints from gpu-alloc, used by the
        // Vulkan backend to manage buffers:
        //
        //     Memory block wasn't deallocated
        //     Not all blocks were deallocated
        //
        // because we never call `gpu_alloc::GpuAllocator::dealloc` on
        // the `wgpu_hal::vulkan::Buffer`'s `gpu_alloc::MemoryBlock`.
        //
        // I'm a little surprised that dropping the `wgpu::Device`
        // doesn't take care of this for me.
        for buffer in self
            .global_buffers
            .iter()
            .chain(self.readback_buffer.iter())
        {
            buffer.wgpu.destroy();
        }
    }
}

impl Buffer {
    fn new(device: &wgpu::Device, descriptor: &wgpu::BufferDescriptor, id: RunBufferId) -> Buffer {
        let wgpu = device.create_buffer(descriptor);
        Buffer {
            wgpu,
            mapped: descriptor.mapped_at_creation,
            id,
        }
    }

    pub fn wait_until_mapped<S>(
        &mut self,
        device: &wgpu::Device,
        range: S,
        mode: wgpu::MapMode,
    ) -> anyhow::Result<wgpu::BufferSlice>
    where
        S: std::ops::RangeBounds<wgpu::BufferAddress>,
    {
        let slice = self.wgpu.slice(range);

        if !self.mapped {
            let error = std::sync::Arc::new(std::sync::Mutex::new(None));
            slice.map_async(mode, {
                let error = error.clone();
                move |result| {
                    if let Err(e) = result {
                        *error.lock().unwrap() = Some(e);
                    }
                }
            });
            device.poll(wgpu::Maintain::Wait);

            if let Some(error) = error.lock().unwrap().take() {
                return Err(error.into());
            }

            self.mapped = true;
        }

        Ok(slice)
    }

    pub fn unmap(&self) {
        self.wgpu.unmap();
    }
}

impl fmt::Display for RunBufferId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            RunBufferId::Readback => write!(f, "readback buffer"),
            RunBufferId::Global(ref id) => write!(f, "{}", id.kind),
        }
    }
}
