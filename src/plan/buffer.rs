//! Statitally known information about GPU buffers.

pub struct Buffer {
    global: naga::Handle<naga::GlobalVariable>,
    usage: wgpu::BufferUsages,
}
