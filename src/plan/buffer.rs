//! Statitally known information about GPU buffers.

pub struct Buffer {
    global: naga::Handle<naga::GlobalVariable>,
    size: usize,
    usage: wgpu::BufferUsages,
    
}

