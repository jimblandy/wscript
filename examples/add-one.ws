module """

    @group(0) @binding(0)
    var<storage,read_write> buf: array<i32, 4096>;

    @group(0) @binding(1)
    var<storage,read_write> vectors: array<vec4<f32>, 4096>;

    @group(0) @binding(2)
    var<uniform> matr: mat4x4<f32>;
    
    @compute
    @workgroup_size(64,1,1)
    fn transform(@builtin(global_invocation_id) i: vec3<u32>) {
        vectors[i.x] = matr * vectors[i.x];
    }
    
    @compute
    @workgroup_size(64,1,1)
    fn add_one(@builtin(global_invocation_id) i: vec3<u32>) {
        buf[i.x] += 1;
    }
    
    @compute
    @workgroup_size(64,1,1)
    fn subtract_one(@builtin(global_invocation_id) i: vec3<u32>) {
        buf[i.x] -= 1;
    }

init buf = 0 .. 4096
dispatch add_one (64)
check buf = 1 .. 4097
dispatch subtract_one (64,1,1)
check buf = 0 .. 4096
