module """

    @group(0) @binding(0)
    var<storage> buf: array<i32, 4096>;

    @group(0) @binding(1)
    var<storage> vectors: array<vec4<f32>, 4096>;

    @group(0) @binding(2)
    var<uniform> matrix: mat4x4<f32>;
    
    @compute
    @workgroup_size(64,1,1)
    fn transform(@builtin(global_invocation_id) i: i32) {
        vectors[i] = matrix * vectors[i];
    }
    
    @compute
    @workgroup_size(64,1,1)
    fn add_one(@builtin(global_invocation_id) i: i32) {
        buf[i] += 1;
    }
    
    @compute
    @workgroup_size(64,1,1)
    fn subtract_one(@builtin(global_invocation_id) i: i32) {
        buf[i] -= 1;
    }

init buf = 0 .. 4096
dispatch add_one (64)
check buf = 1 .. 4097
dispatch subtract_one (64,1,1)
check buf = 0 .. 4096
