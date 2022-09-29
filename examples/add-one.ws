module """

    @group(0) @binding(0)
    var<storage> buf: array<u32, 4096>;
    
    @compute
    @workgroup_size(64)
    fn add_one(@builtin(global_invocation_id) i: i32) {
        buf[i] += 1;
    }

init buf = 0 .. 4096
dispatch add_one 64
check buf = 1 .. 4097
