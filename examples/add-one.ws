module """

    @group(0) @binding(0)
    var<storage> buf: array<u32, 4096>;
    
    @compute
    @workgroup_size(64)
    fn add_one(@builtin(global_invocation_id) i: i32) {
        buf[i] += 1;
    }

buffer buf = 0 .. 4096
check: 1 .. 4097
