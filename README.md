# A Scripting Language for `wgpu`

This is a language for writing quick tests and experiments for the
[`wgpu`](https://github.com/gfx-rs/wgpu) GPU crate.

For example:

```text
buffer @group(0) @binding(0): array<u32> = 0 .. 4096
dispatch """

    @group(0) @binding(0)
    var<storage> buf: array<u32, 4096>;

    @compute
    @workgroup_size(64)
    fn add_one(@builtin(global_invocation_id) i: i32) {
        buf[i] += 1;
    }

check: 1 .. 4097
```

