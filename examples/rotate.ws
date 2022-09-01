buffer @group(0) @binding(0): array<mat3x2<f32>> = {
   { ^x, ^y,  0 },
   { ^x,  0, ^y },
   {  0, ^x, ^y },
}

dispatch """

    @group(0) @binding(0)
    var<storage> matrices: array<mat3x2<f32>>;

    @compute
    @workgroup_size(3)
    fn rotate(@builtin(global_invocation_id) i) {
        let m = matrices[i];

        matrices[i] = mat3x2(m[1], m[2], m[0] + i);
    }

check: {
   { ^y,  0, ^x + 0 },
   {  0, ^y, ^x + 1 },
   { ^x, ^y,  0 + 2 },
}
