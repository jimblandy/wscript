module """

    @group(0) @binding(0)
    var<storage> x: array<f32, 10>;

init x = 0 .. 10
init x = 10 .. 20
