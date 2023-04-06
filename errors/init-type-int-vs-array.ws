module """
  struct S { n: i32 }
  @group(0) @binding(0)
  var<uniform> s: S;

init s = 1..10
