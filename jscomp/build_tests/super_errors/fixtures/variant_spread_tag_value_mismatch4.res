@@uncurried

module Foo: {
  @@uncurried.swap
  let adducu: (. int, int) => int
} = {
  let adducu = (a, b) => a + b
}
