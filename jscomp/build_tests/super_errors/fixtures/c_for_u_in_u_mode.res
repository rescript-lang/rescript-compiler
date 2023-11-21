@@uncurried

module Foo: {
  let add: (int, int) => int
} = {
  @@uncurried.swap
  let add = (. a, b) => a + b
}
