@@uncurried

module Foo: {
  @@uncurried.swap
  let add: (. int, int) => int
} = {
  let add = (a, b) => a + b
}
