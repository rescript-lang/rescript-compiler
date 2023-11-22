@@uncurried

module Foo: {
  let addcuu: (int, int) => int
} = {
  @@uncurried.swap
  let addcuu = (. a, b) => a + b
}
