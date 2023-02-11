@@bs.config({
  flags: ["-bs-jsx", "4"],
})

module type T = {
  type t
}

module C1 = {
  @react.component
  let make = (type a, ~foo: module(T with type t = a)) => {
    module T = unpack(foo)
    foo
  }
}

module C2 = {
  @react.component
  let make = (type a, ~foo) => {
    module T = unpack(foo: T with type t = a)
    foo
  }
}