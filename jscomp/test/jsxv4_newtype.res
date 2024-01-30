@@bs.config({
   flags: ["-bs-jsx", "4"],
 })

module V4A = {
  @react.component
  let make = (type a, ~a: a, ~b: array<option<[#Foo(a)]>>, ~c: 'a, _) => React.null
}

let _ = <V4A a="a" b=[] c=1 />

module V4A1 = {
  @react.component
  let make = (type x y, ~a:x, ~b:array<y>, ~c:'a) => React.null
}

let _ = <V4A1 a="a" b=[0] c=1 />

module type T = {
  type t
}

module V4A2 = {
  @react.component
  let make = (type a, ~foo: module(T with type t = a)) => {
    module T = unpack(foo)
    React.null
  }
}

module M = {
  type t = int
}

let _ = <V4A2 foo=module(M) />

module V4A3 = {
  @react.component
  let make = (type a, ~foo) => {
    module T = unpack(foo: T with type t = a)
    let _ = foo
    React.null
  }
}

let _ = <V4A3 foo=module(M) />