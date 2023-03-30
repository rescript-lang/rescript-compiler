@unboxed
type t = A | I(int) | S(string)
@unboxed
type t2 = S2(string) | I2(float)

let i = I(42)
let i2 = I2(42.5)
let s = S("abc")
let s2 = S2("abc")

let classify = x =>
  switch x {
  | I(_) => "An integer"
  | S(s) => "A string" ++ s
  | A => "A"
  }

let classify2 = x =>
  switch x {
  | I2(_) => "A float"
  | S2(s) => "A string" ++ s
  }

@unboxed
type tt = One | Two | Object({x: int, y: string})

let w = Object({x: 10, y: ""})

let cls = x =>
  switch x {
  | One => "one"
  | Two => "two"
  | Object({y}) => "object" ++ y
  }

module ListWithTuples = {
  @unboxed
  type rec t<'a> = | @as(undefined) Empty | Cons(('a, t<'a>))
}

module ListWithObjects = {
  @unboxed
  type rec t<'a> = | @as(null) Empty | Cons({hd: 'a, tl: t<'a>})
}

let rec tuplesToObjects = (l: ListWithTuples.t<_>): ListWithObjects.t<_> =>
  switch l {
  | Empty => Empty
  | Cons((hd, tl)) => Cons({hd, tl: tuplesToObjects(tl)})
  }

let l1 = ListWithTuples.Cons((1, Cons((2, Cons((3, Empty))))))
let l2 = tuplesToObjects(l1)
Js.log2("l1", l1)
Js.log2("l2", l2)

module Truthy = {
  @unboxed
  type t = | @as(true) True | Obj({flag: bool})

  let isTrue = x =>
    switch x {
    | True => true
    | Obj({flag}) => flag
    }
}

module TwoObjects = {
  @unwrapped
  type t = | @as(null) Null | Object({name: string}) | @as(undefined) Undefined

  let classify = x =>
    switch x {
    | Null => "null"
    | Object({name}) => "object" ++ name
    | Undefined => "undefined"
    }
}
