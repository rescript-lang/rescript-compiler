@unboxed
type t = A | I(int) | S(string)

let i = I(42)
let s = S("abc")

let classify = x =>
  switch x {
  | I(_) => "An integer"
  | S(s) => "A string" ++ s
  | A => "A"
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
