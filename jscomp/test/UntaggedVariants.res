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
  @unboxed
  type t = | @as(null) Null | Object({name: string}) | @as(undefined) Undefined

  let classify = x =>
    switch x {
    | Null => "null"
    | Object({name}) => "object" ++ name
    | Undefined => "undefined"
    }
}

module Unknown = {
  @unboxed
  type t<'a> = A | B | Unknown('a)

  let classify = x =>
    switch x {
    | A => "a"
    | B => "b"
    | Unknown(v) => {
        Js.log(x)
        "Unknown"
      }
    }
}

module MultipleBlocks = {
  @unboxed
  type t<'a> = A | B | C | D | String(string) | Int(int) | Object({name: string})

  let classify = x =>
    switch x {
    | A => "a"
    | B => "b"
    | C => "c"
    | D => "d"
    | String(_) => "string"
    | Int(_) => "int"
    | Object({name}) => "Object" ++ name
    }
}

module OnlyBlocks = {
  @unboxed
  type t<'a> = String(string) | Int(int) | Object({name: string})

  let classify = x =>
    switch x {
    | String(_) => "string"
    | Int(_) => "int"
    | Object({name}) => "Object" ++ name
    }
}

module WithArray = {
  @unboxed
  type t<'a> = String(string) | Float(float) | Array(array<string>) | Object({name: string})

  let classify = x =>
    switch x {
    | String(_) => "string"
    | Float(_) => "int"
    | Array(_) if true => "array"
    | Array(_) => "array"
    | Object({name}) => "Object" ++ name
    }
}

module Json = {
  @unboxed
  type rec t =
    | @as(false) False
    | @as(true) True
    | @as(null) Null
    | String(string)
    | Number(float)
    | Object(Js.Dict.t<t>)
    | Array(array<t>)

  type tagged_t =
    | JSONFalse
    | JSONTrue
    | JSONNull
    | JSONString(string)
    | JSONNumber(float)
    | JSONObject(Js.Dict.t<t>)
    | JSONArray(array<t>)

  let classify = (x: t) =>
    switch x {
    | False => JSONFalse
    | True => JSONTrue
    | Null => JSONNull
    | String(s) => JSONString(s)
    | Number(n) => JSONNumber(n)
    | Object(o) => JSONObject(o)
    | Array(a) => JSONArray(a)
    }

  /* from js_json.ml
let classify  (x : t) : tagged_t =
  let ty = Js.typeof x in
  if ty = "string" then
    JSONString (Obj.magic x)
  else if ty = "number" then
    JSONNumber (Obj.magic x )
  else if ty = "boolean" then
    if (Obj.magic x) = true then JSONTrue
    else JSONFalse
  else if (Obj.magic x) == Js.null then
    JSONNull
  else if Js_array2.isArray x  then
    JSONArray (Obj.magic x)
  else
    JSONObject (Obj.magic x)  
 */
}

module TrickyNested = {
  @unboxed
  type rec t =
    | A((t, t))
    | B

  let check = (s, y) =>
    switch s {
    | A((A(x), B)) if x !== y => 41
    | _ => 42
    }
}

module OverlapString = {
  @unboxed
  type enum = One | Two | Three | FutureAddedValue(string)

  let checkEnum = e =>
    switch e {
    | One => "One!"
    | Two => "Two"
    | Three => "Threeeee"
    | FutureAddedValue(s) => "Something else..." ++ s
    }
}

module OverlapNumber = {
  @unboxed
  type enum = | @as(1.0) One | Two | Three | FutureAddedValue(float)

  let checkEnum = e =>
    switch e {
    | One => "One!"
    | Two => "Two"
    | Three => "Threeeee"
    | FutureAddedValue(_) => "Something else..."
    }
}

module OverlapObject = {
  @unboxed
  type enum = | @as(null) One | Two | Three | Object({x: int})

  let checkEnum = e =>
    switch e {
    | One => "One!"
    | Two => "Two"
    | Three => "Threeeee"
    | Object(_) => "Object..."
    }
}
