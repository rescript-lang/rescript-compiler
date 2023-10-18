type a = One | @as("two") Two | Three

let a: a = Three

let b = (a :> string)

type onlyInts = | @as(1) One1 | @as(2) Two2 | @as(3) Three3

let i = One1

let d = (i :> int)

type onlyFloats = | @as(1.1) Onef | @as(2.2) Twof | @as(3.3) Threef

let ii = Onef

let dd = (ii :> float)

module CoerceVariants = {
  @unboxed type a = One(int) | @as(1.1) Two | @as(null) T2
  @unboxed type b = One(int) | @as(1.1) Two | @as(null) T2 | Three

  let a: a = Two

  let b: b = (a :> b)

  @tag("kind") type x = One({age: int, name?: string})
  @tag("kind") type y = One({age: int, name?: string}) | Two({two: string})

  let x: x = One({age: 1})
  let y: y = (x :> y)
}

module CoerceWithPayload = {
  @unboxed type strings = String(string) | First | Second | Third
  let a: strings = String("hello")
  let aa: strings = First
  let b: string = (a :> string)
  let bb: string = (aa :> string)

  @unboxed type floats = Number(float) | @as(1.) First | @as(2.) Second | @as(3.) Third
  let c: floats = Number(100.)
  let cc: floats = Second
  let d: float = (c :> float)
  let dd: float = (cc :> float)
}
