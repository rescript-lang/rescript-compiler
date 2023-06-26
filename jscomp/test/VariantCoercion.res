type a = One | @as("two") Two | Three

let a: a = Three

let b = (a :> string)

type onlyInts = | @as(1) One1 | @as(2) Two2 | @as(3) Three3

let i = One1

let d = (i :> int)

type onlyFloats = | @as(1.1) Onef | @as(2.2) Twof | @as(3.3) Threef

let i = Onef

let d = (i :> float)
