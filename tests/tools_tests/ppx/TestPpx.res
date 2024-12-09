Console.log("ppx test")

type t = [#A | #B]

let a: t = #A
let b: t = #B

module M = {
  let v = 10
}

open M

let vv = v

module OptionalFields = {
  type opt = {x?: int, y: float}

  let r = {y: 1.0}
}

module Uncurried = {
  type f1 = int => string
  type f2 = (int, int) => string
}
