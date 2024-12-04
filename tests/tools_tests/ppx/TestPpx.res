Console.log("ppx test")

type t = [#A | #B]

let a: t = #A
let b: t = #B

module M = {
  let v = 10
}

open M

let vv = v
