@unboxed
type t = I(int) | S(string)

let i = I(42)
let s = S("abc")

let classify = x =>
  switch x {
  | I(_) => "An integer"
  | S(_) => "A string"
  }
