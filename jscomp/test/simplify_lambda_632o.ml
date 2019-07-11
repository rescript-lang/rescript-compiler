type t = X1 | X2 | X3 | X4
type t1 = X1 | X2 | X3 | X4 | X5
type t2 = X1 | X2 | X3 | X4 | X5 of int

let f (x : t) : t1 = match x with X1 -> X1 | X2 -> X2 | X3 -> X3 | X4 -> X4
let f2 (x : t) : t2 = match x with X1 -> X1 | X2 -> X2 | X3 -> X3 | X4 -> X4
