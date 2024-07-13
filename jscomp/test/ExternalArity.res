@@uncurried

type u = (int, int) => int
@val external v1: u = "v" // arity from u is implicit
@val external v2: (int, int) => int = "v" // arity is explicit

let f1 = x => v1(x,x)
let f2 = x => v2(x,x)
