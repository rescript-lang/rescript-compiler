type u = [%bs.obj: < x: int ; y: 'self -> int > as 'self]
type x = < x: int ; y: 'self -> int > Js.t as 'self

let f (u : x) : u = u
