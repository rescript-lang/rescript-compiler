





type x = (
  (< x : int ; y : 'self -> int > Js.t)
  as 'self
)

let f (u : x) : x = u
