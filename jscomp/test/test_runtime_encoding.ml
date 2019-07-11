let f x = [|3; x|]
let g x = [|3.; x|]

type u = {v: int; u: float}
type uu = {vv: float; uu: float}

let ff v u = {v; u}
let fff vv uu = {vv; uu}
let a (x : int array) = x.(0)
let aa (x : float array) = x.(0)
let aaa (x : u) = x.v
let aaaa (x : uu) = x.vv
let f x = x.(0) <- 1

let f x =
  for i = 0 to 10 do
    x.(i) <- i
  done
