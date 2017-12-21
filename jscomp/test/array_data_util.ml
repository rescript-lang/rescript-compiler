
(* []*)
let range i j = 
  Array.init (j - i + 1) (fun k -> k + i )

let randomRange i j  =
    let v = Bs.Array.init (j - i + 1) (fun[@bs] k -> k + i ) in 
    Bs.Array.shuffleInPlace v ; 
    v 