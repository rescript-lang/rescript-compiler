
module A = Bs_Array 

(* []*)
let range i j = 
  A.initExn (j - i + 1) (fun[@bs] k -> k + i )

let randomRange i j  =
    A.shuffle (A.initExn (j - i + 1) (fun[@bs] k -> k + i ))
    
    