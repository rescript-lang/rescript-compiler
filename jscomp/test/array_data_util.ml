
module A = Bs_Array 

(* []*)
let range i j = 
  A.init (j - i + 1) (fun[@bs] k -> k + i )

let randomRange i j  =
    A.shuffle (A.init (j - i + 1) (fun[@bs] k -> k + i ))
    
    