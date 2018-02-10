
module A = Belt.Array

(* []*)
let range i j = 
  A.makeBy (j - i + 1) (fun k -> k + i )

let randomRange i j  =
    let v= (A.makeBy (j - i + 1) (fun k -> k + i )) in 
    A.shuffleInPlace v ; 
    v 
    
    
