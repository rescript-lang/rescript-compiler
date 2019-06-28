
type t0 = 
  | A0
  | B0 of int 
  
type t1 = 
  | A1 
  | B1 of int 
  | C1 of int

type u = { x : t0; y : t1}
module type S = sig 
  val u : u
  val v : t1
end 
module N = struct  
  let u = {x = A0; y = A1 }
  let v = C1 2 
end 
let () = 
  Js.log 
    (A0, 
    B0 2, 
    B1 3,
    C1 2,
    `hello 3,
    { x = B0 2 ; y = C1 2 },
    (module N: S)
    )


;; [%debugger]