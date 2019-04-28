let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 

let v = 
  2 |. (3 |. (+))

module X = struct  
  type t = Some of int 
end 

let u =  3 |. X.Some 

let xx 
  obj 
  a0 a1 a2 a3 a4 a5
  = 
  obj 
  |. a0 a1
  
  |. a2 a3 
  |. a4 a5
  |. (-) 1 
  |. (3 |. (-))
(*
  (a4 (a2 (a0 obj a1) a3) a5)
*)

let () = eq __LOC__ v 5 

let () = 
   eq __LOC__ 
   (xx 3 (-)  2 (+) 4 ( * ) 3)  11

(*

*)   
;; Mt.from_pair_suites __MODULE__ !suites