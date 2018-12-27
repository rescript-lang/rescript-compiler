let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 


type t = 
  [ `a [@bs.as "x"] 
  | `u [@bs.as "hi"]
  | `b [@bs.as {j|你|j} ]
  | `c [@bs.as {js|我|js}]
  ]
  [@@bs.deriving jsConverter]

let v,u = tToJs, tFromJs  


(* not applicable to thiis type, and unused warning*)
#if 0 then
type t0 =  
  [ `a of int [@bs.as "hi"] ]
  [@@bs.deriving jsConverter]
#end  


;; eq __LOC__ (v `a) "x"
;; eq __LOC__ (v `u) "hi"
;; eq __LOC__ (v `b) {j|你|j}
;; eq __LOC__ (v `c) {js|我|js}

;; eq __LOC__ (u "x")  (Some `a)
;; eq __LOC__ (u "hi") (Some `u)
;; eq __LOC__ (u {j|你|j}) (Some `b)
;; eq __LOC__ (u {js|我|js}) (Some `c)
;; eq __LOC__ (u "xx") None

let () = Mt.from_pair_suites __MODULE__ !suites