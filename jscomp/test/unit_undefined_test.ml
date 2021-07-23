
[@@@bs.config{flags = 
[|
  (* "-bs-diagnose" *)
  (* ; "-drawlambda" *)
  (* ; "-dtypedtree" *)
|]}]

let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 


external
 hi: (unit -> unit [@bs.uncurry 0]) -> int = "hi" [@@bs.val]

let f_01 () = hi (fun (() as x) -> if x = () then Js.log "x" ) (* FIXME: not inlined *)



let u x = 
  match () with 
  | () when x > 3 -> 1 
  | () when x < 2 -> 2
  | () when x > 4 -> 0
  | () -> 3 

let fx () = ()  

let u0 (x : unit) = Some x 

let u1 = Some ()
type t = unit

let u2 (x : t) = Some x 
let u3 : t option = Some ()
let u4 : t = ()

;; eq __LOC__ (u0 ()) (Obj.magic (Some None))
;; eq __LOC__ (u1) (Obj.magic (Some None))
;; eq __LOC__ (u2 ()) (Obj.magic (Some None))
;; eq __LOC__ (u3) (Obj.magic (Some None))
;; eq __LOC__ u4 (Obj.magic None)

;; Mt.from_pair_suites __FILE__ !suites