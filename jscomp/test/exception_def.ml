
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 


exception A of int 
module U = struct 
  exception A of int 
end

module H = Test_other_exn.Make()
type H.t += Bx


let a = 3

let u : exn = Bx


type Test_other_exn.V.t += Ax
type exn += XXX

exception Aa = Match_failure

let v = Aa ("",0,0)

exception H0 = Not_found 
exception H1
exception H2 
exception H3 = H2 

let h2 = H2 
let h3 = H3 
let h4 = H0 
exception H4 = Invalid_argument 

let h5 = H4 "xx"
;; Printexc.register_printer (function 
    | A s -> Some "A"
    | _ -> None
)

let p e = 
  match e with 
  | H4 x -> 0
  | H3 -> 1 
  | H2 -> 2 
  | H0 -> 4 
  | Not_found -> 3
  | _ -> -1


;; eq __LOC__ (p h5) 0
;; eq __LOC__ (p Not_found) 4  
;; eq __LOC__ (p H0) 4  
;; eq __LOC__ (p H3) 1 
;; eq __LOC__ (p H2 ) 1 (* aliased to H3 *)
;; eq __LOC__ (p (Invalid_argument "")) 0
;; Mt.from_pair_suites __FILE__ !suites  