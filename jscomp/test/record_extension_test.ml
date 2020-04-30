
(* for o in jscomp/test/*test.js ; do npx mocha  $o ; done *)

let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 


(* Record_extension *)
type t0  = ..
type t0 += Inline_record of { x : int ; y : string} 

let f x = 
  match x with 
  | Inline_record {x; y} -> Some (x + int_of_string y)  
  | _ -> None 
let v0 = Inline_record {x = 3; y = "4"}

;; eq __LOC__ (f v0) (Some 7)

(* Record_unboxed *)
type t1 = | A of { x : int } [@ocaml.unboxed]


(* Record_inlined *)
type t2 = 
  | B 
  | C of  {x : int ;y : string }
  | D of { w : int}
let f2 x = 
  match x with 
  | D _
  | B -> 0

  | C {x } -> x 

let f2_with x = 
  match x with 
  | D _
  | B -> x 
  | C u -> C {u with x = 0}


exception A of {name : int; x : int}  
exception B of int * int
exception C of {name : int }

let u f = 
  try f () with
  | A {name ; x} -> name + x 
  | B (a,b) -> a + b
  | C x -> x.name
  | _ -> -1

let () = 
  Mt.from_pair_suites __LOC__ !suites

