
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq loc x y = Mt.eq_suites ~test_id ~suites loc x y 

type t = A of int [@@ocaml.unboxed]

let v0 = A 3

let make x = A x 

let get (A x) = x

(* For concrete types *)
type t1 = A of string [@@ocaml.unboxed];;

let x = A "foo" in
eq __LOC__ (Obj.repr x)  (Obj.repr (match x with A s -> s))
;;


(* For records *)
type t2 = { f : string } [@@ocaml.unboxed];;

let x = { f = "foo" } in
eq __LOC__ (Obj.repr x)  (Obj.repr x.f)
;;

(* For inline records *)
type t3 = B of { g : string } [@@ocaml.unboxed];;

let x = B { g = "foo" } in
eq __LOC__ (Obj.repr x)  (Obj.repr (match x with B {g} -> g))
;;

type r = A of r ;;
let rec y = A y;;

let () = 
  Mt.from_pair_suites __FILE__ !suites  
