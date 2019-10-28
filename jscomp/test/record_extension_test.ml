
#if OCAML_VERSION =~ ">4.3.0" then

(* Record_extension *)
type t0  = ..
type t0 += Inline_record of { x : int ; y : string} 

let f x = 
  match x with 
  | Inline_record {x; y} -> Some (x + int_of_string y)  
  | _ -> None 
let v0 = Inline_record {x = 3; y = "4"}
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
#end
