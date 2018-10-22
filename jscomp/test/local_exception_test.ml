exception A of int * bool 

let v = A (3, true)

exception B 

let u = B 

exception D of int 

let d = D  3
#if OCAML_VERSION =~ "<4.03.0" then (* Not allowed *)
exception A of int 
(* intentionally overridden , 
   so that we can not tell the differrence, only by [id]*)

let x = A 3 
#end