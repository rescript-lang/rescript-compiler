



(*
let f = [%bs.error? (Not_found | Invalid_argument _)]


let u = fun [@bs.exn] e ->
  match e with 
  |
*)

let f = function [@bs.open]
  | Not_found -> 0 
  | ( Invalid_argument _
    | Stack_overflow ) -> 1 
  | Sys_error _ -> 2 

type exn += A of int 
type exn += B of int 

let g = function [@bs.open]
  | (Not_found 
    | Invalid_argument _  )-> 0
  | Sys_error _ -> 2
  | (A i | B i) -> i 


(* type v = A  *)
(* let f = function [@bs.open] *)
(*   | A -> 1  *)


(* let f_m = fun match_ -> *)
(*   if Caml_exceptions.isCamlExceptionOrOpenVariant match_ then *)
(*     match Obj.magic match_ with  *)
(*     | Not_found -> Some 0  *)
(*     | ( Invalid_argument _ *)
(*       | Stack_overflow ) -> Some 1  *)
(*     | Sys_error _ -> Some 2  *)
(*     | _ -> None *)
(*   else None  *)

