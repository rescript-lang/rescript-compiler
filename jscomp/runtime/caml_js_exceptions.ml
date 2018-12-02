

type t 

exception Error of t 


(**
   {[
     exception A of int;;
     let v = A  3 ;;
     Caml_obj_extern.tag (Caml_obj_extern.field (Caml_obj_extern.repr v) 0);;
     - : int = 248
   ]}
   This function has to be in this module Since 
   [Error] is defined here 
*)
let internalToOCamlException (e : Caml_obj_extern.t) =
  if Caml_exceptions.caml_is_extension e  then
    (Obj.magic e  : exn)
  else Error (Obj.magic (e : Caml_obj_extern.t) : t) 

let caml_as_js_exn exn =   
  match exn with 
  | Error t ->  
    Some t 
  | _ -> None 