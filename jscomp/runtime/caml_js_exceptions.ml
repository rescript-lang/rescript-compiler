

type  t = 
  | Any : 'a ->  t [@@unboxed]

exception Error of  t 


(**   
   This function has to be in this module Since 
   [Error] is defined here 
*)
let internalToOCamlException (e : Obj.t) =
  if Caml_exceptions.caml_is_extension e  then
    (Obj.magic e  : exn)
  else Error (Any e)

let caml_as_js_exn exn =   
  match exn with 
  | Error t ->  
    Some t
  | _ -> None 