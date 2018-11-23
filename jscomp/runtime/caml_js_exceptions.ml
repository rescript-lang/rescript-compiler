

type t 

exception Error of t 


(**
   {[
     exception A of int;;
     let v = A  3 ;;
     Obj.tag (Obj.field (Obj.repr v) 0);;
     - : int = 248
   ]}
*)
let internalToOCamlException (e : Obj.t) =
  if Caml_exceptions.isCamlExceptionOrOpenVariant e  then
    (Obj.magic e  : exn)
  else Error (Obj.magic (e : Obj.t) : t) 
