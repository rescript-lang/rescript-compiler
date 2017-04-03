

type error

exception Error of error

external stack : error -> string option = ""
  [@@bs.get] [@@bs.return undefined_to_opt]

(**
   {[
     exception A of int;;
     let v = A  3 ;;
     Obj.tag (Obj.field (Obj.repr v) 0);;
     - : int = 248
   ]}
*)
let internalToOCamlException (e : Obj.t) =
  if Caml_exceptions.isCamlException e  then
    (Obj.magic e  : exn)
  else Error (Obj.magic (e : Obj.t) : error) 
(*
exception EvalError of error
exception RangeError of error
exception ReferenceError of error
exception SyntaxError of error
exception TypeError of error

 The URIError object represents an error when a global URI handling function was used in a wrong way. 
exception URIError of error    
*)

