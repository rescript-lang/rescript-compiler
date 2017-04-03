

type error

exception Error of error


external stack : error -> string option = ""
  [@@bs.get] [@@bs.return undefined_to_opt]

(** Used by the compiler internally *)
val internalToOCamlException : Obj.t -> exn
