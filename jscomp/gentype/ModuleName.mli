type t

val compare : t -> t -> int
val curry : t
val forBsFile : t -> t
val forInnerModule : fileName:t -> innerModuleName:string -> t

val fromStringUnsafe : string -> t
(** Used to turn strings read from external files into module names. *)

val rescriptPervasives : t
val toString : t -> string
val uncapitalize : t -> t
