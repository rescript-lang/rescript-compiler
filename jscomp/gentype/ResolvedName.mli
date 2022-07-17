type t
type eq = t * t

val applyEquations : eqs:eq list -> t -> eq list
val dot : string -> t -> t
val fromString : string -> t
val toList : t -> string list
val toString : t -> string
