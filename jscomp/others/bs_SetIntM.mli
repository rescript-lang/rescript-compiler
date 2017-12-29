type t
type elt = int 

val empty: t


val isEmpty: t -> bool
(** Test whether a set is empty or not. *)

val mem: t -> elt -> bool


val add: t -> elt -> t

val singleton: elt -> t
(** [singleton x] returns the one-element set containing only [x]. *)

val checkInvariant: t ->  bool

val length : t -> int  