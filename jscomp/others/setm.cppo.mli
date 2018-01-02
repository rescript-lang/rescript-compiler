#ifdef TYPE_STRING
type elt = string
#elif defined TYPE_INT
type elt = int
#else
  [%error "unknown type"]
#endif  

type t
val empty: unit -> t
val isEmpty: t -> bool
(** Test whether a set is empty or not. *)
val mem: t -> elt -> bool
val add: t -> elt -> t
val addOnly: t -> elt -> unit 
val remove : t -> elt -> t
val removeOnly : t -> elt -> unit
val addArray : t -> elt array -> t 
val addArrayOnly : t -> elt array -> unit 
val ofArray : elt array -> t 
val toArray : t -> elt array 
val singleton: elt -> t
(** [singleton x] returns the one-element set containing only [x]. *)

val checkInvariant: t ->  bool

val length : t -> int  
