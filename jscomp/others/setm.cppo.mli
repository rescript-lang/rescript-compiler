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

val length: t -> int
val toList : t -> elt list 
val toArray: t -> elt array  

val minOpt: t -> elt option
val minNull: t -> elt Js.null
val maxOpt: t -> elt option
val maxNull: t -> elt Js.null
    
val singleton: elt -> t
(** [singleton x] returns the one-element set containing only [x]. *)

val checkInvariant: t ->  bool

val length : t -> int  
