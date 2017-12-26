# 4 "hashset.cppo.mli"
type key = int


# 10
type t
val create : int -> t 
val clear : t -> unit
val reset : t -> unit
val add :  t -> key -> unit
val mem:  
   t -> key -> bool
val remove:
  t -> key -> unit
val iter : t -> (key  -> unit [@bs]) ->  unit
val fold : t -> 'c -> (key -> 'c -> 'c [@bs]) ->   'c
val length  : t -> int  
val logStats : t -> unit
val toArray : t -> key array 
val ofArray : key array -> t 
val addArray : t -> key array -> unit 