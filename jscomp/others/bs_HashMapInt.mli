# 4 "hashmap.cppo.mli"
type key = int


# 10
type 'b t 


val create:  int -> 'b t 

val clear: 'b t -> unit

val set: 'a t -> key -> 'a -> unit

val get:  'a t -> key -> 'a option

val has:  'b  t -> key -> bool

val remove: 'a t -> key -> unit

val forEach: 'b t -> (key -> 'b -> unit [@bs]) -> unit

val reduce: 'b t -> 'c -> ('c -> key -> 'b ->  'c [@bs]) -> 'c

val filterMapDone: 'a t ->  (key -> 'a -> 'a option [@bs]) -> unit
val filterMap: 'a t ->  (key -> 'a -> 'a option [@bs]) -> 'a t
  
val size: _ t -> int  
val logStats: _ t -> unit

val toArray: 'a t -> (key * 'a) array 
val ofArray: (key * 'a) array -> 'a t
val mergeArrayDone: 'a t -> (key * 'a) array -> unit
val mergeArray: 'a t -> (key * 'a) array -> 'a t



