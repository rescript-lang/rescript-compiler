# 4 "hashset.cppo.mli"
type key = int


# 10
type t
val make: int -> t 

val clear: t -> unit

val isEmpty: t -> bool
  
val add:  t -> key -> unit

val copy: t -> t
  
val has: t -> key -> bool
  
val remove: t -> key -> unit

val forEachU: t -> (key  -> unit [@bs]) ->  unit
val forEach: t -> (key  -> unit) ->  unit
  
val reduceU: t -> 'c -> ( 'c -> key -> 'c [@bs]) ->   'c
val reduce: t -> 'c -> ( 'c -> key -> 'c) ->   'c
  
val size: t -> int  

val logStats: t -> unit

val toArray: t -> key array 

val ofArray: key array -> t 

val mergeMany: t -> key array -> unit

val getBucketHistogram: t -> int array
