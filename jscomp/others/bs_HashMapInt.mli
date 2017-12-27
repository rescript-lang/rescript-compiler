# 4 "hashmap.cppo.mli"
type key = int


# 10
type 'b t 


val create :  int -> 'b t 

val clear : 'b t -> unit
(** Empty a hash table. Use [reset] instead of [clear] to shrink the
    size of the bucket table to its initial size. *)


val reset : 'b t -> unit
(** Empty a hash table and shrink the size of the bucket table
    to its initial size.
    @since 4.00.0 *)

val add : 'a t -> key -> 'a -> unit

val findOpt:  'a t -> key -> 'a option


val mem:  'b  t -> key -> bool

val remove: 'a t -> key -> unit

val iter : 'b t -> (key -> 'b -> unit [@bs]) -> unit

val fold : 'b t -> 'c -> (key -> 'b -> 'c -> 'c [@bs]) -> 'c


val filterMapInplace : 'a t ->  (key -> 'a -> 'a option [@bs]) -> unit
  
val length  : _ t -> int  
val logStats : _ t -> unit

val toArray : 'a t -> (key * 'a) array 
val ofArray : (key * 'a) array -> 'a t  
val addArray : 'a t -> (key * 'a) array -> unit 



