#ifdef TYPE_STRING
type key = string
#elif defined TYPE_INT
type key = int
#else
[%error "unknown type"]
#endif  
type 'a t
(** The type of maps from type [key] to type ['a]. *)

val empty: 'a t
val isEmpty: 'a t -> bool
val has:  'a t -> key -> bool    
val cmp:  'a t -> 'a t -> ('a -> 'a -> int [@bs]) -> int

val eq: 'a t -> 'a t -> ('a -> 'a -> bool [@bs]) -> bool
(** [equal m1 m2 cmp] tests whether the maps [m1] and [m2] are
   equal, that is, contain equal keys and associate them with
   equal data.  [cmp] is the equality predicate used to compare
   the data associated with the keys. *)

val forEach: 'a t -> (key -> 'a -> unit [@bs]) ->  unit
(** [forEach m f] applies [f] to all bindings in map [m].
   [f] receives the key as first argument, and the associated value
   as second argument.  The bindings are passed to [f] in increasing
   order with respect to the ordering over the type of the keys. *)

val reduce:  'a t -> 'b -> ('b -> key -> 'a -> 'b [@bs]) -> 'b
(** [reduce m a f] computes [(f kN dN ... (f k1 d1 a)...)],
   where [k1 ... kN] are the keys of all bindings in [m]
   (in increasing order), and [d1 ... dN] are the associated data. *)

val every:  'a t -> (key -> 'a -> bool [@bs]) -> bool
(** [every m p] checks if all the bindings of the map
    satisfy the predicate [p].
 *)

val some:  'a t -> (key -> 'a -> bool [@bs]) -> bool
(** [some m p] checks if at least one binding of the map
    satisfy the predicate [p].
 *)
val size: 'a t -> int
val toList: 'a t -> (key * 'a) list
(** In increasing order with respect *)
val toArray: 'a t -> (key * 'a) array
val ofArray: (key * 'a) array -> 'a t     
val keysToArray: 'a t -> key array 
val valuesToArray: 'a t -> 'a array
val minKey: _ t -> key option 
val minKeyNull: _ t -> key Js.null
val maxKey: _ t -> key option
val maxKeyNull: _ t -> key Js.null    
val minimum: 'a t -> (key * 'a) option
val minNull: 'a t -> (key * 'a) Js.null
val maximum: 'a t -> (key * 'a) option
val maxNull: 'a t -> (key * 'a) Js.null
val get: 'a t -> key -> 'a option
val getNull: 'a t -> key -> 'a Js.null
val getWithDefault:  'a t -> key -> 'a  -> 'a
val getExn: 'a t -> key -> 'a 
val checkInvariant: _ t -> bool   
(****************************************************************************)

val remove: 'a t ->  key -> 'a t
(** [remove m x] returns a map containing the same bindings as
   [m], except for [x] which is unbound in the returned map. *)
val removeArray: 'a t -> key array -> 'a t

val set: 'a t ->  key -> 'a -> 'a t
(** [add m x y] returns a map containing the same bindings as
   [m], plus a binding of [x] to [y]. If [x] was already bound
   in [m], its previous binding disappears. *)
val update: 'a t -> key -> ('a option -> 'a option [@bs]) -> 'a t 
val mergeArray: 'a t -> (key * 'a) array -> 'a t
    
val merge:
    'a t -> 'b t ->
    (key -> 'a option -> 'b option -> 'c option [@bs]) ->
    'c t
(** [merge m1 m2 f] computes a map whose keys is a subset of keys of [m1]
    and of [m2]. The presence of each such binding, and the corresponding
    value, is determined with the function [f].
 *)

val keepBy: 
    'a t -> 
    (key -> 'a -> bool [@bs]) -> 
    'a t
(** [keepBy m p] returns the map with all the bindings in [m]
    that satisfy predicate [p].
*)

val partition: 
    'a t -> 
    (key -> 'a -> bool [@bs]) -> 
    'a t * 'a t
(** [partition m p] returns a pair of maps [(m1, m2)], where
    [m1] contains all the bindings of [s] that satisfy the
    predicate [p], and [m2] is the map with all the bindings of
    [s] that do not satisfy [p].
 *)





val split: key -> 'a t -> 'a t * 'a option * 'a t
(** [split x m] returns a triple [(l, data, r)], where
      [l] is the map with all the bindings of [m] whose key
    is strictly less than [x];
      [r] is the map with all the bindings of [m] whose key
    is strictly greater than [x];
      [data] is [None] if [m] contains no binding for [x],
      or [Some v] if [m] binds [v] to [x].
 *)


val map: 'a t -> ('a -> 'b [@bs]) ->  'b t
(** [map m f] returns a map with same domain as [m], where the
   associated value [a] of all bindings of [m] has been
   replaced by the result of the application of [f] to [a].
   The bindings are passed to [f] in increasing order
   with respect to the ordering over the type of the keys. *)

val mapWithKey: 'a t -> (key -> 'a -> 'b [@bs]) -> 'b t


val checkInvariant: _ t -> bool 
