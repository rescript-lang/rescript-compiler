# 4 "mapm.cppo.mli"
type key = int
# 8
type 'a t


val empty: unit -> 'a t
val isEmpty: 'a t -> bool
val singleton: key -> 'a -> 'a t
val mem:  'a t -> key -> bool
val cmp:  'a t -> 'a t -> ('a -> 'a -> int [@bs]) -> int
(** [cmp m1 m2 cmp]
    First compare by size, if size is the same,
    compare by key, value pair
*)
val eq: 'a t -> 'a t -> ('a -> 'a -> bool [@bs]) -> bool
(** [eq m1 m2 cmp] *)
  
val iter: 'a t -> (key -> 'a -> unit [@bs]) ->  unit
(** [iter m f] applies [f] to all bindings in map [m].
   [f] receives the key as first argument, and the associated value
   as second argument.
   The application order of [f]  is in increasing order. *)

val fold:  'a t -> 'b -> ('b -> key -> 'a -> 'b [@bs]) -> 'b
(** [fold m a f] computes [(f kN dN ... (f k1 d1 a)...)],
   where [k1 ... kN] are the keys of all bindings in [m]
   (in increasing order), and [d1 ... dN] are the associated data. *)

val forAll:  'a t -> (key -> 'a -> bool [@bs]) -> bool
(** [forAll m p] checks if all the bindings of the map
    satisfy the predicate [p].
    The application order of [p] is unspecified. 
 *)

val exists:  'a t -> (key -> 'a -> bool [@bs]) -> bool
(** [exists m p] checks if at least one binding of the map
    satisfy the predicate [p].
    The application order of [p] is unspecified. 
 *)




val length: 'a t -> int
val toList: 'a t -> (key * 'a) list
(** In increasing order *)
val toArray: 'a t -> (key * 'a) array   
val ofArray: (key * 'a) array -> 'a t 
val keysToArray: 'a t -> key array 
val valuesToArray: 'a t -> 'a array
val minKeyOpt: _ t -> key option 
val minKeyNull: _ t -> key Js.null
val maxKeyOpt: _ t -> key option 
val maxKeyNull: _ t -> key Js.null    
val minKeyValueOpt: 'a t -> (key * 'a) option
val minKeyValueNull: 'a t -> (key * 'a) Js.null
val maxKeyValueOpt: 'a t -> (key * 'a) option
val maxKeyValueNull: 'a t -> (key * 'a) Js.null
val findOpt: 'a t ->  key -> 'a option
val findNull: 'a t -> key -> 'a Js.null
val findWithDefault:  'a t -> key -> 'a  -> 'a
val findExn: 'a t -> key -> 'a
  
(****************************************************************************)

(*TODO: add functional [merge, partition, filter, split]*)
  
val addOnly : 'a t -> key -> 'a -> unit  
val add: 'a t ->  key -> 'a -> 'a t
(** [add m x y] do the in-place modification, return
    [m] for chaining. If [x] was already bound
   in [m], its previous binding disappears. *)


val remove: 'a t ->  key -> 'a t
(** [remove m x] do the in-place modification, return [m] for chaining *)


val map: 'a t -> ('a -> 'b [@bs]) ->  'b t
(** [map m f] returns a map with same domain as [m], where the
   associated value [a] of all bindings of [m] has been
   replaced by the result of the application of [f] to [a].
   The bindings are passed to [f] in increasing order
   with respect to the ordering over the type of the keys. *)

val mapi: 'a t -> (key -> 'a -> 'b [@bs]) -> 'b t


val checkInvariant : _ t -> bool 
