# 4 "setm.cppo.mli"
type elt = int

# 9
type t
val empty: unit -> t
val isEmpty: t -> bool
val mem: t -> elt -> bool
val add: t -> elt -> t
val addOnly: t -> elt -> unit
val singleton: elt -> t
val remove: t -> elt -> t
val removeOnly: t -> elt -> unit
val union: t -> t -> t
val inter: t -> t -> t
val diff: t -> t -> t
val cmp: t -> t -> int
val eq: t -> t -> bool

val subset: t -> t -> bool

val iter: t -> (elt -> unit [@bs]) ->  unit
(** In increasing order*)

val fold: t -> 'a -> ('a -> elt ->  'a [@bs]) ->  'a
(** Iterate in increasing order. *)

val forAll: t -> (elt -> bool [@bs]) ->  bool
(** [for_all p s] checks if all elements of the set
    satisfy the predicate [p]. Order unspecified. *)

val exists: t -> (elt -> bool [@bs]) ->  bool
(** [exists p s] checks if at least one element of
    the set satisfies the predicate [p]. Oder unspecified. *)

val filter: t -> (elt -> bool [@bs]) ->  t
(** [filter p s] returns the set of all elements in [s]
    that satisfy predicate [p]. *)

val partition: t -> (elt -> bool [@bs]) ->  t * t
(** [partition p s] returns a pair of sets [(s1, s2)], where
    [s1] is the set of all the elements of [s] that satisfy the
    predicate [p], and [s2] is the set of all the elements of
    [s] that do not satisfy [p]. *)

val length: t -> int
val toList : t -> elt list
 (** In increasing order with respect *)
val toArray: t -> elt array
val ofArray: elt array -> t     
val minOpt: t -> elt option
val minNull: t -> elt Js.null
val maxOpt: t -> elt option
val maxNull: t -> elt Js.null
    
val split:  t -> elt  -> t * bool * t
val findOpt:  t -> elt -> elt option
    
val addArray: t -> elt array -> t 
val addArrayOnly: t -> elt array -> unit 

val checkInvariant: t ->  bool


