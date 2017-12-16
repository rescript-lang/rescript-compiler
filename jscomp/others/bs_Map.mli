(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(** Adapted by authors of BuckleScript without using functors          *)
(** The type of the map keys. *)
type ('k, + 'a, 'id) t0 

type ('k, +'a, 'id) t = {
  dict : ('k,'id) Bs_Cmp.t ;
  data : ('k, 'a, 'id) t0 
}
(** [('k, 'a, id) t] 
    ['k] the key type 
    ['a] the value type
    ['id] is a unique type for each keyed module
*)
(*
    How we remain soundness:
    The only way to create a value of type [_ t] from scratch 
    is through [empty] which requires [_ Bs_Cmp.t]
    The only way to create [_ Bs_Cmp.t] is using [Bs_Cmp.Make] which
    will create a fresh type [id] per module

    Generic operations over tree without [cmp] is still exported 
    (for efficient reaosns) so that [data] does not need be boxed and unboxed.

    The soundness is guarantted in two aspects:
    When create a value of [_ t] it needs both [_ Bs_Cmp.t] and [_ t0].
    [_ Bs_Cmp.t] is an abstract type. Note [add0] requires [_ Bs_Cmp.cmp] which 
    is also an abtract type which can only come from [_ Bs_Cmp.t]

    When destructing a value of [_ t], the ['id] parameter is threaded.

*)

(* should not export [Bs_Cmp.compare]. 
   should only export [Bs_Cmp.t] or [Bs_Cmp.cmp] instead *)



val empty0 : ('k, 'a, 'id) t0
val empty: ('k, 'id) Bs_Cmp.t -> ('k, 'a, 'id) t 
(** The empty map. *)

val ofArray0:  
    cmp: ('k,'id) Bs_Cmp.cmp -> 
    ('k * 'a) array ->  
    ('k,'a,'id) t0 
val ofArray:      
    ('k,'id) Bs_Cmp.t -> 
    ('k * 'a) array ->  
    ('k,'a,'id) t 
    
val isEmpty0 : ('k, 'a,'id) t0 -> bool 
val isEmpty: ('k, 'a, 'id) t -> bool
(** Test whether a map is empty or not. *)

val mem0: cmp: ('k,'id) Bs_Cmp.cmp -> 
  'k -> ('k, 'a, 'id) t0 -> bool
val mem: 
  'k -> ('k, 'a, 'id) t -> bool
(** [mem x m] returns [true] if [m] contains a binding for [x],
    and [false] otherwise. *)

val add0: cmp: ('k,'id) Bs_Cmp.cmp -> 
  'k -> 'a -> ('k, 'a, 'id) t0 -> ('k, 'a, 'id) t0 
val add: 'k -> 'a -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t
(** [add x y m] returns a map containing the same bindings as
    [m], plus a binding of [x] to [y]. If [x] was already bound
    in [m], its previous binding disappears. *)

val singleton0 : 'k -> 'a -> ('k, 'a, 'id) t0    
val singleton: ('k,'id) Bs_Cmp.t ->
  'k -> 'a -> ('k, 'a, 'id) t
(** [singleton x y] returns the one-element map that contains a binding [y]
    for [x].
    @since 3.12.0
*)

val remove0: cmp: ('k,'id) Bs_Cmp.cmp -> 
    'k -> ('k, 'a, 'id) t0 -> ('k, 'a, 'id) t0
val remove: 'k -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t
(** [remove x m] returns a map containing the same bindings as
    [m], except for [x] which is unbound in the returned map. *)

val merge0: cmp: ('k,'id) Bs_Cmp.cmp ->     
    ('k -> 'a option -> 'b option -> 'c option [@bs]) -> ('k, 'a, 'id ) t0 -> ('k, 'b,'id) t0 -> ('k, 'c,'id) t0    
val merge:
  ('k -> 'a option -> 'b option -> 'c option [@bs]) -> ('k, 'a, 'id ) t -> ('k, 'b,'id) t -> ('k, 'c,'id) t
(** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
    and of [m2]. The presence of each such binding, and the corresponding
    value, is determined with the function [f].
    @since 3.12.0
*)

val compare0: cmp:('k,'id) Bs_Cmp.cmp -> 
     ('a -> 'a -> int [@bs]) -> ('k, 'a, 'id) t0 -> ('k, 'a, 'id) t0 -> int
val compare: ('a -> 'a -> int [@bs]) -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t -> int
(** Total ordering between maps.  The first argument is a total ordering
    used to compare data associated with equal keys in the two maps. *)

val equal0: cmp: ('k,'id) Bs_Cmp.cmp ->     
    ('a -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t0 -> ('k, 'a, 'id) t0 -> bool
val equal: ('a -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t -> bool
(** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
    equal, that is, contain equal keys and associate them with
    equal data.  [cmp] is the equality predicate used to compare
    the data associated with the keys. *)

val iter0: ('k -> 'a -> unit [@bs]) -> ('k, 'a, 'id) t0 -> unit   
val iter: ('k -> 'a -> unit [@bs]) -> ('k, 'a, 'id) t -> unit
(** [iter f m] applies [f] to all bindings in map [m].
    [f] receives the 'k as first argument, and the associated value
    as second argument.  The bindings are passed to [f] in increasing
    order with respect to the ordering over the type of the keys. *)

val fold0: ('k -> 'a -> 'b -> 'b [@bs]) -> ('k, 'a, 'id) t0 -> 'b -> 'b
val fold: ('k -> 'a -> 'b -> 'b [@bs]) -> ('k, 'a, 'id) t -> 'b -> 'b
(** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
    where [k1 ... kN] are the keys of all bindings in [m]
    (in increasing order), and [d1 ... dN] are the associated data. *)

val forAll0: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t0 -> bool
val forAll: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t -> bool
(** [for_all p m] checks if all the bindings of the map
    satisfy the predicate [p].
    @since 3.12.0
*)

val exists0: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t0 -> bool
val exists: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t -> bool
(** [exists p m] checks if at least one binding of the map
    satisfy the predicate [p].
    @since 3.12.0
*)

val filter0: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t0 -> ('k, 'a, 'id) t0
val filter: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t
(** [filter p m] returns the map with all the bindings in [m]
    that satisfy predicate [p].
    @since 3.12.0
*)

val partition0: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t0 -> ('k, 'a, 'id) t0 * ('k, 'a, 'id) t0
val partition: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t * ('k, 'a, 'id) t
(** [partition p m] returns a pair of maps [(m1, m2)], where
    [m1] contains all the bindings of [s] that satisfy the
    predicate [p], and [m2] is the map with all the bindings of
    [s] that do not satisfy [p].
    @since 3.12.0
*)

val cardinal0: ('k, 'a, 'id) t0 -> int
val cardinal: ('k, 'a, 'id) t -> int
(** Return the number of bindings of a map.
    @since 3.12.0
*)

val bindings0: ('k, 'a, 'id) t0 -> ('k * 'a) list
val bindings: ('k, 'a, 'id) t -> ('k * 'a) list
(** Return the list of all bindings of the given map.
    The returned list is sorted in increasing order with respect
    to the ordering [Ord.compare], where [Ord] is the argument
    given to {!Map.Make}.
    @since 3.12.0
*)

val minBinding0: ('k, 'a, 'id) t0 -> ('k * 'a) option
val minBinding: ('k, 'a, 'id) t -> ('k * 'a) option
(** Return the smallest binding of the given map
    (with respect to the [Ord.compare] ordering), or raise
    [Not_found] if the map is empty.
    @since 3.12.0
*)

val maxBinding0: ('k, 'a, 'id) t0 -> ('k * 'a) option
val maxBinding: ('k, 'a, 'id) t -> ('k * 'a) option
(** Same as {!Map.S.min_binding}, but returns the largest binding
    of the given map.
    @since 3.12.0
*)



val split0: 
    cmp: ('k,'id) Bs_Cmp.cmp ->
    'k -> ('k, 'a, 'id) t0 -> ('k, 'a, 'id) t0 * 'a option * ('k, 'a, 'id) t0
val split: 'k -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t * 'a option * ('k, 'a, 'id) t
(** [split x m] returns a triple [(l, data, r)], where
      [l] is the map with all the bindings of [m] whose 'k
    is strictly less than [x];
      [r] is the map with all the bindings of [m] whose 'k
    is strictly greater than [x];
      [data] is [None] if [m] contains no binding for [x],
      or [Some v] if [m] binds [v] to [x].
    @since 3.12.0
*)


val findOpt0: 
    cmp: ('k,'id) Bs_Cmp.cmp -> 
    'k -> ('k, 'a, 'id) t0 -> 'a option
val findOpt: 'k -> ('k, 'a, 'id) t -> 'a option
(** [find x m] returns the current binding of [x] in [m],
    or raises [Not_found] if no such binding exists. *)

val findAssert0: 
    cmp: ('k,'id) Bs_Cmp.cmp -> 
    'k -> ('k, 'a, 'id) t0 -> 'a 
val findAssert: 'k -> ('k, 'a, 'id) t -> 'a

val findWithDefault0: 
    cmp: ('k,'id) Bs_Cmp.cmp -> 
    def:'a -> 
    'k -> ('k, 'a, 'id) t0 -> 'a 
val findWithDefault:
    def:'a -> 
    'k -> ('k, 'a, 'id) t -> 'a 


val map0: ('a -> 'b [@bs]) -> ('k, 'a, 'id) t0 -> ('k ,'b,'id ) t0    
val map: ('a -> 'b [@bs]) -> ('k, 'a, 'id) t -> ('k ,'b,'id ) t
(** [map f m] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The bindings are passed to [f] in increasing order
    with respect to the ordering over the type of the keys. *)

val mapi0: ('k -> 'a -> 'b [@bs]) -> ('k, 'a, 'id) t0 -> ('k, 'b, 'id) t0    
val mapi: ('k -> 'a -> 'b [@bs]) -> ('k, 'a, 'id) t -> ('k, 'b, 'id) t
(** Same as {!Map.S.map}, but the function receives as arguments both the
    'k and the associated value for each binding of the map. *)

