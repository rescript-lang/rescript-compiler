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


type ('k,'v,'id) t

type ('key, 'id ) dict = ('key, 'id) Bs_Cmp.t
(** The data associated with a comparison function *)   

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

val empty: dict:('k, 'id) dict -> ('k, 'a, 'id) t 
val isEmpty: _ t -> bool

val has: ('k, 'a, 'id) t -> 'k  -> bool    

val cmp:
    ('k, 'v, 'id) t ->
    ('k, 'v, 'id) t ->
    ('v -> 'v -> int [@bs]) ->
     int

val eq:  
    ('k, 'a, 'id) t -> 
    ('k, 'a, 'id) t -> 
    ('a -> 'a -> bool [@bs]) -> 
    bool
(** [eq m1 m2 cmp] tests whether the maps [m1] and [m2] are
    equal, that is, contain equal keys and associate them with
    equal data.  [cmp] is the equality predicate used to compare
    the data associated with the keys. *)
    
val forEach:  ('k, 'a, 'id) t -> ('k -> 'a -> unit [@bs]) -> unit
(** [forEach m f] applies [f] to all bindings in map [m].
    [f] receives the 'k as first argument, and the associated value
    as second argument.  The bindings are passed to [f] in increasing
    order with respect to the ordering over the type of the keys. *)
    
val reduce: ('k, 'a, 'id) t -> 'b ->  ('b -> 'k -> 'a -> 'b [@bs]) ->  'b
(** [reduce m a f] computes [(f kN dN ... (f k1 d1 a)...)],
    where [k1 ... kN] are the keys of all bindings in [m]
    (in increasing order), and [d1 ... dN] are the associated data. *)

val every: ('k, 'a, 'id) t -> ('k -> 'a -> bool [@bs]) ->  bool
(** [every m p] checks if all the bindings of the map
    satisfy the predicate [p]. Order unspecified *)
    
val some: ('k, 'a, 'id) t -> ('k -> 'a -> bool [@bs]) ->  bool
(** [some m p] checks if at least one binding of the map
    satisfy the predicate [p]. Order unspecified *)

val size: ('k, 'a, 'id) t -> int
val toList: ('k, 'a, 'id) t -> ('k * 'a) list
(** In increasing order*)
val toArray: ('k, 'a, 'id) t -> ('k * 'a) array
val ofArray:  ('k * 'a) array -> dict:('k,'id) dict -> ('k,'a,'id) t         
val keysToArray: ('k, 'a, 'id) t -> 'k  array
val valuesToArray: ('k, 'a, 'id) t -> 'a  array
val minKey: ('k, _, _) t -> 'k option
val minKeyUndefined: ('k, _, _) t -> 'k Js.undefined
val maxKey: ('k, _, _) t -> 'k option
val maxKeyUndefined: ('k, _, _) t -> 'k Js.undefined
val minimum: ('k, 'a,  _) t -> ('k * 'a) option
val minUndefined: ('k, 'a, _) t -> ('k * 'a) Js.undefined
val maximum: ('k, 'a, _) t -> ('k * 'a) option
val maxUndefined:('k, 'a, _) t -> ('k * 'a) Js.undefined
val get:  ('k, 'a, 'id) t -> 'k -> 'a option
val getUndefined: ('k, 'a, 'id) t -> 'k ->  'a Js.undefined
val getWithDefault:
    ('k, 'a, 'id) t -> 'k ->  'a -> 'a 
val getExn:  ('k, 'a, 'id) t -> 'k -> 'a 
val checkInvariantInternal: _ t -> bool   
(****************************************************************************)

val remove:  ('k, 'a, 'id) t -> 'k -> ('k, 'a, 'id) t
(** [remove m x] when [x] is not in [m], [m] is returned reference unchanged *)
val removeMany: ('k, 'a, 'id) t -> 'k array -> ('k, 'a, 'id) t  
  
val set: 
    ('k, 'a, 'id) t -> 'k -> 'a ->  ('k, 'a, 'id) t
(** [set m x y ] returns a map containing the same bindings as
    [m], with a new binding of [x] to [y]. If [x] was already bound
    in [m], its previous binding disappears. *)
val update: ('k, 'a, 'id) t -> 'k -> ('a option -> 'a option [@bs]) -> ('k, 'a, 'id) t      
val merge:
    ('k, 'a, 'id) t -> ('k * 'a) array ->  ('k, 'a, 'id) t

val merge:
   ('k, 'a, 'id ) t -> 
   ('k, 'b,'id) t ->
   ('k -> 'a option -> 'b option -> 'c option [@bs]) -> 
   ('k, 'c,'id) t
(** [merge m1 m2 f] computes a map whose keys is a subset of keys of [m1]
    and of [m2]. The presence of each such binding, and the corresponding
    value, is determined with the function [f].
*)    

val mergeMany:
  ('a, 'b, 'id) t ->
  ('a * 'b) array ->
  ('a, 'b, 'id) t

val keepBy: 
    ('k, 'a, 'id) t -> 
    ('k -> 'a -> bool [@bs]) -> 
    ('k, 'a, 'id) t
(** [keepBy m p] returns the map with all the bindings in [m]
    that satisfy predicate [p]. *)
    
val partition: 
    ('k, 'a, 'id) t ->
    ('k -> 'a -> bool [@bs]) -> 
    ('k, 'a, 'id) t * ('k, 'a, 'id) t
(** [partition m p] returns a pair of maps [(m1, m2)], where
    [m1] contains all the bindings of [s] that satisfy the
    predicate [p], and [m2] is the map with all the bindings of
    [s] that do not satisfy [p].
*)

val split: 
    ('k, 'a, 'id) t -> 'k -> 
    (('k, 'a, 'id) t * ('k, 'a, 'id) t )* 'a option 
(** [split x m] returns a triple [(l, data, r)], where
      [l] is the map with all the bindings of [m] whose 'k
    is strictly less than [x];
      [r] is the map with all the bindings of [m] whose 'k
    is strictly greater than [x];
      [data] is [None] if [m] contains no binding for [x],
      or [Some v] if [m] binds [v] to [x].
*)

val map: ('k, 'a, 'id) t -> ('a -> 'b [@bs]) ->  ('k ,'b,'id ) t
(** [map m f] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The bindings are passed to [f] in increasing order
    with respect to the ordering over the type of the keys. *)

val mapWithKey: ('k, 'a, 'id) t -> ('k -> 'a -> 'b [@bs]) -> ('k, 'b, 'id) t


val getDict: ('a, 'b, 'c) t -> ('a, 'c) dict

val getData: ('a, 'b, 'c) t -> ('a, 'b, 'c) Bs_SortedMapDict.t
    
val packDictData: dict:('a, 'b) dict -> data:('a, 'c, 'b) Bs_SortedMapDict.t -> ('a, 'c, 'b) t

module Int = Bs_MapInt
module String = Bs_MapString  
