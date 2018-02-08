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
(*  Adapted by authors of BuckleScript without using functors          *)
(***********************************************************************)

(** A {i immutable} sorted map module which allows customize {i compare} behavior.

   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.

   All data are parameterized by not its only type but also a unique identity in
   the time of initialization, so that two {i Map of int keys} initialized with different
   {i compare} functions will have different type.

   For example:
   {[
     type t = int * int 
      module I0 =
        (val Belt.Id.comparableU ~cmp:(fun[\@bs] ((a0,a1) : t) ((b0,b1) : t) ->
             match Pervasives.compare a0 b0 with
             | 0 -> Pervasives.compare a1 b1
             | c -> c 
           ))
    let s0 : (_, string, _) t = make ~id:(module I0) (* value is of type string *)
    module I1 =
      (val Belt.Id.comparableU ~cmp:(fun[\@bs] ((a0,a1) : t) ((b0,b1) : t) ->
           match compare a1 b1 with
           | 0 -> compare a0 b0
           | c -> c 
         ))
    let s1 : (_, string, _) t = make ~id:(module I1)
   ]}


   Here the compiler would infer [s0] and [s1] having different type so that
    it would not mix.

   {[
     val s0 : ((int * int), string,  I0.identity) t 
     val s1 : ((int * int), string,  I1.identity) t
   ]}

   We can add elements to the collection:

   {[

     let s2 = add s1 (0,0) "a"
     let s3 = add s2 (1,1) "b"
   ]}

   Since this is an immutable data strucure, [s1] will be an empty map
   while [s2] will contain one pair, [s3] will contain two.

   The [merge s0 s3 callback] will result in a type error, since their identity mismatch
*)


(** Specalized when key type is [int], more efficient
    than the gerneic type, its compare behavior is fixed using the built-in comparison
*)
module Int = Bs_MapInt

(** specalized when key type is [string], more efficient
    than the gerneic type, its compare behavior is fixed using the built-in comparison *)  
module String = Bs_MapString

(** This module seprate identity from data, it is a bit more verbsoe but slightly
    more efficient due to the fact that there is no need to pack identity and data back
    after each operation
*)  
module Dict = Bs_MapDict


type ('key,'value,'identity) t
(** [('key, 'identity) t]

    ['key] is the element type

    ['identity] the identity of the collection
*)


type ('key, 'id ) id = ('key, 'id) Bs_Id.comparable
(** The identity needed for making an empty map*)


(*
    How we retain soundness:
    The only way to create a value of type [_ t] from scratch 
    is through [empty] which requires [_ Bs_Id.t]
    The only way to create [_ Bs_Id.t] is using [Bs_Id.Make] which
    will create a fresh type [id] per module

    Generic operations over tree without [cmp] are still exported 
    (for efficient reasons) so that [data] does not need be boxed and unboxed.

    The soundness is guaranteed in two aspects:
    When create a value of [_ t] it needs both [_ Bs_Id.t] and [_ t0].
    [_ Bs_Id.t] is an abstract type. Note [add0] requires [_ Bs_Id.cmp] which 
    is also an abstract type which can only come from [_ Bs_Id.t]

    When destructing a value of [_ t], the ['id] parameter is threaded.

*)

(* should not export [Bs_Id.compare]. 
   should only export [Bs_Id.t] or [Bs_Id.cmp] instead *)


val make: id:('k, 'id) id -> ('k, 'a, 'id) t
(** [make ~id]
   
     @example {[
     module IntCmp = (val IntCmp.comparable ~cmp:(fun (x:int) y -> Pervasives.comapre x y))
     let s = make ~id:(module IntCmp)
    ]}

*)


val isEmpty: _ t -> bool

val has: ('k, 'a, 'id) t -> 'k  -> bool    

val cmpU:
    ('k, 'v, 'id) t ->
    ('k, 'v, 'id) t ->
    ('v -> 'v -> int [@bs]) ->
     int
val cmp:
    ('k, 'v, 'id) t ->
    ('k, 'v, 'id) t ->
    ('v -> 'v -> int ) ->
     int

val eqU:  
    ('k, 'a, 'id) t -> 
    ('k, 'a, 'id) t -> 
    ('a -> 'a -> bool [@bs]) -> 
    bool
val eq:  
    ('k, 'a, 'id) t -> 
    ('k, 'a, 'id) t -> 
    ('a -> 'a -> bool) -> 
    bool
(** [eq m1 m2 cmp] tests whether the maps [m1] and [m2] are
    equal, that is, contain equal keys and associate them with
    equal data.  [cmp] is the equality predicate used to compare
    the data associated with the keys. *)

val forEachU:  ('k, 'a, 'id) t -> ('k -> 'a -> unit [@bs]) -> unit
val forEach:  ('k, 'a, 'id) t -> ('k -> 'a -> unit) -> unit
(** [forEach m f] applies [f] to all bindings in map [m].
    [f] receives the 'k as first argument, and the associated value
    as second argument.  The bindings are passed to [f] in increasing
    order with respect to the ordering over the type of the keys. *)

val reduceU: ('k, 'a, 'id) t -> 'b ->  ('b -> 'k -> 'a -> 'b [@bs]) ->  'b
val reduce: ('k, 'a, 'id) t -> 'b ->  ('b -> 'k -> 'a -> 'b) ->  'b
(** [reduce m a f] computes [(f kN dN ... (f k1 d1 a)...)],
    where [k1 ... kN] are the keys of all bindings in [m]
    (in increasing order), and [d1 ... dN] are the associated data. *)

val everyU: ('k, 'a, 'id) t -> ('k -> 'a -> bool [@bs]) ->  bool
val every: ('k, 'a, 'id) t -> ('k -> 'a -> bool) ->  bool
(** [every m p] checks if all the bindings of the map
    satisfy the predicate [p]. Order unspecified *)

val someU: ('k, 'a, 'id) t -> ('k -> 'a -> bool [@bs]) ->  bool
val some: ('k, 'a, 'id) t -> ('k -> 'a -> bool) ->  bool
(** [some m p] checks if at least one binding of the map
    satisfy the predicate [p]. Order unspecified *)

val size: ('k, 'a, 'id) t -> int
val toList: ('k, 'a, 'id) t -> ('k * 'a) list
(** In increasing order*)
val toArray: ('k, 'a, 'id) t -> ('k * 'a) array
val ofArray:  ('k * 'a) array -> id:('k,'id) id -> ('k,'a,'id) t         
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
val updateU: ('k, 'a, 'id) t -> 'k -> ('a option -> 'a option [@bs]) -> ('k, 'a, 'id) t      
val update: ('k, 'a, 'id) t -> 'k -> ('a option -> 'a option) -> ('k, 'a, 'id) t      
val mergeMany:
    ('k, 'a, 'id) t -> ('k * 'a) array ->  ('k, 'a, 'id) t

val mergeU:
   ('k, 'a, 'id ) t -> 
   ('k, 'b,'id) t ->
   ('k -> 'a option -> 'b option -> 'c option [@bs]) -> 
   ('k, 'c,'id) t
val merge:
   ('k, 'a, 'id ) t -> 
   ('k, 'b,'id) t ->
   ('k -> 'a option -> 'b option -> 'c option) -> 
   ('k, 'c,'id) t
(** [merge m1 m2 f] computes a map whose keys is a subset of keys of [m1]
    and of [m2]. The presence of each such binding, and the corresponding
    value, is determined with the function [f].
*)    


val keepU: 
    ('k, 'a, 'id) t -> 
    ('k -> 'a -> bool [@bs]) -> 
    ('k, 'a, 'id) t
val keep: 
    ('k, 'a, 'id) t -> 
    ('k -> 'a -> bool) -> 
    ('k, 'a, 'id) t
(** [keep m p] returns the map with all the bindings in [m]
    that satisfy predicate [p]. *)
    
val partitionU: 
    ('k, 'a, 'id) t ->
    ('k -> 'a -> bool [@bs]) -> 
    ('k, 'a, 'id) t * ('k, 'a, 'id) t
val partition: 
    ('k, 'a, 'id) t ->
    ('k -> 'a -> bool) -> 
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

val mapU: ('k, 'a, 'id) t -> ('a -> 'b [@bs]) ->  ('k ,'b,'id ) t
val map: ('k, 'a, 'id) t -> ('a -> 'b) ->  ('k ,'b,'id ) t
(** [map m f] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The bindings are passed to [f] in increasing order
    with respect to the ordering over the type of the keys. *)

val mapWithKeyU: ('k, 'a, 'id) t -> ('k -> 'a -> 'b [@bs]) -> ('k, 'b, 'id) t
val mapWithKey: ('k, 'a, 'id) t -> ('k -> 'a -> 'b) -> ('k, 'b, 'id) t

val getId: ('a, 'b, 'c) t -> ('a, 'c) id

val getData: ('a, 'b, 'c) t -> ('a, 'b, 'c) Bs_MapDict.t
    
val packIdData: id:('a, 'b) id -> data:('a, 'c, 'b) Bs_MapDict.t -> ('a, 'c, 'b) t

