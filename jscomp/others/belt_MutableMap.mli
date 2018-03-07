
(* Copyright (C) 2017 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** A {b mutable} sorted map module which allows customize {i compare} behavior.

   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.

   All data are parameterized by not its only type but also a unique identity in
   the time of initialization, so that two {i Sets of ints} initialized with different
   {i compare} functions will have different type.

   For example:
   {[
     type t = int * int 
      module I0 =
        (val Belt.Id.comparableU (fun[\@bs] ((a0,a1) : t) ((b0,b1) : t) ->
             match Pervasives.compare a0 b0 with
             | 0 -> Pervasives.compare a1 b1
             | c -> c 
           ))
    let s0 : (_, string,_) t = make ~id:(module I0)
    module I1 =
      (val Belt.Id.comparableU (fun[\@bs] ((a0,a1) : t) ((b0,b1) : t) ->
           match compare a1 b1 with
           | 0 -> compare a0 b0
           | c -> c 
         ))
    let s1 : (_, string, _) t = make ~id:(module I1)
   ]}


   Here the compiler would infer [s0] and [s1] having different type so that
    it would not mix.

   {[
     val s0 : ((int * int), string, I0.identity) t 
     val s1 : ((int * int), string, I1.identity) t 
   ]}

   We can add elements to the collection:

   {[

     let () =
       add s1 (0,0) "a";
       add s1 (1,1) "b"
   ]}

   Since this is an mutable data strucure, [s1] will contain two pairs.

*)



module Int = Belt_MutableMapInt

module String = Belt_MutableMapString


type ('k,'v,'id) t
type ('key, 'id) id = ('key, 'id) Belt_Id.comparable
    
val make: id:('k, 'id) id -> ('k, 'a, 'id) t
val clear: _ t -> unit 
val isEmpty: _ t -> bool
val has: ('k, _, _) t -> 'k  -> bool

val cmpU: 
    ('k, 'a, 'id) t -> 
    ('k, 'a, 'id) t ->
    ('a -> 'a -> int [@bs]) -> 
     int
val cmp: 
    ('k, 'a, 'id) t -> 
    ('k, 'a, 'id) t ->
    ('a -> 'a -> int ) -> 
     int     
(** [cmp m1 m2 cmp]
    First compare by size, if size is the same,
    compare by key, value pair
*)     
val eqU:  ('k, 'a, 'id) t -> ('k, 'a, 'id) t -> ('a -> 'a -> bool [@bs]) -> bool
val eq:  ('k, 'a, 'id) t -> ('k, 'a, 'id) t -> ('a -> 'a -> bool) -> bool  
(** [eq m1 m2 eqf] tests whether the maps [m1] and [m2] are
    equal, that is, contain equal keys and associate them with
    equal data.  [eqf] is the equality predicate used to compare
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
    satisfy the predicate [p].
*)
    

val someU: ('k, 'a, 'id) t -> ('k -> 'a -> bool [@bs]) ->  bool
val some: ('k, 'a, 'id) t -> ('k -> 'a -> bool) ->  bool
(** [some m p] checks if at least one binding of the map
    satisfy the predicate [p].
*)
val size: ('k, 'a, 'id) t -> int


val toList: ('k, 'a, 'id) t -> ('k * 'a) list
(** In increasing order*)
val toArray: ('k, 'a, 'id) t -> ('k * 'a) array
val fromArray: ('k * 'a) array -> id:('k,'id) id ->  ('k,'a,'id) t    
val keysToArray: ('k, _, _) t -> 'k array 
val valuesToArray: (_, 'a, _) t -> 'a array
val minKey: ('k, _,  _) t -> 'k option
val minKeyUndefined: ('k, _,  _) t -> 'k Js.undefined
val maxKey: ('k, _,  _) t -> 'k option
val maxKeyUndefined: ('k, _,  _) t -> 'k Js.undefined
val minimum: ('k, 'a,  _) t -> ('k * 'a) option
val minUndefined: ('k, 'a, _) t -> ('k * 'a) Js.undefined
val maximum: ('k, 'a, _) t -> ('k * 'a) option
val maxUndefined:('k, 'a, _) t -> ('k * 'a) Js.undefined
val get:  ('k, 'a, 'id) t -> 'k -> 'a option
val getUndefined: ('k, 'a, 'id) t -> 'k ->  'a Js.undefined
val getWithDefault:
    ('k, 'a, 'id) t -> 'k ->  'a -> 'a 
val getExn:  ('k, 'a, 'id) t -> 'k ->  'a 
val checkInvariantInternal: _ t -> unit
(**
   {b raise} when invariant is not held
*)  
  
val ofArray: ('k * 'a) array -> id:('k,'id) id ->  ('k,'a,'id) t    
[@@ocaml.deprecated "Use fromArray instead"]

(****************************************************************************)

(*TODO: add functional [merge, partition, keep, split]*)

val remove:  ('k, 'a, 'id) t -> 'k -> unit
(** [remove m x] do the in-place modification,
*)
val removeMany: ('k, 'a, 'id) t -> 'k array -> unit    
  
val set: ('k, 'a, 'id) t -> 'k -> 'a ->  unit
(** [set m x y ] do the in-place modification *)

val updateU: ('k, 'a, 'id) t -> 'k -> ('a option -> 'a option [@bs]) -> unit
val update: ('k, 'a, 'id) t -> 'k -> ('a option -> 'a option) -> unit

val mergeMany:  ('k, 'a, 'id) t -> ('k * 'a) array ->  unit

val mapU: ('k, 'a, 'id) t -> ('a -> 'b [@bs]) ->  ('k ,'b,'id ) t
val map: ('k, 'a, 'id) t -> ('a -> 'b) ->  ('k ,'b,'id ) t
(** [map m f] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The bindings are passed to [f] in increasing order
    with respect to the ordering over the type of the keys. *)

val mapWithKeyU: ('k, 'a, 'id) t -> ('k -> 'a -> 'b [@bs]) -> ('k, 'b, 'id) t
val mapWithKey: ('k, 'a, 'id) t -> ('k -> 'a -> 'b) -> ('k, 'b, 'id) t
    


