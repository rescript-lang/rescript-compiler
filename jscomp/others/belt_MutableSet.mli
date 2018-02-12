
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

(** A {i mutable} sorted set module which allows customize {i compare} behavior.

   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.

   All data are parameterized by not its only type but also a unique identity in
   the time of initialization, so that two {i Sets of ints} initialized with different
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
    let s0 = make ~id:(module I0)
    module I1 =
      (val Belt.Id.comparableU ~cmp:(fun[\@bs] ((a0,a1) : t) ((b0,b1) : t) ->
           match compare a1 b1 with
           | 0 -> compare a0 b0
           | c -> c 
         ))
    let s1 = make ~id:(module I1)
   ]}


   Here the compiler would infer [s0] and [s1] having different type so that
    it would not mix.

   {[
     val s0 : ((int * int), I0.identity) t 
     val s1 : ((int * int), I1.identity) t 
   ]}

   We can add elements to the collection:

   {[

     let () =
       add s1 (0,0);
       add s1 (1,1)
   ]}

   Since this is an mutable data strucure, [s1] will contain two pairs.

*)


(** Specalized when key type is [int], more efficient
    than the gerneic type
*)
module Int = Belt_MutableSetInt

(** Specalized when key type is [string], more efficient
    than the gerneic type *)  
module String = Belt_MutableSetString

type ('k,'id) t 

type ('k, 'id) id = ('k, 'id) Belt_Id.comparable

val make: id:('elt, 'id) id -> ('elt, 'id) t

val ofArray: 'k array -> id:('k, 'id) id ->   ('k, 'id) t
val ofSortedArrayUnsafe: 'elt array -> id:('elt, 'id) id ->  ('elt,'id) t
val copy: ('k, 'id) t -> ('k, 'id) t     
val isEmpty: _ t -> bool
val has:  ('elt, _) t -> 'elt ->  bool

val add: ('elt, 'id) t -> 'elt -> unit 

val addCheck:
  ('elt, 'id) t -> 'elt -> bool 

val mergeMany:
  ('elt, 'id) t -> 'elt array -> unit 

val remove: ('elt, 'id) t -> 'elt -> unit 

val removeCheck: ('elt, 'id) t -> 'elt -> bool
   (* [b = removeCheck s e] [b] is true means one element removed *)      

val removeMany:
  ('elt, 'id) t -> 'elt array -> unit 

val union: ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t
val intersect: ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t 
val diff: ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t 
val subset: ('elt, 'id) t -> ('elt, 'id) t -> bool     

val cmp:  
  ('elt, 'id) t -> ('elt, 'id) t -> int
val eq:  
  ('elt, 'id) t -> ('elt, 'id) t -> bool

val forEachU: ('elt, 'id) t -> ('elt -> unit [@bs]) ->  unit
val forEach: ('elt, 'id) t -> ('elt -> unit) ->  unit
(** [forEach m f] applies [f] in turn to all elements of [m].
    In increasing order *)
  
val reduceU: ('elt, 'id) t -> 'a -> ('a -> 'elt -> 'a [@bs]) -> 'a
val reduce: ('elt, 'id) t -> 'a -> ('a -> 'elt -> 'a) -> 'a  
(** In increasing order. *)
  
val everyU: ('elt, 'id) t -> ('elt -> bool [@bs]) -> bool
val every: ('elt, 'id) t -> ('elt -> bool) -> bool  
(** [every s p] checks if all elements of the set
    satisfy the predicate [p]. Order unspecified *)

val someU: ('elt, 'id) t ->  ('elt -> bool [@bs]) -> bool
val some: ('elt, 'id) t ->  ('elt -> bool) -> bool  
(** [some p s] checks if at least one element of
    the set satisfies the predicate [p]. *)

val keepU: ('elt, 'id) t -> ('elt -> bool [@bs]) -> ('elt, 'id) t
val keep: ('elt, 'id) t -> ('elt -> bool) -> ('elt, 'id) t    
(** [keep s p] returns the set of all elements in [s]
    that satisfy predicate [p]. *)    

val partitionU: ('elt, 'id) t -> ('elt -> bool [@bs]) -> ('elt, 'id) t * ('elt, 'id) t
val partition: ('elt, 'id) t -> ('elt -> bool) -> ('elt, 'id) t * ('elt, 'id) t                                                           
(** [partition p s] returns a pair of sets [(s1, s2)], where
    [s1] is the set of all the elements of [s] that satisfy the
    predicate [p], and [s2] is the set of all the elements of
    [s] that do not satisfy [p]. *)


val size:  ('elt, 'id) t -> int    
val toList: ('elt, 'id) t -> 'elt list
(** In increasing order*)
val toArray: ('elt, 'id) t -> 'elt array

val minimum: ('elt, 'id) t -> 'elt option
val minUndefined: ('elt, 'id) t -> 'elt Js.undefined
val maximum: ('elt, 'id) t -> 'elt option
val maxUndefined: ('elt, 'id) t -> 'elt Js.undefined

val get: ('elt, 'id) t -> 'elt -> 'elt option 
val getUndefined: ('elt, 'id) t -> 'elt -> 'elt Js.undefined
val getExn: ('elt, 'id) t -> 'elt -> 'elt 


val split: ('elt, 'id) t -> 'elt ->  (('elt, 'id) t * ('elt, 'id) t) * bool
(** [split s x] returns a triple [((l, r), present)], where
      [l] is the set of elements of [s] that are
      strictly less than [x];
      [r] is the set of elements of [s] that are
      strictly greater than [x];
      [present] is [false] if [s] contains no element equal to [x],
      or [true] if [s] contains an element equal to [x].
    [l,r] are freshly made, no sharing with [s]   
*)

val checkInvariantInternal: _ t -> unit
(**
   {b raise} when invariant is not helld
*)  

(*
  [add0] was not exposed for various reasons:
  1. such api is dangerious
  [ cmp: ('elt,'id) Belt_Cmp.cmp -> 
    ('elt, 'id) t0 -> 'elt ->  
    ('elt, 'id) t0]
  2. It is not really significantly more *)

