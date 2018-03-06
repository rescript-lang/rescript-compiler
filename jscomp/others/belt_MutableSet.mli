
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
        (val Belt.Id.comparableU (fun[\@bs] ((a0,a1) : t) ((b0,b1) : t) ->
             match Pervasives.compare a0 b0 with
             | 0 -> Pervasives.compare a1 b1
             | c -> c 
           ))
    let s0 = make ~id:(module I0)
    module I1 =
      (val Belt.Id.comparableU (fun[\@bs] ((a0,a1) : t) ((b0,b1) : t) ->
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

val make: id:('value, 'id) id -> ('value, 'id) t

val ofArray: 'k array -> id:('k, 'id) id ->   ('k, 'id) t
val ofSortedArrayUnsafe: 'value array -> id:('value, 'id) id ->  ('value,'id) t
val copy: ('k, 'id) t -> ('k, 'id) t     
val isEmpty: _ t -> bool
val has:  ('value, _) t -> 'value ->  bool

val add: ('value, 'id) t -> 'value -> unit 

val addCheck:
  ('value, 'id) t -> 'value -> bool 

val mergeMany:
  ('value, 'id) t -> 'value array -> unit 

val remove: ('value, 'id) t -> 'value -> unit 

val removeCheck: ('value, 'id) t -> 'value -> bool
   (* [b = removeCheck s e] [b] is true means one element removed *)      

val removeMany:
  ('value, 'id) t -> 'value array -> unit 

val union: ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t
val intersect: ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t 
val diff: ('value, 'id) t -> ('value, 'id) t -> ('value, 'id) t 
val subset: ('value, 'id) t -> ('value, 'id) t -> bool     

val cmp:  
  ('value, 'id) t -> ('value, 'id) t -> int
val eq:  
  ('value, 'id) t -> ('value, 'id) t -> bool

val forEachU: ('value, 'id) t -> ('value -> unit [@bs]) ->  unit
val forEach: ('value, 'id) t -> ('value -> unit) ->  unit
(** [forEach m f] applies [f] in turn to all elements of [m].
    In increasing order *)
  
val reduceU: ('value, 'id) t -> 'a -> ('a -> 'value -> 'a [@bs]) -> 'a
val reduce: ('value, 'id) t -> 'a -> ('a -> 'value -> 'a) -> 'a  
(** In increasing order. *)
  
val everyU: ('value, 'id) t -> ('value -> bool [@bs]) -> bool
val every: ('value, 'id) t -> ('value -> bool) -> bool  
(** [every s p] checks if all elements of the set
    satisfy the predicate [p]. Order unspecified *)

val someU: ('value, 'id) t ->  ('value -> bool [@bs]) -> bool
val some: ('value, 'id) t ->  ('value -> bool) -> bool  
(** [some p s] checks if at least one element of
    the set satisfies the predicate [p]. *)

val keepU: ('value, 'id) t -> ('value -> bool [@bs]) -> ('value, 'id) t
val keep: ('value, 'id) t -> ('value -> bool) -> ('value, 'id) t    
(** [keep s p] returns the set of all elements in [s]
    that satisfy predicate [p]. *)    

val partitionU: ('value, 'id) t -> ('value -> bool [@bs]) -> ('value, 'id) t * ('value, 'id) t
val partition: ('value, 'id) t -> ('value -> bool) -> ('value, 'id) t * ('value, 'id) t                                                           
(** [partition p s] returns a pair of sets [(s1, s2)], where
    [s1] is the set of all the elements of [s] that satisfy the
    predicate [p], and [s2] is the set of all the elements of
    [s] that do not satisfy [p]. *)


val size:  ('value, 'id) t -> int    
val toList: ('value, 'id) t -> 'value list
(** In increasing order*)
val toArray: ('value, 'id) t -> 'value array

val minimum: ('value, 'id) t -> 'value option
val minUndefined: ('value, 'id) t -> 'value Js.undefined
val maximum: ('value, 'id) t -> 'value option
val maxUndefined: ('value, 'id) t -> 'value Js.undefined

val get: ('value, 'id) t -> 'value -> 'value option 
val getUndefined: ('value, 'id) t -> 'value -> 'value Js.undefined
val getExn: ('value, 'id) t -> 'value -> 'value 


val split: ('value, 'id) t -> 'value ->  (('value, 'id) t * ('value, 'id) t) * bool
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
   {b raise} when invariant is not held
*)  

(*
  [add0] was not exposed for various reasons:
  1. such api is dangerious
  [ cmp: ('value,'id) Belt_Cmp.cmp -> 
    ('value, 'id) t0 -> 'value ->  
    ('value, 'id) t0]
  2. It is not really significantly more *)

