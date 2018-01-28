
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

type ('k,'id) t 


val empty: dict:('elt, 'id) Bs_Cmp.t -> ('elt, 'id) t

val ofArray: 'k array -> dict:('k, 'id) Bs_Cmp.t ->   ('k, 'id) t
val ofSortedArrayUnsafe: 'elt array -> dict:('elt, 'id) Bs_Cmp.t ->  ('elt,'id) t
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

val forEach: ('elt, 'id) t -> ('elt -> unit [@bs]) ->  unit
(** [forEach m f] applies [f] in turn to all elements of [m].
    In increasing order *)
val reduce: ('elt, 'id) t -> 'a  -> ('a -> 'elt -> 'a [@bs]) ->  'a
(** In increasing order. *)
val every: ('elt, 'id) t -> ('elt -> bool [@bs]) -> bool
(** [for_all p s] checks if all elements of the set
    satisfy the predicate [p]. Order unspecified *)
val some: ('elt, 'id) t ->  ('elt -> bool [@bs]) -> bool
(** [some p s] checks if at least one element of
    the set satisfies the predicate [p]. *)

val keepBy: ('elt, 'id) t ->  ('elt -> bool [@bs]) -> ('elt, 'id) t
(** [keepBy p s] returns the set of all elements in [s]
    that satisfy predicate [p]. *)    
val partition: ('elt, 'id) t -> ('elt -> bool [@bs]) ->  ('elt, 'id) t * ('elt, 'id) t
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

val checkInvariantInternal: _ t -> bool

(*
  [add0] was not exposed for various reasons:
  1. such api is dangerious
  [ cmp: ('elt,'id) Bs_Cmp.cmp -> 
    ('elt, 'id) t0 -> 'elt ->  
    ('elt, 'id) t0]
  2. It is not really significantly more *)
