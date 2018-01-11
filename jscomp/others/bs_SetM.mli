
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


val empty : ('elt, 'id) Bs_Cmp.t -> ('elt, 'id) t

val ofArray: ('k, 'id) Bs_Cmp.t -> 'k array -> ('k, 'id) t 

val isEmpty : _ t -> bool

val mem:  ('elt, _) t -> 'elt ->  bool

val addOnly:   
  ('elt, 'id) t -> 'elt -> unit 

val add:   
  ('elt, 'id) t -> 'elt -> ('elt, 'id) t

val addCheck:
  ('elt, 'id) t -> 'elt -> bool 

val addArrayOnly:
  ('elt, 'id) t -> 'elt array -> unit 
  
val addArray:
  ('elt, 'id) t -> 'elt array -> ('elt, 'id) t
  
val removeArrayOnly:
  ('elt, 'id) t -> 'elt array -> unit 

val removeArray:
  ('elt, 'id) t -> 'elt array -> ('elt, 'id) t
  
val singleton : 
  ('elt,'id) Bs_Cmp.t -> 
  'elt -> ('elt, 'id) t
(** [singleton x] returns the one-element set containing only [x]. *)

val removeOnly:
   ('elt, 'id) t -> 'elt -> unit 

val remove:  
   ('elt, 'id) t -> 'elt -> ('elt, 'id) t

val removeCheck:  
   ('elt, 'id) t -> 'elt -> bool
(* [b = removeCheck s e] [b] is true means one element removed *)   


val union:  
  ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t

val inter: 
  ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t 

val diff:   
  ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t 
  
val subset:  
  ('elt, 'id) t -> ('elt, 'id) t -> bool     

val cmp:  
  ('elt, 'id) t -> ('elt, 'id) t -> int
(** Total ordering between sets. Can be used as the ordering function
    for doing sets of sets. *)
val eq:  
  ('elt, 'id) t -> ('elt, 'id) t -> bool

val iter: ('elt, 'id) t -> ('elt -> unit [@bs]) ->  unit
(** [iter s f] applies [f] in turn to all elements of [s].
    In increasing order *)
val fold: ('elt, 'id) t -> 'a  -> ('a -> 'elt -> 'a [@bs]) ->  'a
(** In increasing order. *)
val forAll: ('elt, 'id) t -> ('elt -> bool [@bs]) -> bool
(** [for_all p s] checks if all elements of the set
    satisfy the predicate [p]. Order unspecified *)

val exists: ('elt, 'id) t ->  ('elt -> bool [@bs]) -> bool
(** [exists p s] checks if at least one element of
    the set satisfies the predicate [p]. *)
val filter: ('elt, 'id) t ->  ('elt -> bool [@bs]) -> ('elt, 'id) t
(** [filter p s] returns the set of all elements in [s]
    that satisfy predicate [p]. *)    
val partition: ('elt, 'id) t -> ('elt -> bool [@bs]) ->  ('elt, 'id) t * ('elt, 'id) t
(** [partition p s] returns a pair of sets [(s1, s2)], where
    [s1] is the set of all the elements of [s] that satisfy the
    predicate [p], and [s2] is the set of all the elements of
    [s] that do not satisfy [p]. *)

val length:  ('elt, 'id) t -> int

    
val toList: ('elt, 'id) t -> 'elt list
(** In increasing order*)

val toArray: ('elt, 'id) t -> 'elt array

val minOpt: ('elt, 'id) t -> 'elt option
val minNull: ('elt, 'id) t -> 'elt Js.null
val maxOpt: ('elt, 'id) t -> 'elt option
val maxNull: ('elt, 'id) t -> 'elt Js.null
val split: 
   ('elt, 'id) t -> 'elt ->  (('elt, 'id) t * ('elt, 'id) t) * bool
(** [split s x] returns a triple [((l, r), present)], where
      [l] is the set of elements of [s] that are
      strictly less than [x];
      [r] is the set of elements of [s] that are
      strictly greater than [x];
      [present] is [false] if [s] contains no element equal to [x],
      or [true] if [s] contains an element equal to [x].
    [l,r] are freshly made, no sharing with [s]   
*)

val ofSortedArrayUnsafe:
  dict:('elt, 'id) Bs_Cmp.t ->
  'elt array -> ('elt,'id) t

val findOpt: 
  ('elt, 'id) t -> 'elt -> 'elt option 
val findNull:   
  ('elt, 'id) t -> 'elt -> 'elt Js.null
(** [findOpt s ele] 
    return the element in the collection 
    which is semantically equal to it 
 *)

(* No need
  could be made use of by 
  [Js.assertNonNull (findNull s x)]
 *)
(* val findAssert:  
  ('elt, 'id) t -> 'elt -> 'elt   *)

(*
  [add0] was not exposed for various reasons:
  1. such api is dangerious
  [ cmp: ('elt,'id) Bs_Cmp.cmp -> 
    ('elt, 'id) t0 -> 'elt ->  
    ('elt, 'id) t0]
  2. It is not really significantly more *)