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
val singleton: 'elt -> dict:('elt,'id) Bs_Cmp.t -> ('elt, 'id) t
(** [singleton x] returns the one-element set containing only [x]. *)    
val ofArray: dict:('k, 'id) Bs_Cmp.t -> 'k array -> ('k, 'id) t
val ofSortedArrayUnsafe: 'elt array -> dict:('elt, 'id) Bs_Cmp.t -> ('elt,'id) t
    
val isEmpty: _ t -> bool
val has: ('elt, 'id) t -> 'elt ->  bool


val add:   
  ('elt, 'id) t -> 'elt -> ('elt, 'id) t
(** [add s x] If [x] was already in [s], [s] is returned unchanged. *)
val mergeArray: ('elt, 'id) t -> 'elt array -> ('elt, 'id) t 

val remove: ('elt, 'id) t -> 'elt -> ('elt, 'id) t
(** [remove m x] If [x] was not in [m], [m] is returned reference unchanged. *)

val removeArray:
  ('elt, 'id) t -> 'elt array -> ('elt, 'id) t 


val union: ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t
val inter: ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t 
val diff: ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t
val subset: ('elt, 'id) t -> ('elt, 'id) t -> bool     

val cmp: ('elt, 'id) t -> ('elt, 'id) t -> int
(** Total ordering between sets. Can be used as the ordering function
    for doing sets of sets. *)
val eq: ('elt, 'id) t -> ('elt, 'id) t -> bool

val forEach: ('elt, 'id) t -> ('elt -> unit [@bs]) ->  unit
(** [forEach s f] applies [f] in turn to all elements of [s].
    In increasing order *)
val reduce: ('elt, 'id) t -> 'a  -> ('a -> 'elt -> 'a [@bs]) ->  'a
(** In increasing order. *)
val every: ('elt, 'id) t -> ('elt -> bool [@bs]) -> bool
(** [every p s] checks if all elements of the set
    satisfy the predicate [p]. Order unspecified *)

val some: ('elt, 'id) t ->  ('elt -> bool [@bs]) -> bool
(** [some p s] checks if at least one element of
    the set satisfies the predicate [p]. *)
val keepBy: ('elt, 'id) t ->  ('elt -> bool [@bs]) -> ('elt, 'id) t
(** [keepBy m p] returns the set of all elements in [s]
    that satisfy predicate [p]. *)    
val partition: ('elt, 'id) t -> ('elt -> bool [@bs]) ->  ('elt, 'id) t * ('elt, 'id) t
(** [partition m p] returns a pair of sets [(s1, s2)], where
    [s1] is the set of all the elements of [s] that satisfy the
    predicate [p], and [s2] is the set of all the elements of
    [s] that do not satisfy [p]. *)

val size:  ('elt, 'id) t -> int
val toList: ('elt, 'id) t -> 'elt list
(** In increasing order*)
val toArray: ('elt, 'id) t -> 'elt array

val minimum: ('elt, 'id) t -> 'elt option
val minNull: ('elt, 'id) t -> 'elt Js.null
val maximum: ('elt, 'id) t -> 'elt option
val maxNull: ('elt, 'id) t -> 'elt Js.null



val get: ('elt, 'id) t -> 'elt -> 'elt option 
val getNull: ('elt, 'id) t -> 'elt -> 'elt Js.null
val getExn: ('elt, 'id) t -> 'elt -> 'elt 

val split: ('elt, 'id) t -> 'elt -> (('elt, 'id) t  * ('elt, 'id) t) * bool
                                    
val checkInvariant: _ t -> bool

(****************************************************************************)
(** Below are operations only when better performance needed,
    it is still safe API but more verbose.
    More API will be exposed by needs
*)
type ('k,'id) t0    
val getData: ('k,'id) t  -> ('k,'id) t0
val getDict: ('k,'id) t  -> ('k,'id) Bs_Cmp.t
val packDictData: dict:('k, 'id) Bs_Cmp.t -> data:('k, 'id) t0 -> ('k, 'id) t
    
val empty0: ('elt, 'id) t0

val ofArray0: 'k array -> cmp:('k,'id) Bs_Cmp.cmp ->  ('k, 'id) t0  

val isEmpty0: ('elt, 'id) t0 -> bool

val has0: 
  ('elt, 'id) t0 ->
  'elt ->
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  bool

val add0: 
  ('elt, 'id) t0 -> 
  'elt ->  
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0

val remove0: 
  ('elt, 'id) t0 -> 
  'elt ->  
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0
    
val mergeArray0:  
  ('elt, 'id) t0 -> 'elt array ->
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0

val removeArray0:  
  ('elt, 'id) t0 -> 'elt array ->
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0

val singleton0: 'elt -> ('elt, 'id) t0

val union0: 
    ('elt, 'id) t0 ->
    ('elt, 'id) t0 ->
    cmp: ('elt,'id) Bs_Cmp.cmp ->
    ('elt, 'id) t0
val inter0: 
  ('elt, 'id) t0 ->
  ('elt, 'id) t0 ->
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0
    
val diff0: 
  ('elt, 'id) t0 ->
  ('elt, 'id) t0 ->
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  ('elt, 'id) t0


val subset0: 
  ('elt, 'id) t0 ->
  ('elt, 'id) t0 ->
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  bool

val cmp0: 
  ('elt, 'id) t0 ->
  ('elt, 'id) t0  -> 
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  int

val eq0:
  ('elt, 'id) t0 ->
  ('elt, 'id) t0 ->
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  bool

val forEach0: ('elt, 'id) t0 -> ('elt -> unit [@bs]) ->  unit

val reduce0: ('elt, 'id) t0 -> 'a -> ('a -> 'elt ->  'a [@bs]) ->  'a
val every0: ('elt, 'id) t0 -> ('elt -> bool [@bs]) ->  bool
val some0: ('elt, 'id) t0 -> ('elt -> bool [@bs]) ->  bool

val filter0: ('elt, 'id) t0 -> ('elt -> bool [@bs]) ->  ('elt, 'id) t0

val partition0: ('elt, 'id) t0 -> ('elt -> bool [@bs]) -> ('elt, 'id) t0 * ('elt, 'id) t0

val size0: ('elt, 'id) t0 -> int

val toList0: ('elt, 'id) t0 -> 'elt list

val toArray0: ('elt, 'id) t0 -> 'elt array


val minimum0: ('elt, 'id) t0 -> 'elt option

val maximum0: ('elt, 'id) t0 -> 'elt option

val split0: 
  ('elt, 'id) t0 -> 'elt ->
   cmp: ('elt,'id) Bs_Cmp.cmp ->
  (('elt, 'id) t0  * ('elt, 'id) t0) * bool

val ofSortedArrayUnsafe0:
  'elt array -> ('elt,'id) t0
  

val get0: 
  ('elt, 'id) t0 -> 'elt ->
  cmp: ('elt,'id) Bs_Cmp.cmp -> 
  'elt option


val getNull0:
  ('elt, 'id) t0 -> 'elt ->
  cmp: ('elt,'id) Bs_Cmp.cmp ->
  'elt Js.null
