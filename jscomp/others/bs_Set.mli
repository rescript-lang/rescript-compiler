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

(** specalized when key type is [int], more efficient
    than the gerneic type
*)
module Int = Bs_SetInt

(** specalized when key type is [string], more efficient
    than the gerneic type *)  
module String = Bs_SetString

(** seprate function from data, a more verbsoe but slightly
    more efficient
*)  
module Dict = Bs_SetDict

type ('k,'id) t

type ('key, 'id) dict = ('key, 'id) Bs_Dict.comparable
    
val make: dict:('elt, 'id) dict -> ('elt, 'id) t

val ofArray:  'k array -> dict:('k, 'id) dict ->  ('k, 'id) t

val ofSortedArrayUnsafe: 'elt array -> dict:('elt, 'id) dict -> ('elt,'id) t
    
val isEmpty: _ t -> bool
val has: ('elt, 'id) t -> 'elt ->  bool


val add:   
  ('elt, 'id) t -> 'elt -> ('elt, 'id) t
(** [add s x] If [x] was already in [s], [s] is returned unchanged. *)
val mergeMany: ('elt, 'id) t -> 'elt array -> ('elt, 'id) t 

val remove: ('elt, 'id) t -> 'elt -> ('elt, 'id) t
(** [remove m x] If [x] was not in [m], [m] is returned reference unchanged. *)

val removeMany:
  ('elt, 'id) t -> 'elt array -> ('elt, 'id) t 


val union: ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t
val intersect: ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t 
val diff: ('elt, 'id) t -> ('elt, 'id) t -> ('elt, 'id) t
val subset: ('elt, 'id) t -> ('elt, 'id) t -> bool     

val cmp: ('elt, 'id) t -> ('elt, 'id) t -> int
(** Total ordering between sets. Can be used as the ordering function
    for doing sets of sets. *)
val eq: ('elt, 'id) t -> ('elt, 'id) t -> bool

val forEachU: ('elt, 'id) t -> ('elt -> unit [@bs]) ->  unit
val forEach: ('elt, 'id) t -> ('elt -> unit ) ->  unit  
(** [forEach s f] applies [f] in turn to all elements of [s].
    In increasing order *)

val reduceU: ('elt, 'id) t -> 'a  -> ('a -> 'elt -> 'a [@bs]) ->  'a
val reduce: ('elt, 'id) t -> 'a  -> ('a -> 'elt -> 'a ) ->  'a  
(** In increasing order. *)

val everyU: ('elt, 'id) t -> ('elt -> bool [@bs]) -> bool
val every: ('elt, 'id) t -> ('elt -> bool ) -> bool  
(** [every p s] checks if all elements of the set
    satisfy the predicate [p]. Order unspecified *)

val someU: ('elt, 'id) t ->  ('elt -> bool [@bs]) -> bool
val some: ('elt, 'id) t ->  ('elt -> bool ) -> bool  
(** [some p s] checks if at least one element of
    the set satisfies the predicate [p]. *)

val keepU: ('elt, 'id) t ->  ('elt -> bool [@bs]) -> ('elt, 'id) t
val keep: ('elt, 'id) t ->  ('elt -> bool ) -> ('elt, 'id) t    
(** [keep m p] returns the set of all elements in [s]
    that satisfy predicate [p]. *)
    
val partitionU: ('elt, 'id) t -> ('elt -> bool [@bs]) ->  ('elt, 'id) t * ('elt, 'id) t
val partition: ('elt, 'id) t -> ('elt -> bool) ->  ('elt, 'id) t * ('elt, 'id) t
(** [partition m p] returns a pair of sets [(s1, s2)], where
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

val split: ('elt, 'id) t -> 'elt -> (('elt, 'id) t  * ('elt, 'id) t) * bool
                                    
val checkInvariantInternal: _ t -> bool

(****************************************************************************)
(** Below are operations only when better performance needed,
    it is still safe API but more verbose.
    More API will be exposed by needs
*)

val getData: ('k,'id) t  -> ('k,'id) Bs_SetDict.t
val getDict: ('k,'id) t  -> ('k,'id) dict
val packDictData: dict:('k, 'id) dict -> data:('k, 'id) Bs_SetDict.t -> ('k, 'id) t
    
