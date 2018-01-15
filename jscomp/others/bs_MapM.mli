
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

type ('k,'v,'id) t

val empty: ('k, 'id) Bs_Cmp.t -> ('k, 'a, 'id) t 

val ofArray:      
  ('k,'id) Bs_Cmp.t -> 
  ('k * 'a) array ->  
  ('k,'a,'id) t
    
val isEmpty: ('k, 'a, 'id) t -> bool
val mem: 
   ('k, 'a, 'id) t -> 'k  -> bool

val add: ('k, 'a, 'id) t -> 'k -> 'a ->  ('k, 'a, 'id) t
(** [add m x y ] do the in-place modification,
    returnning [m] for chaining. *)

val singleton: ('k,'id) Bs_Cmp.t ->
  'k -> 'a -> ('k, 'a, 'id) t

val remove:  ('k, 'a, 'id) t -> 'k -> ('k, 'a, 'id) t
(** [remove m x] do the in-place modification,
    returnning [m] for chaining. *)


(* val merge: *)
(*    ('k, 'a, 'id ) t -> ('k, 'b,'id) t -> ('k -> 'a option -> 'b option -> 'c option [@bs]) -> ('k, 'c,'id) t *)
(** [merge m1 m2 f] computes a map whose keys is a subset of keys of [m1]
    and of [m2]. The presence of each such binding, and the corresponding
    value, is determined with the function [f].
*)    

val cmp: 
    ('k, 'a, 'id) t -> 
    ('k, 'a, 'id) t ->
    ('a -> 'a -> int [@bs]) -> 
     int


val eq:  ('k, 'a, 'id) t -> ('k, 'a, 'id) t -> ('a -> 'a -> bool [@bs]) -> bool
(** [eq m1 m2 cmp] tests whether the maps [m1] and [m2] are
    equal, that is, contain equal keys and associate them with
    equal data.  [cmp] is the equality predicate used to compare
    the data associated with the keys. *)
    
val iter:  ('k, 'a, 'id) t -> ('k -> 'a -> unit [@bs]) -> unit
(** [iter m f] applies [f] to all bindings in map [m].
    [f] receives the 'k as first argument, and the associated value
    as second argument.  The bindings are passed to [f] in increasing
    order with respect to the ordering over the type of the keys. *)
    
val fold: ('k, 'a, 'id) t -> 'b ->  ('b -> 'k -> 'a -> 'b [@bs]) ->  'b
(** [fold m a f] computes [(f kN dN ... (f k1 d1 a)...)],
    where [k1 ... kN] are the keys of all bindings in [m]
    (in increasing order), and [d1 ... dN] are the associated data. *)

val forAll: ('k, 'a, 'id) t -> ('k -> 'a -> bool [@bs]) ->  bool
(** [forAll m p] checks if all the bindings of the map
    satisfy the predicate [p].
*)
    

val exists: ('k, 'a, 'id) t -> ('k -> 'a -> bool [@bs]) ->  bool
(** [exists m p] checks if at least one binding of the map
    satisfy the predicate [p].
*)

(* val filter: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t *)
(** [filter p m] returns the map with all the bindings in [m]
    that satisfy predicate [p].
*)
    
(* val partition: ('k -> 'a -> bool [@bs]) -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t * ('k, 'a, 'id) t *)
(** [partition p m] returns a pair of maps [(m1, m2)], where
    [m1] contains all the bindings of [s] that satisfy the
    predicate [p], and [m2] is the map with all the bindings of
    [s] that do not satisfy [p].
*)

val length: ('k, 'a, 'id) t -> int


val toList: ('k, 'a, 'id) t -> ('k * 'a) list
(** In increasing order*)
val toArray : ('k, 'a, 'id) t -> ('k * 'a) array

val minKVOpt: ('k, 'a,  _) t -> ('k * 'a) option
val minKVNull: ('k, 'a, _) t -> ('k * 'a) Js.null
val maxKVOpt: ('k, 'a, _) t -> ('k * 'a) option
val maxKVNull:('k, 'a, _) t -> ('k * 'a) Js.null

(* val split: 'k -> ('k, 'a, 'id) t -> ('k, 'a, 'id) t * 'a option * ('k, 'a, 'id) t *)
(** [split x m] returns a triple [(l, data, r)], where
      [l] is the map with all the bindings of [m] whose 'k
    is strictly less than [x];
      [r] is the map with all the bindings of [m] whose 'k
    is strictly greater than [x];
      [data] is [None] if [m] contains no binding for [x],
      or [Some v] if [m] binds [v] to [x].
*)

val findOpt:  ('k, 'a, 'id) t -> 'k -> 'a option
(** [find x m] returns the current binding of [x] in [m],
    or raises [Not_found] if no such binding exists. *)
val findNull: ('k, 'a, 'id) t -> 'k ->  'a Js.null

val findWithDefault:
    ('k, 'a, 'id) t -> 'k ->  'a -> 'a 
  
val map: ('k, 'a, 'id) t -> ('a -> 'b [@bs]) ->  ('k ,'b,'id ) t
(** [map m f] returns a map with same domain as [m], where the
    associated value [a] of all bindings of [m] has been
    replaced by the result of the application of [f] to [a].
    The bindings are passed to [f] in increasing order
    with respect to the ordering over the type of the keys. *)

val mapi: ('k, 'a, 'id) t -> ('k -> 'a -> 'b [@bs]) -> ('k, 'b, 'id) t
    

