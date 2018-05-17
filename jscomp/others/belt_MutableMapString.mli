# 1 "mapm.cppo.mli"
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

# 26 "mapm.cppo.mli"
type key = string
# 32 "mapm.cppo.mli"
type 'a t


val make: unit -> 'a t
val clear: 'a t -> unit
val isEmpty: 'a t -> bool

val has:  'a t -> key -> bool

val cmpU:  'a t -> 'a t -> ('a -> 'a -> int [@bs]) -> int
val cmp:  'a t -> 'a t -> ('a -> 'a -> int) -> int  
(** [cmp m1 m2 cmp]
    First compare by size, if size is the same,
    compare by key, value pair
*)

val eqU: 'a t -> 'a t -> ('a -> 'a -> bool [@bs]) -> bool
val eq: 'a t -> 'a t -> ('a -> 'a -> bool ) -> bool  
(** [eq m1 m2 cmp] *)
  
val forEachU: 'a t -> (key -> 'a -> unit [@bs]) ->  unit
val forEach: 'a t -> (key -> 'a -> unit) ->  unit 
(** [forEach m f] applies [f] to all bindings in map [m].
   [f] receives the key as first argument, and the associated value
   as second argument.
   The application order of [f]  is in increasing order. *)

val reduceU:  'a t -> 'b -> ('b -> key -> 'a -> 'b [@bs]) -> 'b
val reduce:  'a t -> 'b -> ('b -> key -> 'a -> 'b ) -> 'b
(** [reduce m a f] computes [(f kN dN ... (f k1 d1 a)...)],
   where [k1 ... kN] are the keys of all bindings in [m]
   (in increasing order), and [d1 ... dN] are the associated data. *)

val everyU:  'a t -> (key -> 'a -> bool [@bs]) -> bool
val every:  'a t -> (key -> 'a -> bool) -> bool  
(** [every m p] checks if all the bindings of the map
    satisfy the predicate [p].
    The application order of [p] is unspecified. 
 *)

val someU:  'a t -> (key -> 'a -> bool [@bs]) -> bool
val some:  'a t -> (key -> 'a -> bool) -> bool  
(** [some m p] checks if at least one binding of the map
    satisfy the predicate [p].
    The application order of [p] is unspecified. 
 *)




val size: 'a t -> int
val toList: 'a t -> (key * 'a) list
(** In increasing order *)
val toArray: 'a t -> (key * 'a) array   


val fromArray: (key * 'a) array -> 'a t 
val keysToArray: 'a t -> key array 
val valuesToArray: 'a t -> 'a array
val minKey: _ t -> key option 
val minKeyUndefined: _ t -> key Js.undefined
val maxKey: _ t -> key option 
val maxKeyUndefined: _ t -> key Js.undefined
val minimum: 'a t -> (key * 'a) option
val minUndefined: 'a t -> (key * 'a) Js.undefined
val maximum: 'a t -> (key * 'a) option
val maxUndefined: 'a t -> (key * 'a) Js.undefined
val get: 'a t ->  key -> 'a option
val getUndefined: 'a t -> key -> 'a Js.undefined
val getWithDefault:  'a t -> key -> 'a  -> 'a
val getExn: 'a t -> key -> 'a
val checkInvariantInternal: _ t -> unit
(**
   {b raise} when invariant is not held
*)  
  


(****************************************************************************)

(*TODO: add functional [merge, partition, keep, split]*)

val remove: 'a t -> key -> unit  
(** [remove m x] do the in-place modification *)
val removeMany: 'a t -> key array -> unit
    
val set: 'a t -> key -> 'a -> unit  
(** [set m x y] do the in-place modification, return
    [m] for chaining. If [x] was already bound
   in [m], its previous binding disappears. *)

val updateU: 'a t -> key -> ('a option -> 'a option [@bs]) -> unit
val update: 'a t -> key -> ('a option -> 'a option) -> unit  


val mapU: 'a t -> ('a -> 'b [@bs]) ->  'b t
val map: 'a t -> ('a -> 'b) ->  'b t
(** [map m f] returns a map with same domain as [m], where the
   associated value [a] of all bindings of [m] has been
   replaced by the result of the application of [f] to [a].
   The bindings are passed to [f] in increasing order
   with respect to the ordering over the type of the keys. *)

val mapWithKeyU: 'a t -> (key -> 'a -> 'b [@bs]) -> 'b t
val mapWithKey: 'a t -> (key -> 'a -> 'b) -> 'b t    



