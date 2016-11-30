(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

type key = Ident.t
val compare_key : key -> key -> int 

(*************************************************)
type + 'a t 
val empty: 'a t
val is_empty: 'a t -> bool
val iter: (key -> 'a ->  unit) -> 'a t -> unit
val fold: (key -> 'a -> 'b ->  'b) -> 'a t -> 'b -> 'b
val for_all: (key -> 'a  -> bool) -> 'a t -> bool
val exists: (key -> 'a -> bool) -> 'a t -> bool
val singleton: key -> 'a  -> 'a t
val cardinal: 'a t -> int
(* val elements: 'a t -> (key * 'a) list *)
val choose: 'a t -> key * 'a 
(* val partition: (key -> bool) -> 'a t -> 'a t * 'a t *)

val mem: key -> 'a t -> bool
val add: key -> 'a -> 'a t -> 'a t
val find : key -> 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (key -> 'b -> 'c) -> 'b t -> 'c t
val merge : 
    (key -> 'b option -> 'c option -> 'd option)
    -> 'b t
    -> 'c t 
    -> 'd t 
(*************************************************)

val of_list  : (key * 'a) list -> 'a t

val keys : 'a t -> key list

val add_if_not_exist : key -> 'a -> 'a t -> 'a t

val merge_disjoint : 'a t -> 'a t -> 'a t
