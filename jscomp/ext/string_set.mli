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




type elt = String.t
val compare_elt : elt -> elt -> int 
(***********************************************************************)             
type t
val empty: t
val is_empty: t -> bool
val iter:  t -> (elt -> unit) -> unit
val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
val for_all: t -> (elt -> bool) -> bool
val exists:  t -> (elt -> bool) -> bool
val singleton: elt -> t
val cardinal: t -> int
val elements: t -> elt list
val remove : t -> elt ->  t
val min_elt: t -> elt
val max_elt: t -> elt
val choose: t -> elt
val of_sorted_list : elt list -> t 
val of_sorted_array : elt array -> t
val partition: (elt -> bool) -> t -> t * t

val mem: elt -> t -> bool
val add: t -> elt ->  t

val of_list : elt list -> t
val find : elt -> t -> elt 
(***********************************************************************) 
