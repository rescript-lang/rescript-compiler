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


type 'a t  = 'a list 

val length : 'a t -> int

val cons : 'a -> 'a t -> 'a t 

val isEmpty : 'a t -> bool 

val hd : 'a t -> 'a option

val tl : 'a t -> 'a t option

val nth : 'a t -> int -> 'a option

val revAppend : 'a t -> 'a t -> 'a t

val rev : 'a t -> 'a t

val mapRev : ('a -> 'b [@bs]) -> 'a t -> 'b t

val map : ('a -> 'b [@bs]) -> 'a t -> 'b t

val iter : ('a -> unit [@bs]) -> 'a t -> unit 

val iteri : (int -> 'a -> unit [@bs]) -> 'a t -> unit 

val foldLeft : ('a -> 'b -> 'a [@bs]) -> 'a -> 'b list -> 'a
(** Application order is left to right, tail recurisve *)

val foldRight : ('a -> 'b -> 'b [@bs]) -> 'a list -> 'b -> 'b
(** Application order is right to left
    tail-recursive. *)

val flatten : 'a t t  -> 'a t

val filter : ('a -> bool [@bs]) -> 'a t -> 'a t 

val filterMap : ('a -> 'b option [@bs]) -> 'a t -> 'b t

val countBy : ('a -> bool [@bs]) -> 'a list -> int 

val init : int -> (int -> 'a [@bs]) -> 'a t 

val toVector : 'a t -> 'a Js_vector.t 

val equal : ('a -> 'a -> bool [@bs]) -> 'a list -> 'a list -> bool