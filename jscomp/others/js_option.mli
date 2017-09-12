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

type 'a t = 'a option

val some : 'a -> 'a option

val isSome : 'a option -> bool

val isSomeValue : ('a -> 'a -> bool [@bs]) -> 'a -> 'a option -> bool

val isNone : 'a option -> bool

val getExn : 'a option -> 'a

val equal : ('a -> 'b -> bool [@bs]) -> 'a option -> 'b option -> bool

val andThen : ('a -> 'b option [@bs]) -> 'a option -> 'b option

val map : ('a -> 'b [@bs]) -> 'a option -> 'b option


val getWithDefault :  'a -> 'a option -> 'a

val default : 'a -> 'a option -> 'a
[@@deprecated "Use getWithDefault instead since default has special meaning in ES module"]


val filter : ('a -> bool [@bs]) -> 'a option -> 'a option

val firstSome : 'a option -> 'a option -> 'a option
