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





(** *)

type t 
val min_int : t (* [@@dead "min_int"] *)
val max_int : t (* [@@dead "max_int"] *)
val one : t (* [@@dead "one"] *)
val zero : t (* [@@dead "zero"] *)
val not : t -> t (* [@@dead "not"] *)
val of_int32 : nativeint -> t (* [@@dead "of_int32"] *)
val to_int32 : t -> nativeint (* [@@dead "to_int32"] *)

val add : t -> t -> t (* [@@dead "add"] *)
val neg : t -> t (* [@@dead "neg"] *)
val sub : t -> t -> t (* [@@dead "sub"] *)
val lsl_ : t -> int -> t (* [@@dead "lsl_"] *)
val lsr_ : t -> int -> t (* [@@dead "lsr_"] *)
val asr_ : t -> int -> t (* [@@dead "asr_"] *)
val is_zero : t -> bool (* [@@dead "is_zero"] *)
val mul : t -> t -> t (* [@@dead "mul"] *)
val xor : t -> t -> t  (* [@@dead "xor"] *)
val or_ : t -> t -> t  (* [@@dead "or_"] *)
val and_ : t -> t -> t  (* [@@dead "and_"] *)
val swap : t -> t (* [@@dead "swap"] *)

type comparison = t -> t -> bool 
val ge : comparison (* [@@dead "ge"] *)
val eq : comparison (* [@@dead "eq"] *)
val neq : comparison (* [@@dead "neq"] *)
val lt : comparison (* [@@dead "lt"] *)
val gt : comparison (* [@@dead "gt"] *)
val le : comparison (* [@@dead "le"] *)


val equal_null : t -> t Js.null -> bool  (* [@@dead "equal_null"] *)
val equal_undefined : t -> t Js.undefined -> bool  (* [@@dead "equal_undefined"] *)
val equal_nullable : t -> t Js.nullable -> bool  (* [@@dead "equal_nullable"] *)

val min : t -> t -> t  (* [@@dead "min"] *)
val max : t -> t -> t (* [@@dead "max"] *)

val to_float : t -> float (* [@@dead "to_float"] *)
val of_float : float -> t (* [@@dead "of_float"] *)
val div : t -> t -> t (* [@@dead "div"] *)
val mod_ : t -> t -> t (* [@@dead "mod_"] *)


val compare : t -> t -> int (* [@@dead "compare"] *)



val float_of_bits : t -> float  (* [@@dead "float_of_bits"] *)

(** [bits_of_float fl] it is undefined behaivor when [f] is NaN*)
val bits_of_float : float -> t  (* [@@dead "bits_of_float"] *)

(* val get64 : string -> int -> t *)


external unsafe_to_int64 : t -> int64 = "%identity"           
external unsafe_of_int64 : int64 -> t = "%identity"
val div_mod :  int64 -> int64 -> int64 * int64
val to_hex : int64 -> string
val discard_sign : int64 -> int64 