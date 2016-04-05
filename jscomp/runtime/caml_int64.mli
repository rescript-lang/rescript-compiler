(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)


type t (* = { lo : nativeint; hi : nativeint; } *)
val min_int : t
val max_int : t
val one : t
val zero : t
val not : t -> t
val of_int32 : nativeint -> t
val to_int32 : t -> nativeint

val add : t -> t -> t
val neg : t -> t
val sub : t -> t -> t
val lsl_ : t -> int -> t
val lsr_ : t -> int -> t
val asr_ : t -> int -> t
val is_zero : t -> bool
val mul : t -> t -> t

val swap : t -> t

type comparison = t -> t -> bool 
val ge : comparison
val eq : comparison
val neq : comparison
val lt : comparison
val gt : comparison
val le : comparison

val to_float : t -> float
val of_float : float -> t
val div : t -> t -> t
val mod_ : t -> t -> t

val div_mod :  t -> t -> t * t 
val compare : t -> t -> int
val to_hex : t -> string

val discard_sign : t -> t 
val float_of_bits : t -> float 
val bits_of_float : float -> t 
