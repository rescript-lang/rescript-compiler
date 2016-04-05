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

val parse_digit : char -> int 

val caml_invalid_argument : string -> 'a

val repeat : int -> string -> string 

val caml_failwith : string -> 'a 

type fmt

val caml_format_float : string -> float -> string

val caml_format_int : string -> int -> string
val caml_nativeint_format : string -> int -> string     
val caml_int32_format : string -> int -> string
val caml_float_of_string : string -> float 
val caml_int64_format : string -> int64 -> string
val caml_int_of_string : string -> nativeint
val caml_int32_of_string : string -> nativeint
val caml_int64_of_string : string -> int64
val caml_nativeint_of_string : string -> nativeint
