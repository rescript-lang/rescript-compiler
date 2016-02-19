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



(** Utilities to wrap [string] and [bytes] compilation, 

   this is isolated, so that we can swap different representation in the future.
   [string] is Immutable, so there is not [set_string] method
*)

val ref_string : J.expression -> J.expression -> J.expression

val ref_byte : J.expression -> J.expression -> J.expression

val set_byte : J.expression -> J.expression -> J.expression -> J.expression 

val caml_char_of_int : ?comment:string -> J.expression -> J.expression

val caml_char_to_int : ?comment:string -> J.expression -> J.expression

val const_char : char -> J.expression

val bytes_to_string : J.expression -> J.expression

val bytes_of_string : J.expression -> J.expression
