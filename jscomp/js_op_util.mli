(* OCamlScript compiler
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



(** Some basic utilties around {!Js_op} module *)

val op_prec : Js_op.binop -> int * int * int

val op_str : Js_op.binop -> string

val op_int_prec : Js_op.int_op -> int * int * int

val op_int_str : Js_op.int_op -> string

val str_of_used_stats : Js_op.used_stats -> string

val update_used_stats : J.ident_info -> Js_op.used_stats -> unit

val same_vident : J.vident -> J.vident -> bool
