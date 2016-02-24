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

type env = 
  | Browser
  | NodeJS


val get_env : unit -> env

val set_env : env -> unit

val runtime_set : String_set.t
val stdlib_set : String_set.t

val prim : string 

val builtin_exceptions : string

val io : string

val oo : string

val sys : string

val lexer : string 
val parser : string
val obj_runtime : string

val array : string

val format : string

val string : string 

val float : string 

val curry : string 

val internalMod : string

val bigarray : string
val unix : string
