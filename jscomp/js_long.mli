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

type int64_call = J.expression list -> J.expression  

val make_const : lo:Int32.t -> hi:Int32.t -> J.expression

val of_const : Int64.t -> J.expression

val to_int32 : int64_call

val of_int32 : int64_call
val comp : Lambda.comparison -> int64_call
val neg : int64_call
val add : int64_call
val sub : int64_call
val mul : int64_call
val div : int64_call
val xor : int64_call
val mod_ : int64_call
val lsl_ : int64_call
val lsr_ : int64_call
val asr_ : int64_call
val and_ : int64_call
val or_ : int64_call
val swap : int64_call
val to_float : int64_call
val of_float : int64_call
