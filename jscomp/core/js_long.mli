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






type int64_call = J.expression list -> J.expression  

val make_const : lo:Int32.t -> hi:Int32.t -> J.expression

val of_const : int64 -> J.expression

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
val min : int64_call
val max : int64_call
val discard_sign : int64_call
val div_mod : int64_call
val to_hex : int64_call  
val to_float : int64_call
val of_float : int64_call
val compare : int64_call
val of_string : int64_call
val float_of_bits : int64_call
val bits_of_float : int64_call  
val get64 : int64_call
