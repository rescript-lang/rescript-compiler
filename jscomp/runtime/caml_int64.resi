/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

type t
let mk: (~lo: int, ~hi: int) => t
let succ: t => t
let min_int: t
let max_int: t
let one: t
let zero: t
let neg_one: t
/* val not : t -> t */
let of_int32: int => t
let to_int32: t => int

let add: (t, t) => t
let neg: t => t
let sub: (t, t) => t
let lsl_: (t, int) => t
let lsr_: (t, int) => t
let asr_: (t, int) => t
let is_zero: t => bool
let mul: (t, t) => t
let xor: (t, t) => t
let or_: (t, t) => t
let and_: (t, t) => t

let equal: (t, t) => bool
let equal_null: (t, Js.null<t>) => bool
let equal_undefined: (t, Js.undefined<t>) => bool
let equal_nullable: (t, Js.nullable<t>) => bool

let to_float: t => float
let of_float: float => t
let div: (t, t) => t
let mod_: (t, t) => t

let compare: (t, t) => int

let float_of_bits: t => float

/** [bits_of_float fl] it is undefined behaivor when [f] is NaN*/
let bits_of_float: float => t

/* val get64 : string -> int -> t */

external unsafe_to_int64: t => int64 = "%identity"
external unsafe_of_int64: int64 => t = "%identity"
let div_mod: (int64, int64) => (int64, int64)
let to_hex: int64 => string
let discard_sign: int64 => int64
let to_string: int64 => string
