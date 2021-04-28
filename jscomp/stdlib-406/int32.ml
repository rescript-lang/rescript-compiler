(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
type t = int
(* Module [t]: 32-bit integers *)

external neg : t -> t = "%int32_neg"
external add : t -> t -> t = "%int32_add"
external sub : t -> t -> t = "%int32_sub"
external mul : t -> t -> t = "%int32_mul"
external div : t -> t -> t = "%int32_div"
external rem : t -> t -> t = "%int32_mod"
external logand : t -> t -> t = "%int32_and"
external logor : t -> t -> t = "%int32_or"
external logxor : t -> t -> t = "%int32_xor"
external shift_left : t -> int -> t = "%int32_lsl"
external shift_right : t -> int -> t = "%int32_asr"
external shift_right_logical : t -> int -> t = "%int32_lsr"
external of_int : int -> t = "%int32_of_int"
external to_int : t -> int = "%int32_to_int"
external of_float : float -> t
  = "caml_int_of_float" 
external to_float : t -> float
  = "caml_int_to_float" 
external bits_of_float : float -> t
  = "caml_int_bits_of_float" "caml_int_bits_of_float_unboxed"
  [@@noalloc]
external float_of_bits : t -> float
  = "caml_int_float_of_bits" "caml_int_float_of_bits_unboxed"
  [@@noalloc]

let zero = 0l
let one = 1l
let minus_one = -1l
let succ n = add n 1l
let pred n = sub n 1l
let abs n = if n >= 0l then n else neg n
let min_int = 0x80000000l
let max_int = 0x7FFFFFFFl
let lognot n = logxor n (-1l)

external format : string -> t -> string = "caml_format_int"
let to_string n = format "%d" n

external of_string : string -> t = "caml_int_of_string"

let of_string_opt s =
  (* TODO: expose a non-raising primitive directly. *)
  try Some (of_string s)
  with Failure _ -> None



let compare (x: t) (y: t) = Pervasives.compare x y
let equal (x: t) (y: t) = compare x y = 0
