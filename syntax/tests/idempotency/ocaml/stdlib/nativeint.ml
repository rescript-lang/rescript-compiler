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

(* Module [Nativeint]: processor-native integers *)

external neg: nativeint -> nativeint = "%nativeint_neg"
external add: nativeint -> nativeint -> nativeint = "%nativeint_add"
external sub: nativeint -> nativeint -> nativeint = "%nativeint_sub"
external mul: nativeint -> nativeint -> nativeint = "%nativeint_mul"
external div: nativeint -> nativeint -> nativeint = "%nativeint_div"
external rem: nativeint -> nativeint -> nativeint = "%nativeint_mod"
external logand: nativeint -> nativeint -> nativeint = "%nativeint_and"
external logor: nativeint -> nativeint -> nativeint = "%nativeint_or"
external logxor: nativeint -> nativeint -> nativeint = "%nativeint_xor"
external shift_left: nativeint -> int -> nativeint = "%nativeint_lsl"
external shift_right: nativeint -> int -> nativeint = "%nativeint_asr"
external shift_right_logical: nativeint -> int -> nativeint = "%nativeint_lsr"
external of_int: int -> nativeint = "%nativeint_of_int"
external to_int: nativeint -> int = "%nativeint_to_int"
external of_float : float -> nativeint
  = "caml_nativeint_of_float" "caml_nativeint_of_float_unboxed"
  [@@unboxed] [@@noalloc]
external to_float : nativeint -> float
  = "caml_nativeint_to_float" "caml_nativeint_to_float_unboxed"
  [@@unboxed] [@@noalloc]
external of_int32: int32 -> nativeint = "%nativeint_of_int32"
external to_int32: nativeint -> int32 = "%nativeint_to_int32"

(* let zero = 0n *)
(* let one = 1n *)
(* let minus_one = -1n *)
(* let succ n = add n 1n *)
(* let pred n = sub n 1n *)
(* let abs n = if n >= 0n then n else neg n *)
(* let size = Sys.word_size *)
(* let min_int = shift_left 1n (size - 1) *)
(* let max_int = sub min_int 1n *)
(* let lognot n = logxor n (-1n) *)

external format : string -> nativeint -> string = "caml_nativeint_format"
let to_string n = format "%d" n

external of_string: string -> nativeint = "caml_nativeint_of_string"

let of_string_opt s =
  (* TODO: expose a non-raising primitive directly. *)
  try Some (of_string s)
  with Failure _ -> None

type t = nativeint

let compare (x: t) (y: t) = Pervasives.compare x y
let equal (x: t) (y: t) = compare x y = 0
