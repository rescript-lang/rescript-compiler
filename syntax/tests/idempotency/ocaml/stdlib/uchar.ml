(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                           Daniel C. Buenzli                            *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external format_int : string -> int -> string = "caml_format_int"

let err_no_pred = "U+0000 has no predecessor"
let err_no_succ = "U+10FFFF has no successor"
let err_not_sv i = format_int "%X" i ^ " is not an Unicode scalar value"
let err_not_latin1 u = "U+" ^ format_int "%04X" u ^ " is not a latin1 character"

type t = int

let min = 0x0000
let max = 0x10FFFF
let lo_bound = 0xD7FF
let hi_bound = 0xE000

let bom = 0xFEFF
let rep = 0xFFFD

let succ u =
  if u = lo_bound then hi_bound else
  if u = max then invalid_arg err_no_succ else
  u + 1

let pred u =
  if u = hi_bound then lo_bound else
  if u = min then invalid_arg err_no_pred else
  u - 1

let is_valid i = (min <= i && i <= lo_bound) || (hi_bound <= i && i <= max)
let of_int i = if is_valid i then i else invalid_arg (err_not_sv i)
external unsafe_of_int : int -> t = "%identity"
external to_int : t -> int = "%identity"

let is_char u = u < 256
let of_char c = Char.code c
let to_char u =
  if u > 255 then invalid_arg (err_not_latin1 u) else
  Char.unsafe_chr u

let unsafe_to_char = Char.unsafe_chr

let equal : int -> int -> bool = ( = )
let compare : int -> int -> int = Pervasives.compare
let hash = to_int
