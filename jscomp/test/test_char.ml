(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Character operations *)

external code : char -> int = "%identity"
external unsafe_chr : int -> char = "%identity"

let f x = x + 1
let chr n = if n < 0 || n > 255 then invalid_arg "Char.chr" else unsafe_chr n

external is_printable : char -> bool = "caml_is_printable"
external string_create : int -> string = "caml_create_string"
external string_unsafe_get : string -> int -> char = "%string_unsafe_get"

(** this primitive -- should be removed .. *)

let lowercase c =
  if
    (c >= 'A' && c <= 'Z')
    || (c >= '\192' && c <= '\214')
    || (c >= '\216' && c <= '\222')
  then unsafe_chr (code c + 32)
  else c

let uppercase c =
  if
    (c >= 'a' && c <= 'z')
    || (c >= '\224' && c <= '\246')
    || (c >= '\248' && c <= '\254')
  then unsafe_chr (code c - 32)
  else c

type t = char

let compare c1 c2 = code c1 - code c2
