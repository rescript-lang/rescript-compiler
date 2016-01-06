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

external code: char -> int = "%char_to_int"
external unsafe_chr: int -> char = "%char_of_int"

let chr n =
  if n < 0 || n > 255 then invalid_arg "Char.chr" else unsafe_chr n

external is_printable: char -> bool = "caml_is_printable"

external string_of_char_array : char array -> string = "caml_string_of_char_array"

let escaped = function
  | '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | '\b' -> "\\b"
  | c ->
    if is_printable c then begin
      string_of_char_array [|c|]
    end else begin
      let n = code c in
      string_of_char_array [|'\\'; 
                           (unsafe_chr (48 + n / 100));
                           (unsafe_chr (48 + (n / 10) mod 10));
                           (unsafe_chr (48 + n mod 10));
                           |]
    end

let lowercase c =
  if (c >= 'A' && c <= 'Z')
  || (c >= '\192' && c <= '\214')
  || (c >= '\216' && c <= '\222')
  then unsafe_chr(code c + 32)
  else c

let uppercase c =
  if (c >= 'a' && c <= 'z')
  || (c >= '\224' && c <= '\246')
  || (c >= '\248' && c <= '\254')
  then unsafe_chr(code c - 32)
  else c

type t = char

let compare c1 c2 = code c1 - code c2
