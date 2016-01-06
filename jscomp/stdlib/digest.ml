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

(* Message digest (MD5) *)

type t = string

let compare = String.compare

external unsafe_string: string -> int -> int -> t = "caml_md5_string"
external channel: in_channel -> int -> t = "caml_md5_chan"

let string str =
  unsafe_string str 0 (String.length str)

let bytes b = string (Bytes.unsafe_to_string b)

let substring str ofs len =
  if ofs < 0 || len < 0 || ofs > String.length str - len
  then invalid_arg "Digest.substring"
  else unsafe_string str ofs len

let subbytes b ofs len = substring (Bytes.unsafe_to_string b) ofs len

let file filename =
  let ic = open_in_bin filename in
  match channel ic (-1) with
    | d -> close_in ic; d
    | exception e -> close_in ic; raise e

let output chan digest =
  output_string chan digest

let input chan = really_input_string chan 16

let char_hex n =
  Char.unsafe_chr (n + if n < 10 then Char.code '0' else (Char.code 'a' - 10))

let to_hex d =
  let result = Bytes.create 32 in
  for i = 0 to 15 do
    let x = Char.code d.[i] in
    Bytes.unsafe_set result (i*2) (char_hex (x lsr 4));
    Bytes.unsafe_set result (i*2+1) (char_hex (x land 0x0f));
  done;
  Bytes.unsafe_to_string result

let from_hex s =
  if String.length s <> 32 then raise (Invalid_argument "Digest.from_hex");
  let digit c =
    match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | _ -> raise (Invalid_argument "Digest.from_hex")
  in
  let byte i = digit s.[i] lsl 4 + digit s.[i+1] in
  let result = Bytes.create 16 in
  for i = 0 to 15 do
    Bytes.set result i (Char.chr (byte (2 * i)));
  done;
  Bytes.unsafe_to_string result
