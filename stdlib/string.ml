(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Damien Doligez, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* String operations, based on byte sequence operations *)

external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external set : bytes -> int -> char -> unit = "%bytes_safe_set"
external create : int -> bytes = "caml_create_string"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external unsafe_blit : string -> int ->  bytes -> int -> int -> unit
                     = "caml_blit_string" "noalloc"
external unsafe_fill : bytes -> int -> int -> char -> unit
                     = "caml_fill_string" "noalloc"

module B = Bytes

let bts = B.unsafe_to_string
let bos = B.unsafe_of_string

let make n c =
  B.make n c |> bts
let init n f =
  B.init n f |> bts
let copy s =
  B.copy (bos s) |> bts
let sub s ofs len =
  B.sub (bos s) ofs len |> bts
let fill =
  B.fill
let blit =
  B.blit_string

let concat sep l =
  match l with
  | [] -> ""
  | hd :: tl ->
      let num = ref 0 and len = ref 0 in
      List.iter (fun s -> incr num; len := !len + length s) l;
      let r = B.create (!len + length sep * (!num - 1)) in
      unsafe_blit hd 0 r 0 (length hd);
      let pos = ref(length hd) in
      List.iter
        (fun s ->
          unsafe_blit sep 0 r !pos (length sep);
          pos := !pos + length sep;
          unsafe_blit s 0 r !pos (length s);
          pos := !pos + length s)
        tl;
      Bytes.unsafe_to_string r

let iter f s =
  B.iter f (bos s)
let iteri f s =
  B.iteri f (bos s)
let map f s =
  B.map f (bos s) |> bts
let mapi f s =
  B.mapi f (bos s) |> bts

(* Beware: we cannot use B.trim or B.escape because they always make a
   copy, but String.mli spells out some cases where we are not allowed
   to make a copy. *)



let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let trim s =
  if s = "" then s
  else if is_space (unsafe_get s 0) || is_space (unsafe_get s (length s - 1))
    then bts (B.trim (bos s))
  else s

let escaped s =
  let rec needs_escape i =
    if i >= length s then false else
      match unsafe_get s i with
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> true
      | ' ' .. '~' -> needs_escape (i + 1)
      | _ -> true
  in
  if needs_escape 0 then
    bts (B.escaped (bos s))
  else
    s

let index s c =
  B.index (bos s) c
let rindex s c =
  B.rindex (bos s) c
let index_from s i c=
  B.index_from (bos s) i c
let rindex_from s i c =
  B.rindex_from (bos s) i c
let contains s c =
  B.contains (bos s) c
let contains_from s i c =
  B.contains_from (bos s) i c
let rcontains_from s i c =
  B.rcontains_from (bos s) i c
let uppercase s =
  B.uppercase (bos s) |> bts
let lowercase s =
  B.lowercase (bos s) |> bts
let capitalize s =
  B.capitalize (bos s) |> bts
let uncapitalize s =
  B.uncapitalize (bos s) |> bts

type t = string

let compare (x: t) (y: t) = Pervasives.compare x y
