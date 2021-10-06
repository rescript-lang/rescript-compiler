(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

[@@@ocaml.text
"\n * Copyright (c) 2017-present, Facebook, Inc.\n *\n * This source code is licensed under the MIT license found in the\n * LICENSE file in the root directory of this source tree.\n "]

type codepoint =
  | Point of int
  | Malformed

type 'a folder = 'a -> int -> codepoint -> 'a

let needed_bytes c =
  if 0x00 <= c && c <= 0x7F then
    1
  else if 0xC2 <= c && c <= 0xDF then
    2
  else if 0xE0 <= c && c <= 0xEF then
    3
  else if 0xF0 <= c && c <= 0xF4 then
    4
  else
    0

let unsafe_char s i = Char.code (Bytes.unsafe_get s i)

let codepoint s i = function
  | 1 -> unsafe_char s i
  | 2 ->
    let b0 = unsafe_char s i in
    let b1 = unsafe_char s (i + 1) in
    ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F)
  | 3 ->
    let b0 = unsafe_char s i in
    let b1 = unsafe_char s (i + 1) in
    let b2 = unsafe_char s (i + 2) in
    ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
  | 4 ->
    let b0 = unsafe_char s i in
    let b1 = unsafe_char s (i + 1) in
    let b2 = unsafe_char s (i + 2) in
    let b3 = unsafe_char s (i + 3) in
    ((b0 land 0x07) lsl 18) lor ((b1 land 0x3F) lsl 12) lor ((b2 land 0x3F) lsl 6) lor (b3 land 0x3F)
  | _ -> assert false

let fold_wtf_8 ?(pos = 0) ?len f acc s =
  let rec loop acc f s i l =
    if i = l then
      acc
    else
      let need = needed_bytes (unsafe_char s i) in
      if need = 0 then
        (loop [@tailcall]) (f acc i Malformed) f s (i + 1) l
      else
        let rem = l - i in
        if rem < need then
          f acc i Malformed
        else
          (loop [@tailcall]) (f acc i (Point (codepoint s i need))) f s (i + need) l
  in
  let len =
    match len with
    | None -> String.length s - pos
    | Some l -> l
  in
  loop acc f (Bytes.unsafe_of_string s) pos len

let add_wtf_8 buf code =
  let w byte = Buffer.add_char buf (Char.unsafe_chr byte) [@@inline] in
  if code >= 0x10000 then (
    w (0xf0 lor (code lsr 18));
    w (0x80 lor ((code lsr 12) land 0x3F));
    w (0x80 lor ((code lsr 6) land 0x3F));
    w (0x80 lor (code land 0x3F))
  ) else if code >= 0x800 then (
    w (0xe0 lor (code lsr 12));
    w (0x80 lor ((code lsr 6) land 0x3F));
    w (0x80 lor (code land 0x3F))
  ) else if code >= 0x80 then (
    w (0xc0 lor (code lsr 6));
    w (0x80 lor (code land 0x3F))
  ) else
    w code
