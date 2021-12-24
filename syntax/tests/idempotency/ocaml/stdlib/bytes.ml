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

(* Byte sequence operations *)

(* WARNING: Some functions in this file are duplicated in string.ml for
   efficiency reasons. When you modify the one in this file you need to
   modify its duplicate in string.ml.
   These functions have a "duplicated" comment above their definition.
*)

external length : bytes -> int = "%bytes_length"
external string_length : string -> int = "%string_length"
external get : bytes -> int -> char = "%bytes_safe_get"
external set : bytes -> int -> char -> unit = "%bytes_safe_set"
external create : int -> bytes = "caml_create_bytes"
external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external unsafe_fill : bytes -> int -> int -> char -> unit
                     = "caml_fill_bytes" [@@noalloc]
external unsafe_to_string : bytes -> string = "%bytes_to_string"
external unsafe_of_string : string -> bytes = "%bytes_of_string"

external unsafe_blit : bytes -> int -> bytes -> int -> int -> unit
                     = "caml_blit_bytes" [@@noalloc]
external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
                     = "caml_blit_string" [@@noalloc]

let make n c =
  let s = create n in
  unsafe_fill s 0 n c;
  s

let init n f =
  let s = create n in
  for i = 0 to n - 1 do
    unsafe_set s i (f i)
  done;
  s

let empty = create 0

let copy s =
  let len = length s in
  let r = create len in
  unsafe_blit s 0 r 0 len;
  r

let to_string b = unsafe_to_string (copy b)
let of_string s = copy (unsafe_of_string s)

let sub s ofs len =
  if ofs < 0 || len < 0 || ofs > length s - len
  then invalid_arg "String.sub / Bytes.sub"
  else begin
    let r = create len in
    unsafe_blit s ofs r 0 len;
    r
  end

let sub_string b ofs len = unsafe_to_string (sub b ofs len)

(* addition with an overflow check *)
let (++) a b =
  let c = a + b in
  match a < 0, b < 0, c < 0 with
  | true , true , false
  | false, false, true  -> invalid_arg "Bytes.extend" (* overflow *)
  | _ -> c

let extend s left right =
  let len = length s ++ left ++ right in
  let r = create len in
  let (srcoff, dstoff) = if left < 0 then -left, 0 else 0, left in
  let cpylen = min (length s - srcoff) (len - dstoff) in
  if cpylen > 0 then unsafe_blit s srcoff r dstoff cpylen;
  r

let fill s ofs len c =
  if ofs < 0 || len < 0 || ofs > length s - len
  then invalid_arg "String.fill / Bytes.fill"
  else unsafe_fill s ofs len c

let blit s1 ofs1 s2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > length s1 - len
             || ofs2 < 0 || ofs2 > length s2 - len
  then invalid_arg "Bytes.blit"
  else unsafe_blit s1 ofs1 s2 ofs2 len

let blit_string s1 ofs1 s2 ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > string_length s1 - len
             || ofs2 < 0 || ofs2 > length s2 - len
  then invalid_arg "String.blit / Bytes.blit_string"
  else unsafe_blit_string s1 ofs1 s2 ofs2 len

(* duplicated in string.ml *)
let iter f a =
  for i = 0 to length a - 1 do f(unsafe_get a i) done

(* duplicated in string.ml *)
let iteri f a =
  for i = 0 to length a - 1 do f i (unsafe_get a i) done

let ensure_ge (x:int) y = if x >= y then x else invalid_arg "Bytes.concat"

let rec sum_lengths acc seplen = function
  | [] -> acc
  | hd :: [] -> length hd + acc
  | hd :: tl -> sum_lengths (ensure_ge (length hd + seplen + acc) acc) seplen tl

let rec unsafe_blits dst pos sep seplen = function
    [] -> dst
  | hd :: [] ->
    unsafe_blit hd 0 dst pos (length hd); dst
  | hd :: tl ->
    unsafe_blit hd 0 dst pos (length hd);
    unsafe_blit sep 0 dst (pos + length hd) seplen;
    unsafe_blits dst (pos + length hd + seplen) sep seplen tl

let concat sep = function
    [] -> empty
  | l -> let seplen = length sep in
          unsafe_blits
            (create (sum_lengths 0 seplen l))
            0 sep seplen l

let cat s1 s2 =
  let l1 = length s1 in
  let l2 = length s2 in
  let r = create (l1 + l2) in
  unsafe_blit s1 0 r 0 l1;
  unsafe_blit s2 0 r l1 l2;
  r


external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let trim s =
  let len = length s in
  let i = ref 0 in
  while !i < len && is_space (unsafe_get s !i) do
    incr i
  done;
  let j = ref (len - 1) in
  while !j >= !i && is_space (unsafe_get s !j) do
    decr j
  done;
  if !j >= !i then
    sub s !i (!j - !i + 1)
  else
    empty

let escaped s =
  let n = ref 0 in
  for i = 0 to length s - 1 do
    n := !n +
      (match unsafe_get s i with
       | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | ' ' .. '~' -> 1
       | _ -> 4)
  done;
  if !n = length s then copy s else begin
    let s' = create !n in
    n := 0;
    for i = 0 to length s - 1 do
      begin match unsafe_get s i with
      | ('\"' | '\\') as c ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
      | '\n' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
      | '\t' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
      | '\r' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'r'
      | '\b' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'b'
      | (' ' .. '~') as c -> unsafe_set s' !n c
      | c ->
          let a = char_code c in
          unsafe_set s' !n '\\';
          incr n;
          unsafe_set s' !n (char_chr (48 + a / 100));
          incr n;
          unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
          incr n;
          unsafe_set s' !n (char_chr (48 + a mod 10));
      end;
      incr n
    done;
    s'
  end

let map f s =
  let l = length s in
  if l = 0 then s else begin
    let r = create l in
    for i = 0 to l - 1 do unsafe_set r i (f (unsafe_get s i)) done;
    r
  end

let mapi f s =
  let l = length s in
  if l = 0 then s else begin
    let r = create l in
    for i = 0 to l - 1 do unsafe_set r i (f i (unsafe_get s i)) done;
    r
  end

let uppercase_ascii s = map Char.uppercase_ascii s
let lowercase_ascii s = map Char.lowercase_ascii s

let apply1 f s =
  if length s = 0 then s else begin
    let r = copy s in
    unsafe_set r 0 (f(unsafe_get s 0));
    r
  end

let capitalize_ascii s = apply1 Char.uppercase_ascii s
let uncapitalize_ascii s = apply1 Char.lowercase_ascii s

(* duplicated in string.ml *)
let rec index_rec s lim i c =
  if i >= lim then raise Not_found else
  if unsafe_get s i = c then i else index_rec s lim (i + 1) c

(* duplicated in string.ml *)
let index s c = index_rec s (length s) 0 c

(* duplicated in string.ml *)
let rec index_rec_opt s lim i c =
  if i >= lim then None else
  if unsafe_get s i = c then Some i else index_rec_opt s lim (i + 1) c

(* duplicated in string.ml *)
let index_opt s c = index_rec_opt s (length s) 0 c

(* duplicated in string.ml *)
let index_from s i c =
  let l = length s in
  if i < 0 || i > l then invalid_arg "String.index_from / Bytes.index_from" else
  index_rec s l i c

(* duplicated in string.ml *)
let index_from_opt s i c =
  let l = length s in
  if i < 0 || i > l then invalid_arg "String.index_from_opt / Bytes.index_from_opt" else
  index_rec_opt s l i c

(* duplicated in string.ml *)
let rec rindex_rec s i c =
  if i < 0 then raise Not_found else
  if unsafe_get s i = c then i else rindex_rec s (i - 1) c

(* duplicated in string.ml *)
let rindex s c = rindex_rec s (length s - 1) c

(* duplicated in string.ml *)
let rindex_from s i c =
  if i < -1 || i >= length s then
    invalid_arg "String.rindex_from / Bytes.rindex_from"
  else
    rindex_rec s i c

(* duplicated in string.ml *)
let rec rindex_rec_opt s i c =
  if i < 0 then None else
  if unsafe_get s i = c then Some i else rindex_rec_opt s (i - 1) c

(* duplicated in string.ml *)
let rindex_opt s c = rindex_rec_opt s (length s - 1) c

(* duplicated in string.ml *)
let rindex_from_opt s i c =
  if i < -1 || i >= length s then
    invalid_arg "String.rindex_from_opt / Bytes.rindex_from_opt"
  else
    rindex_rec_opt s i c


(* duplicated in string.ml *)
let contains_from s i c =
  let l = length s in
  if i < 0 || i > l then
    invalid_arg "String.contains_from / Bytes.contains_from"
  else
    try ignore (index_rec s l i c); true with Not_found -> false


(* duplicated in string.ml *)
let contains s c = contains_from s 0 c

(* duplicated in string.ml *)
let rcontains_from s i c =
  if i < 0 || i >= length s then
    invalid_arg "String.rcontains_from / Bytes.rcontains_from"
  else
    try ignore (rindex_rec s i c); true with Not_found -> false


type t = bytes

let compare (x: t) (y: t) = Pervasives.compare x y
external equal : t -> t -> bool = "caml_bytes_equal"

(* Deprecated functions implemented via other deprecated functions *)
[@@@ocaml.warning "-3"]
let uppercase s = map Char.uppercase s
let lowercase s = map Char.lowercase s

let capitalize s = apply1 Char.uppercase s
let uncapitalize s = apply1 Char.lowercase s
