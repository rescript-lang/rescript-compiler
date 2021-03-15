(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt    *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Extensible buffers *)

type t =
 {mutable buffer : bytes;
  mutable position : int;
  mutable length : int;
  initial_buffer : bytes}

let create n =
 let n = if n < 1 then 1 else n in
#if BS
#else 
 let n = if n > Sys.max_string_length then Sys.max_string_length else n in
#end 
 let s = Bytes.create n in
 {buffer = s; position = 0; length = n; initial_buffer = s}

let contents b = Bytes.sub_string b.buffer 0 b.position
let to_bytes b = Bytes.sub b.buffer 0 b.position

let sub b ofs len =
  if ofs < 0 || len < 0 || ofs > b.position - len
  then invalid_arg "Buffer.sub"
  else Bytes.sub_string b.buffer ofs len


let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || srcoff > src.position - len
             || dstoff < 0 || dstoff > (Bytes.length dst) - len
  then invalid_arg "Buffer.blit"
  else
    Bytes.unsafe_blit src.buffer srcoff dst dstoff len


let nth b ofs =
  if ofs < 0 || ofs >= b.position then
   invalid_arg "Buffer.nth"
  else Bytes.unsafe_get b.buffer ofs


let length b = b.position

let clear b = b.position <- 0

let reset b =
  b.position <- 0; b.buffer <- b.initial_buffer;
  b.length <- Bytes.length b.buffer

let resize b more =
  let len = b.length in
  let new_len = ref len in
  while b.position + more > !new_len do new_len := 2 * !new_len done;
#if BS
#else   
  if !new_len > Sys.max_string_length then begin
    if b.position + more <= Sys.max_string_length
    then new_len := Sys.max_string_length
    else failwith "Buffer.add: cannot grow buffer"
  end;
#end  
  let new_buffer = Bytes.create !new_len in
  (* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
     this tricky function that is slow anyway. *)
  Bytes.blit b.buffer 0 new_buffer 0 b.position;
  b.buffer <- new_buffer;
  b.length <- !new_len

let add_char b c =
  let pos = b.position in
  if pos >= b.length then resize b 1;
  Bytes.unsafe_set b.buffer pos c;
  b.position <- pos + 1

 let add_utf_8_uchar b u = match Uchar.to_int u with
 | u when u < 0 -> assert false
 | u when u <= 0x007F ->
     add_char b (Char.unsafe_chr u)
 | u when u <= 0x07FF ->
     let pos = b.position in
     if pos + 2 > b.length then resize b 2;
     Bytes.unsafe_set b.buffer (pos    )
       (Char.unsafe_chr (0xC0 lor (u lsr 6)));
     Bytes.unsafe_set b.buffer (pos + 1)
       (Char.unsafe_chr (0x80 lor (u land 0x3F)));
     b.position <- pos + 2
 | u when u <= 0xFFFF ->
     let pos = b.position in
     if pos + 3 > b.length then resize b 3;
     Bytes.unsafe_set b.buffer (pos    )
       (Char.unsafe_chr (0xE0 lor (u lsr 12)));
     Bytes.unsafe_set b.buffer (pos + 1)
       (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
     Bytes.unsafe_set b.buffer (pos + 2)
       (Char.unsafe_chr (0x80 lor (u land 0x3F)));
     b.position <- pos + 3
 | u when u <= 0x10FFFF ->
     let pos = b.position in
     if pos + 4 > b.length then resize b 4;
     Bytes.unsafe_set b.buffer (pos    )
       (Char.unsafe_chr (0xF0 lor (u lsr 18)));
     Bytes.unsafe_set b.buffer (pos + 1)
       (Char.unsafe_chr (0x80 lor ((u lsr 12) land 0x3F)));
     Bytes.unsafe_set b.buffer (pos + 2)
       (Char.unsafe_chr (0x80 lor ((u lsr 6) land 0x3F)));
     Bytes.unsafe_set b.buffer (pos + 3)
       (Char.unsafe_chr (0x80 lor (u land 0x3F)));
     b.position <- pos + 4
 | _ -> assert false

 let add_utf_16be_uchar b u = match Uchar.to_int u with
 | u when u < 0 -> assert false
 | u when u <= 0xFFFF ->
     let pos = b.position in
     if pos + 2 > b.length then resize b 2;
     Bytes.unsafe_set b.buffer (pos    ) (Char.unsafe_chr (u lsr 8));
     Bytes.unsafe_set b.buffer (pos + 1) (Char.unsafe_chr (u land 0xFF));
     b.position <- pos + 2
 | u when u <= 0x10FFFF ->
     let u' = u - 0x10000 in
     let hi = 0xD800 lor (u' lsr 10) in
     let lo = 0xDC00 lor (u' land 0x3FF) in
     let pos = b.position in
     if pos + 4 > b.length then resize b 4;
     Bytes.unsafe_set b.buffer (pos    ) (Char.unsafe_chr (hi lsr 8));
     Bytes.unsafe_set b.buffer (pos + 1) (Char.unsafe_chr (hi land 0xFF));
     Bytes.unsafe_set b.buffer (pos + 2) (Char.unsafe_chr (lo lsr 8));
     Bytes.unsafe_set b.buffer (pos + 3) (Char.unsafe_chr (lo land 0xFF));
     b.position <- pos + 4
 | _ -> assert false

 let add_utf_16le_uchar b u = match Uchar.to_int u with
 | u when u < 0 -> assert false
 | u when u <= 0xFFFF ->
     let pos = b.position in
     if pos + 2 > b.length then resize b 2;
     Bytes.unsafe_set b.buffer (pos    ) (Char.unsafe_chr (u land 0xFF));
     Bytes.unsafe_set b.buffer (pos + 1) (Char.unsafe_chr (u lsr 8));
     b.position <- pos + 2
 | u when u <= 0x10FFFF ->
     let u' = u - 0x10000 in
     let hi = 0xD800 lor (u' lsr 10) in
     let lo = 0xDC00 lor (u' land 0x3FF) in
     let pos = b.position in
     if pos + 4 > b.length then resize b 4;
     Bytes.unsafe_set b.buffer (pos    ) (Char.unsafe_chr (hi land 0xFF));
     Bytes.unsafe_set b.buffer (pos + 1) (Char.unsafe_chr (hi lsr 8));
     Bytes.unsafe_set b.buffer (pos + 2) (Char.unsafe_chr (lo land 0xFF));
     Bytes.unsafe_set b.buffer (pos + 3) (Char.unsafe_chr (lo lsr 8));
     b.position <- pos + 4
 | _ -> assert false

let add_substring b s offset len =
  if offset < 0 || len < 0 || offset > String.length s - len
  then invalid_arg "Buffer.add_substring/add_subbytes";
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  Bytes.blit_string s offset b.buffer b.position len;
  b.position <- new_position

let add_subbytes b s offset len =
  add_substring b (Bytes.unsafe_to_string s) offset len

let add_string b s =
  let len = String.length s in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  Bytes.blit_string s 0 b.buffer b.position len;
  b.position <- new_position

let add_bytes b s = add_string b (Bytes.unsafe_to_string s)

let add_buffer b bs =
  add_subbytes b bs.buffer 0 bs.position

(* read up to [len] bytes from [ic] into [b]. *)
let rec add_channel_rec b ic len =
  if len > 0 then (
    let n = input ic b.buffer b.position len in
    b.position <- b.position + n;
    if n = 0 then raise End_of_file
    else add_channel_rec b ic (len-n)   (* n <= len *)
  )

let add_channel b ic len =
  if len < 0 
#if BS
#else
  || len > Sys.max_string_length 
#end
  then   (* PR#5004 *)
    invalid_arg "Buffer.add_channel";
  if b.position + len > b.length then resize b len;
  add_channel_rec b ic len

let output_buffer oc b =
  output oc b.buffer 0 b.position

let closing = function
  | '(' -> ')'
  | '{' -> '}'
  | _ -> assert false

(* opening and closing: open and close characters, typically ( and )
   k: balance of opening and closing chars
   s: the string where we are searching
   start: the index where we start the search. *)
let advance_to_closing opening closing k s start =
  let rec advance k i lim =
    if i >= lim then raise Not_found else
    if s.[i] = opening then advance (k + 1) (i + 1) lim else
    if s.[i] = closing then
      if k = 0 then i else advance (k - 1) (i + 1) lim
    else advance k (i + 1) lim in
  advance k start (String.length s)

let advance_to_non_alpha s start =
  let rec advance i lim =
    if i >= lim then lim else
    match s.[i] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> advance (i + 1) lim
    | _ -> i in
  advance start (String.length s)

(* We are just at the beginning of an ident in s, starting at start. *)
let find_ident s start lim =
  if start >= lim then raise Not_found else
  match s.[start] with
  (* Parenthesized ident ? *)
  | '(' | '{' as c ->
     let new_start = start + 1 in
     let stop = advance_to_closing c (closing c) 0 s new_start in
     String.sub s new_start (stop - start - 1), stop + 1
  (* Regular ident *)
  | _ ->
     let stop = advance_to_non_alpha s (start + 1) in
     String.sub s start (stop - start), stop

(* Substitute $ident, $(ident), or ${ident} in s,
    according to the function mapping f. *)
let add_substitute b f s =
  let lim = String.length s in
  let rec subst previous i =
    if i < lim then begin
      match s.[i] with
      | '$' as current when previous = '\\' ->
         add_char b current;
         subst ' ' (i + 1)
      | '$' ->
         let j = i + 1 in
         let ident, next_i = find_ident s j lim in
         add_string b (f ident);
         subst ' ' next_i
      | current when previous == '\\' ->
         add_char b '\\';
         add_char b current;
         subst ' ' (i + 1)
      | '\\' as current ->
         subst current (i + 1)
      | current ->
         add_char b current;
         subst current (i + 1)
    end else
    if previous = '\\' then add_char b previous in
  subst ' ' 0

let truncate b len =
    if len < 0 || len > length b then
      invalid_arg "Buffer.truncate"
    else
      b.position <- len
