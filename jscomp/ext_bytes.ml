(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let escaped s =
  let n = ref 0 in
  for i = 0 to Bytes.length s - 1 do
    n := !n +
      (match Bytes.unsafe_get s i with
       | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | ' ' .. '~' -> 1
       | _ -> 4)
  done;
  if !n = Bytes.length s then Bytes.copy s else begin
    let s' = Bytes.create !n in
    n := 0;
    for i = 0 to Bytes.length s - 1 do
      begin match Bytes.unsafe_get s i with
      | ('"' | '\\') as c ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n c
      | '\n' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'n'
      | '\t' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 't'
      | '\r' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'r'
      | '\b' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'b'
      | (' ' .. '~') as c -> Bytes.unsafe_set s' !n c
      | c ->
          let a = char_code c in
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + a / 100));
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + a mod 10));
      end;
      incr n
    done;
    s'
  end
