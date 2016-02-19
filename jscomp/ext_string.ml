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



let split_by ?(keep_empty=false) is_delim str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      String.sub str 0 last_pos :: acc
    else
      if is_delim str.[pos] then
        let new_len = (last_pos - pos - 1) in
        if new_len !=0 || keep_empty then 
          let v = String.sub str (pos + 1) new_len in
          loop ( v :: acc)
            pos (pos - 1)
        else loop acc pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let split ?keep_empty  str on = 
  split_by ?keep_empty (fun x -> (x : char) = on) str  ;;

let starts_with s beg = 
  let beg_len = String.length beg in
  let s_len = String.length s in
   beg_len <=  s_len &&
  (let i = ref 0 in
    while !i <  beg_len && s.[!i] = beg.[!i] do 
      incr i 
    done;
    !i = beg_len
  )

(* TODO: optimization *)
let ends_with s beg = 
  let s_finish = String.length s - 1 in
  let s_beg = String.length beg - 1 in
  if s_beg > s_finish then false 
  else
    let rec aux j k = 
      if k < 0 then true 
      else if String.unsafe_get s j = String.unsafe_get beg k then 
        aux (j - 1) (k - 1)
      else  false in 
    aux s_finish s_beg


(**  In OCaml 4.02.3, {!String.escaped} is locale senstive, 
     this version try to make it not locale senstive, this bug is fixed
     in the compiler trunk     
*)
let escaped s =
  let rec needs_escape i =
    if i >= String.length s then false else
      match String.unsafe_get s i with
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> true
      | ' ' .. '~' -> needs_escape (i+1)
      | _ -> true
  in
  if needs_escape 0 then
    Bytes.unsafe_to_string (Ext_bytes.escaped (Bytes.unsafe_of_string s))
  else
    s


let for_all (p : char -> bool) s = 
  let len = String.length s in
  let rec aux i = 
    if i >= len then true 
    else  p (String.unsafe_get s i) && aux (i + 1)
  in aux 0 

let is_empty s = String.length s = 0


let repeat n s  =
  let len = String.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len
  done;
  Bytes.to_string res

let equal (x : string) y  = x = y
