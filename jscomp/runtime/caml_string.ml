(* OCamlScript compiler
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





external append : string -> string -> string = "js_string_append"

external array_append : 'a array -> 'a array -> 'a array = "js_array_append"
external string_of_small_int_array : int array -> string = "js_string_of_small_int_array"
external bytes_to_int_array : bytes -> int array = "%identity"
external bytes_of_int_array : int array -> bytes = "%identity"
external new_uninitialized_bytes : int -> bytes = "js_create_array" 

let add = append 

let caml_string_get s i= 
  if i >= String.length s || i < 0  then
    raise (Invalid_argument "index out of bounds") 
  else String.unsafe_get s i


let caml_create_string len : bytes = 
  (* Node raise [RangeError] exception *)
  if len < 0 then raise (Invalid_argument "String.create")
  else new_uninitialized_bytes len 


let caml_string_compare (s1 : string) (s2 : string) : int = 
  if s1 = s2 then 0 
  else if s1 < s2 then -1
  else 1

let caml_fill_string (s : bytes) i l (c : char) = 
  if l > 0 then
    for k = i to l + i - 1 do 
      Bytes.unsafe_set s k c 
    done

(**
   TODO: [min] is not type specialized in OCaml
 *)
let caml_blit_string s1 i1 s2 i2 (len : int ) = 
  if len > 0 then
    let off1 = String.length s1 - i1 in
    if len <= off1 then 
      for i = 0 to len - 1 do 
        Bytes.unsafe_set s2 (i2 + i) s1.[i1 + i]
      done
    else 
      begin
        for i = 0 to off1 - 1 do 
          Bytes.unsafe_set s2 (i2 + i) s1.[i1 + i]
        done;
        for i = off1 to len - 1 do 
          Bytes.unsafe_set s2 (i2 + i) '\000'
        done
      end

(* TODO: when the compiler could optimize small function calls, 
   use high order functions instead
 *)
let caml_blit_bytes s1 i1 s2 i2 len = 
  if len > 0 then
    let off1 = Bytes.length s1 - i1 in
    if len <= off1 then 
      for i = 0 to len - 1 do 
        Bytes.unsafe_set s2 (i2 + i) (Bytes.unsafe_get s1 (i1 + i))
      done
    else 
      begin
        for i = 0 to off1 - 1 do 
          Bytes.unsafe_set s2 (i2 + i) (Bytes.unsafe_get s1 (i1 + i))
        done;
        for i = off1 to len - 1 do 
          Bytes.unsafe_set s2 (i2 + i) '\000'
        done
      end

(** checkout [Bytes.empty] -- to be inlined? *)
let bytes_of_string  s = 
  let len = String.length s in
  let res = new_uninitialized_bytes len  in
  for i = 0 to len - 1 do 
    Bytes.unsafe_set res i s.[i]
      (* Note that when get a char and convert it to int immedately, should be optimized
         should be [s.charCodeAt[i]]
       *)
  done;
  res






let string_of_large_bytes bytes i len = 
  let s = ref "" in
  let s_len = ref len in
  let seg = 1024 in
  if i = 0 && len <= 4 * seg && len = Bytes.length bytes then 
    string_of_small_int_array  (bytes_to_int_array bytes)
  else 
    begin
      let offset = ref 0 in
      while !s_len > 0 do 
        let next = if !s_len < 1024 then !s_len else seg in
        let tmp_bytes = new_uninitialized_bytes next in
        let () = caml_blit_bytes bytes !offset tmp_bytes 0 next in 
        s := append !s (string_of_small_int_array (bytes_to_int_array tmp_bytes));
        s_len := !s_len - next ; 
        offset := !offset + next;
      done;
      !s
    end

let bytes_to_string a  = 
  string_of_large_bytes a 0 (String.length a)   


(** TODO: performance could be improved, however, 
    this function is not in critical Path
 *)
let caml_string_of_char_array chars =
    let len = Array.length chars  in
    let bytes = new_uninitialized_bytes len in
    for i = 0 to len - 1 do 
      Bytes.unsafe_set bytes i chars.(i)
    done;
    bytes_to_string bytes


let caml_is_printable c = 
  let code = Char.code c in
  code > 31 && code < 127


