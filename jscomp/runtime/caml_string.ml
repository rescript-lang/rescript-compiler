(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)



(** *)

external new_uninitialized : int -> bytes = "Array"  [@@bs.new]
external to_int_array : bytes -> int array = "%identity"
external of_int_array : int array -> bytes = "%identity"


(***********************)
(* replaced primitives *)
(* Note that we explicitly define [unsafe_set] instead of 
   using {!Bytes.unsafe_set} since for some standard libraries, 
   it might point to ["%string_unsafe_set"]
*)
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
external length : bytes -> int = "%bytes_length"


let string_of_char = Bs_string.of_char
let add = Bs_string.append 

let caml_string_get s i= 
  if i >=Bs_string.length s || i < 0  then
    raise (Invalid_argument "index out of bounds") 
  else Bs_string.unsafe_get s i


let caml_create_string len : bytes = 
  (* Node raise [RangeError] exception *)
  if len < 0 then raise (Invalid_argument "String.create")
  else 
    let result = new_uninitialized len in 
    for i = 0 to  len - 1 do 
      unsafe_set result i '\000'
    done ;
    result 





let caml_fill_string (s : bytes) i l (c : char) = 
  if l > 0 then
    for k = i to l + i - 1 do 
      unsafe_set s k c 
    done

(**
   TODO: [min] is not type specialized in OCaml
 *)
let caml_blit_string s1 i1 s2 i2 (len : int ) = 
  if len > 0 then
    let off1 = Bs_string.length s1 - i1 in
    if len <= off1 then 
      for i = 0 to len - 1 do 
        unsafe_set s2 (i2 + i) (String.unsafe_get s1 (i1 + i))
      done
    else 
      begin
        for i = 0 to off1 - 1 do 
          unsafe_set s2 (i2 + i) (String.unsafe_get s1 (i1 + i))
        done;
        for i = off1 to len - 1 do 
          unsafe_set s2 (i2 + i) '\000'
        done
      end

(** Same as {!Array.prototype.copyWithin} *)
let copyWithin s1 i1 i2 len = 
  if i1 < i2  then (* nop for i1 = i2 *)
    let range_a =  length s1 - i2 - 1 in
    let range_b = len - 1 in         
    let range = if range_a > range_b then range_b else range_a in
    for j = range downto 0 do
      unsafe_set s1 (i2 + j) (unsafe_get s1 (i1 + j))
    done
  else if i1 > i2 then
    let range_a = length s1 - i1 - 1 in 
    let range_b = len - 1 in 
    let range = if range_a > range_b then range_b else range_a in 
    for k = 0 to range  do 
      unsafe_set s1 (i2 + k) (unsafe_get s1 (i1 + k))
    done

(* TODO: when the compiler could optimize small function calls, 
   use high order functions instead
 *)
let caml_blit_bytes s1 i1 s2 i2 len = 
  if len > 0 then
    if s1 == s2 then
      copyWithin s1 i1 i2 len 
    else
      let off1 = length s1 - i1 in
      if len <= off1 then 
        for i = 0 to len - 1 do 
          unsafe_set s2 (i2 + i) (unsafe_get s1 (i1 + i))
        done
      else 
        begin
          for i = 0 to off1 - 1 do 
            unsafe_set s2 (i2 + i) (unsafe_get s1 (i1 + i))
          done;
          for i = off1 to len - 1 do 
            unsafe_set s2 (i2 + i) '\000'
          done
        end

(** checkout [Bytes.empty] -- to be inlined? *)
let bytes_of_string  s = 
  let len = Bs_string.length s in
  let res = new_uninitialized len  in
  for i = 0 to len - 1 do 
    unsafe_set res i (String.unsafe_get s i)
      (* Note that when get a char and convert it to int immedately, should be optimized
         should be [s.charCodeAt[i]]
       *)
  done;
  res






let string_of_large_bytes bytes i len = 
  let s = ref "" in
  let s_len = ref len in
  let seg = 1024 in
  if i = 0 && len <= 4 * seg && len = length bytes then 
    Bs_string.of_small_int_array  (to_int_array bytes)
  else 
    begin
      let offset = ref 0 in
      while !s_len > 0 do 
        let next = if !s_len < 1024 then !s_len else seg in
        let tmp_bytes = new_uninitialized next in
        let () = caml_blit_bytes bytes !offset tmp_bytes 0 next in 
        s := Bs_string.append !s (Bs_string.of_small_int_array (to_int_array tmp_bytes));
        s_len := !s_len - next ; 
        offset := !offset + next;
      done;
      !s
    end

let bytes_to_string a  = 
  string_of_large_bytes a 0 (length a)   


(** TODO: performance could be improved, however, 
    this function is not in critical Path
 *)
let caml_string_of_char_array chars =
    let len = Array.length chars  in
    let bytes = new_uninitialized len in
    for i = 0 to len - 1 do 
      unsafe_set bytes i chars.(i)
    done;
    bytes_to_string bytes


let caml_is_printable c = 
  let code = Char.code c in
  code > 31 && code < 127


let caml_string_get16 s i = 
  Char.code (String.unsafe_get s i) + Char.code (String.unsafe_get s (i+1)) lsl 8  

let caml_string_get32 s i = 
  Char.code (String.unsafe_get s i) + 
  Char.code (String.unsafe_get s (i+1)) lsl 8  + 
  Char.code (String.unsafe_get s (i+2)) lsl 16 + 
  Char.code (String.unsafe_get s (i+3)) lsl 24

let get s i =
  if i < 0 || i >= String.length s then
    raise (Invalid_argument "index out of bounds")
  else String.unsafe_get s i      
