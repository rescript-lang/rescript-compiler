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



let caml_array_sub (x : 'a array) (offset : int) (len : int) = 
  let result = Caml_array_extern.new_uninitialized len  in
  let j = ref 0 and i = ref offset in
  while !j < len do
    Caml_array_extern.unsafe_set result !j (Caml_array_extern.unsafe_get x !i);
    incr j; 
    incr i;
  done;
  result


let rec len acc l  =
  match l with
  | [] -> acc 
  | x::xs -> len (Caml_array_extern.length x + acc) xs

let rec fill arr i l = 
  match l with 
  | [] -> ()
  | x :: xs -> 
      let l = Caml_array_extern.length x in
      let k = ref i in
      let j = ref 0 in
      while !j < l do 
        Caml_array_extern.unsafe_set arr !k (Caml_array_extern.unsafe_get  x !j);
        incr k; 
        incr j;
      done;
      fill arr !k  xs 

let  caml_array_concat (l : 'a array list) : 'a array =
  let v = len 0 l in
  let result = Caml_array_extern.new_uninitialized v in
  fill result 0 l ;
  result

let caml_array_set xs index newval = 
  if index <0 || index >= Caml_array_extern.length xs
  then raise (Invalid_argument "index out of bounds")
  else Caml_array_extern.unsafe_set xs index  newval

let caml_array_get xs index =  
  if index <0 || index >= Caml_array_extern.length xs then
    raise (Invalid_argument "index out of bounds")
  else Caml_array_extern.unsafe_get xs index


let caml_make_vect len init = 
  let b = Caml_array_extern.new_uninitialized len in
  for i = 0 to len - 1 do 
    Caml_array_extern.unsafe_set b i  init
  done;
  b

let caml_make_float_vect len = 
  let b = Caml_array_extern.new_uninitialized len in
  for i = 0 to len - 1 do 
      Caml_array_extern.unsafe_set b i  0.
  done;
  b  
  
let caml_array_blit a1 i1 a2 i2 len = 
  if i2 <= i1 then 
    for j = 0 to len - 1 do
      Caml_array_extern.unsafe_set a2 (j+i2) (Caml_array_extern.unsafe_get a1 (j+i1))
    done
  else
    for j = len - 1 downto 0 do
      Caml_array_extern.unsafe_set a2 (j+i2) (Caml_array_extern.unsafe_get a1 (j+i1))
    done

