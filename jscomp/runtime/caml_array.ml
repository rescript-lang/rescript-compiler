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







let caml_array_sub (x : 'a array) (offset : int) (len : int) = 
  let result = Js.Array.new_uninitialized len  in
  let j = ref 0 and i = ref offset in
  while !j < len do
    result.(!j) <- x.(!i);
    incr j; 
    incr i;
  done;
  result


let rec len acc l  =
  match l with
  | [] -> acc 
  | x::xs -> len (Array.length x + acc) xs

let rec fill arr i l = 
  match l with 
  | [] -> ()
  | x :: xs -> 
      let l = Array.length x in
      let k = ref i in
      let j = ref 0 in
      while !j < l do 
        arr.(!k) <- x.(!j);
        incr k; 
        incr j;
      done;
      fill arr !k  xs 

let  caml_array_concat (l : 'a array list) : 'a array =
  let v = len 0 l in
  let result = Js.Array.new_uninitialized v in
  fill result 0 l ;
  result

let caml_array_set xs index newval = 
  if(index <0 || index >= Array.length xs) 
  then (raise (Invalid_argument "index out of bounds"))
  else xs.(index) <- newval

let caml_array_get xs index =  
  if(index <0 || index >= Array.length xs) then
    raise (Invalid_argument "index out of bounds")
  else xs.(index)

let caml_make_vect len init = 
  let b = Js.Array.new_uninitialized len in
  for i = 0 to len - 1 do 
    b.(i) <- init
  done;
  b

let caml_array_blit a1 i1 a2 i2 len = 
  if i2 <= i1 then 
    for j = 0 to len - 1 do
      a2.(j+i2)<- a1.(j+i1)
    done
  else
    for j = len - 1 downto 0 do
      a2.(j+i2) <- a1.(j+i1)
    done
