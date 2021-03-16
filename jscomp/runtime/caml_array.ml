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


external dup : 'a array -> (_ [@bs.as 0]) -> 'a array = 
  "slice"  [@@bs.send]

let %private {unsafe_get = (.!()) ; unsafe_set = (.!()<-)} = 
  (module Caml_array_extern)


let sub (x : 'a array) (offset : int) (len : int) = 
  let result = Caml_array_extern.new_uninitialized len  in
  let j = {contents = 0} and i =  {contents = offset} in
  while j.contents < len do
    result.!(j.contents) <- x.!(i.contents);
    j.contents <- j.contents + 1; 
    i.contents <- i.contents + 1;
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
    let k = {contents =  i} in
    let j = {contents = 0} in
    while j.contents < l do 
      arr.!(k.contents) <- x .!(j.contents);
      k.contents <- k.contents + 1; 
      j.contents <- j.contents + 1;
    done;
    fill arr k.contents  xs 

let  concat (l : 'a array list) : 'a array =
  let v = len 0 l in
  let result = Caml_array_extern.new_uninitialized v in
  fill result 0 l ;
  result

let set xs index newval = 
  if index <0 || index >= Caml_array_extern.length xs
  then raise (Invalid_argument "index out of bounds")
  else  xs.!( index)<-  newval

let get xs index =  
  if index <0 || index >= Caml_array_extern.length xs then
    raise (Invalid_argument "index out of bounds")
  else  xs.!( index)


let make len init = 
  let b = Caml_array_extern.new_uninitialized len in
  for i = 0 to len - 1 do 
    b.!(i) <-  init
  done;
  b

let make_float len = 
  let b = Caml_array_extern.new_uninitialized len in
  for i = 0 to len - 1 do 
    b.!(i) <-  0.
  done;
  b  

let blit a1 i1 a2 i2 len = 
  if i2 <= i1 then 
    for j = 0 to len - 1 do
      a2.! (j+i2) <- a1.! (j+i1)
    done
  else
    for j = len - 1 downto 0 do
      a2 .!(j+i2) <-  a1.! (j+i1)
    done

