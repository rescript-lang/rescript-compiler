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


let reverse_in_place a =
  let aux a i len =
    if len=0 then ()
    else
      for k = 0 to (len-1)/2 do
        let t = Array.unsafe_get a (i+k) in
        Array.unsafe_set a (i+k) ( Array.unsafe_get a (i+len-1-k));
        Array.unsafe_set a (i+len-1-k) t;
      done
  in
  aux a 0 (Array.length a)


let reverse_of_list =  function
  | [] -> [||]
  | hd::tl as l ->
    let len = List.length l in
    let a = Array.make len hd in
    let rec fill i = function
      | [] -> a
      | hd::tl -> Array.unsafe_set a (len - i - 2) hd; fill (i+1) tl in
    fill 0 tl

let filter f a =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len 
    then reverse_of_list acc 
    else
      let v = Array.unsafe_get a i in
      if f  v then 
        aux (v::acc) (i+1)
      else aux acc (i + 1) 
  in aux [] 0


let filter_map (f : _ -> _ option) a =
  let arr_len = Array.length a in
  let rec aux acc i =
    if i = arr_len 
    then reverse_of_list acc 
    else
      let v = Array.unsafe_get a i in
      match f  v with 
      | Some v -> 
        aux (v::acc) (i+1)
      | None -> 
        aux acc (i + 1) 
  in aux [] 0

