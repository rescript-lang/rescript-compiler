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
type key = int
type  t = key  Hash_set_gen.t 
let create = Hash_set_gen.create
let clear = Hash_set_gen.clear
let reset = Hash_set_gen.reset
let copy = Hash_set_gen.copy
let iter = Hash_set_gen.iter
let fold = Hash_set_gen.fold
let length = Hash_set_gen.length
let stats = Hash_set_gen.stats
let elements = Hash_set_gen.elements


let key_index (h :  t ) key =
  (Bs_hash_stubs.hash_int  key) land (Array.length h.data - 1)

let remove (h : t) key =
  let rec remove_bucket = function
    | [ ] ->
      [ ]
    | k :: next ->
      if  (k : key)  = key
      then begin h.size <- h.size - 1; next end
      else k :: remove_bucket next in
  let i = key_index h key in
  h.data.(i) <- remove_bucket h.data.(i)

let rec small_bucket_mem key lst =
  match lst with 
  | [] -> false 
  | key1::rest -> 
     (key : key) = key1 ||
    match rest with 
    | [] -> false 
    | key2 :: rest -> 
       (key : key) = key2 ||
      match rest with 
      | [] -> false 
      | key3 :: rest -> 
         (key : key) = key3 ||
         small_bucket_mem key rest 

let add (h : t) key =
  let i = key_index h key  in 
  if not (small_bucket_mem key  h.data.(i)) then 
    begin 
      h.data.(i) <- key :: h.data.(i);
      h.size <- h.size + 1 ;
      if h.size > Array.length h.data lsl 1 then Hash_set_gen.resize key_index h
    end

let mem (h :  t) key =
  small_bucket_mem key h.data.(key_index h key) 
