(* Copyright (C) 2019 - Present Authors of BuckleScript
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

 let bsbuild_cache = Literals.bsbuild_cache


 type group = {
   modules : string array ; 
   meta_info_offset : int 
 }

type t = group array * string (* string is whole content*)



let bool buf b =   
  Buffer.add_char buf (if b then '1' else '0')



let decode_ml_info (x : char ) : Bsb_db.ml_info =   
  match x with 
  | '0' -> Ml_empty 
  | '1' -> Ml_source(false,false) 
  | '2' -> Ml_source(false,true) 
  | '3' -> Ml_source(true, false) 
  | '4' -> Ml_source(true, true) 
  | _ -> assert false


let decode_mli_info (x : char ) : Bsb_db.mli_info =   
  match x with 
  | '0' -> Mli_empty 
  | '1' -> Mli_source(false,false) 
  | '2' -> Mli_source(false,true) 
  | '3' -> Mli_source(true, false)
  | '4' -> Mli_source(true, true) 
  | _ -> assert false







type cursor = int ref 

let extract_line (x : string) (cur : cursor) : string =
  Ext_string.extract_until x cur '\n'

let next_mdoule_info (s : string) (cur : int) ~count  =  
  if count = 0 then cur 
  else 
    Ext_string.index_count s cur '\n' count  + 1

let rec decode (x : string) (offset : cursor) =   
  let len = int_of_string (extract_line x offset) in  
  Array.init len (fun _ ->  decode_single x offset)
and decode_single x (offset : cursor) : group = 
  let cardinal = int_of_string (extract_line x offset) in 
  let modules = decode_modules x offset cardinal in 
  let meta_info_offset = !offset in 
  offset := next_mdoule_info x meta_info_offset ~count:cardinal;
  { modules ; meta_info_offset }
and decode_modules x (offset : cursor) cardinal =   
  let result = Array.make cardinal "" in 
  for i = 0 to cardinal - 1 do 
    Array.unsafe_set result i (extract_line x offset)
  done ;
  result
  





let read_build_cache ~dir  : t = 
  let ic = open_in_bin (Filename.concat dir bsbuild_cache) in 
  let len = in_channel_length ic in 
  let all_content = really_input_string ic len in 
  let offset = ref 0 in 
  let cur_module_info_magic_number = extract_line all_content offset in 
  assert (cur_module_info_magic_number = Bs_version.version); 
  decode all_content offset, all_content

let cmp (a : string) b = String_map.compare_key a b   

let rec binarySearchAux (arr : string array) (lo : int) (hi : int) (key : string)  : _ option = 
  let mid = (lo + hi)/2 in 
  let midVal = Array.unsafe_get arr mid in 
  let c = cmp key midVal in 
  if c = 0 then Some (mid)
  else if c < 0 then  (*  a[lo] =< key < a[mid] <= a[hi] *)
    if hi = mid then  
      let loVal = (Array.unsafe_get arr lo) in 
      if  loVal = key then Some lo
      else None
    else binarySearchAux arr lo mid key 
  else  (*  a[lo] =< a[mid] < key <= a[hi] *)
  if lo = mid then 
    let hiVal = (Array.unsafe_get arr hi) in 
    if  hiVal = key then Some hi
    else None
  else binarySearchAux arr mid hi key 

let find_opt_aux sorted key  : _ option =  
  let len = Array.length sorted in 
  if len = 0 then None
  else 
    let lo = Array.unsafe_get sorted 0 in 
    let c = cmp key lo in 
    if c < 0 then None
    else
      let hi = Array.unsafe_get sorted (len - 1) in 
      let c2 = cmp key hi in 
      if c2 > 0 then None
      else binarySearchAux sorted 0 (len - 1) key

let find_opt 
  ((sorteds,whole) : t )  i key 
    : Bsb_db.module_info option = 
  let group = sorteds.(i) in 
  let i = find_opt_aux group.modules key in 
  match i with 
  | None -> None 
  | Some count ->     
    let cursor = 
      ref (next_mdoule_info whole group.meta_info_offset ~count)
    in 
    let name_sans_extension = 
        Ext_string.extract_until whole cursor ',' in 
    let mli_info =  decode_mli_info whole.[!cursor] in 
    let ml_info = decode_ml_info whole.[!cursor + 1] in
    Some {mli_info ; ml_info; name_sans_extension}