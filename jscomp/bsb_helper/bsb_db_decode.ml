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

type cursor = int ref 

let extract_line (x : string) (cur : cursor) : string =
  Ext_string.extract_until x cur '\n'

let next_mdoule_info (s : string) (cur : int) ~count  =  
  if count = 0 then cur 
  else 
    Ext_string.index_count s cur '\n' count  + 1

let rec decode_internal (x : string) (offset : cursor) =   
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
  let _cur_module_info_magic_number = extract_line all_content offset in 
  (* assert (cur_module_info_magic_number = Bs_version.version);  *)
  decode_internal all_content offset, all_content

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

type ml_info = Ml_source of bool * Bsb_db.case

type module_info =  {
  mli_info : Bsb_db.mli_info;
  ml_info : ml_info;
  name_sans_extension : string
} 


let find_opt 
  ((sorteds,whole) : t )  i (key : string) 
    : module_info option = 
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
    Some (match whole.[!cursor] with
        | 'f'
          -> 
          {
            name_sans_extension;
            ml_info = Ml_source(false,false);
            mli_info = Mli_empty
          }
        | 'g'
          -> 
          {
            name_sans_extension;
            ml_info = Ml_source(false,false);
            mli_info = Mli_source(false,false)
          }
        | 'h'
          ->
          {
            name_sans_extension;
            ml_info = Ml_source(false,false);
            mli_info = Mli_source(false,true)
          }
        | 'i'
          ->
          {
            name_sans_extension;
            ml_info = Ml_source(false,false);
            mli_info = Mli_source(true,false)}

        | 'j'
          ->
          {
            name_sans_extension;
            ml_info = Ml_source(false,false);
            mli_info = Mli_source(true,true)}

        (* another group *)
        | 'k'
          -> {
              name_sans_extension;
              ml_info = Ml_source(false,true) ;
              mli_info = Mli_empty
            } 
        | 'l'
          -> {
              name_sans_extension;
              ml_info = Ml_source(false,true) ;
              mli_info = Mli_source(false,false)
            } 
        | 'm'
          ->
          {
            name_sans_extension;
            ml_info = Ml_source(false,true) ;
            mli_info = Mli_source(false,true)
          }
        | 'n'
          ->
          {
            name_sans_extension;
            ml_info = Ml_source(false,true) ;
            mli_info = Mli_source(true,false)
          }
        | 'o'
          ->
          {
            name_sans_extension;
            ml_info = Ml_source(false,true) ;
            mli_info = Mli_source(true,true)
          }
        (* another group*)
        | 'p'
          -> 
          {name_sans_extension;
           ml_info = Ml_source(true, false);
           mli_info = Mli_empty
          }


        | 'q'
          ->
          {name_sans_extension;
           ml_info = Ml_source(true, false);
           mli_info = Mli_source(false,false)
          }
        | 'r'
          ->
          {name_sans_extension;
           ml_info = Ml_source(true, false);
           mli_info = Mli_source(false,true)
          }
        | 's'
          ->
          {name_sans_extension;
           ml_info = Ml_source(true, false);
           mli_info = Mli_source(true,false)
          }
        | 't'
          -> 
          {name_sans_extension;
           ml_info = Ml_source(true, false);
           mli_info = Mli_source(true,true)
          }
        (* another group *)
        | 'u'
          -> 
          {name_sans_extension ; 
           ml_info = Ml_source(true, true);
           mli_info = Mli_empty
          }
        | 'v'
          ->
          {name_sans_extension ; 
           ml_info = Ml_source(true, true);
           mli_info = Mli_source(false,false)
          }
        | 'w'
          ->
          {name_sans_extension ; 
           ml_info = Ml_source(true, true);
           mli_info = Mli_source(false,true)
          }
        | 'x' -> 

          {name_sans_extension ; 
           ml_info = Ml_source(true, true);
           mli_info = Mli_source(true,false)
          }
        | 'y' 
          -> 
          {name_sans_extension ; 
           ml_info = Ml_source(true, true);
           mli_info = Mli_source(true,true)
          }
        | 'a'

        | 'b'

        | 'c'

        | 'd'

        | 'e' 
          -> assert false    
        | _ -> assert false)
