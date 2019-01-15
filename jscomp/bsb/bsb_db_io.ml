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


 type t = string array * Bsb_db.module_info array
 type ts = t array 

let bsbuild_cache = ".bsbuild"    

let module_info_magic_number = "BSBUILD20170802"

let nl buf = 
  Buffer.add_char buf '\n'
let comma buf = 
  Buffer.add_char buf ','
let bool buf b =   
  Buffer.add_char buf (if b then '1' else '0')
let rec encode_module_info  (x : Bsb_db.module_info) (buf : Buffer.t) =   
  encode_mli x.mli buf;
  nl buf; 
  encode_ml x.ml buf 
and encode_ml (ml_kind : Bsb_db.ml_kind ) (buf : Buffer.t) =   
  match ml_kind with 
  | Ml_empty -> Buffer.add_char buf '0'
  | Ml_source (name,is_re,case) -> 
    encode_triple name is_re case buf
and encode_mli (mli_kind : Bsb_db.mli_kind) (buf : Buffer.t) =     
  match mli_kind with 
  | Mli_empty -> Buffer.add_char buf '0'
  | Mli_source (name,is_re,case) -> 
    encode_triple name is_re case buf 
and encode_triple name is_re case buf =     
  Buffer.add_string buf name; 
  comma buf;
  bool buf is_re ; 
  comma buf;
  bool buf case

let encode_pair (name : string) (module_info : Bsb_db.module_info) 
  (buf : Buffer.t) buf2 = 
  nl buf; 
  Buffer.add_string buf name; 
  nl buf2; 
  encode_module_info module_info buf2 

let encode_single (x : Bsb_db.t) (buf : Buffer.t)  (buf2 : Buffer.t)=  
  let len = String_map.cardinal x in 
  nl buf ; 
  Buffer.add_string buf (string_of_int len);
  String_map.iter (fun name module_info -> encode_pair name module_info buf buf2) x   

let encode (x : Bsb_db.ts) (buf : Buffer.t) (buf2 : Buffer.t) =     
  nl buf; 
  let len = Array.length x in 
  Buffer.add_string buf (string_of_int len); 
  Ext_array.iter x (fun x -> encode_single x buf buf2)

type cursor = int ref 

let extract_line (x : string) (cur : cursor) : string =
  Ext_string.extract_until x cur '\n'

let rec decode (x : string) (offset : cursor) =   
  let len = int_of_string (extract_line x offset) in  
  Array.init len (fun _ ->  decode_single x offset)
and decode_single x (offset : cursor) = 
  let cardinal = int_of_string (extract_line x offset) in 
  let a = decode_modules x offset cardinal in 
  let b = decode_module_infos x offset cardinal in 
  a, b
and decode_modules x offset cardinal =   
  Array.init cardinal (fun _ -> extract_line x offset)
and decode_module_infos x offset cardinal =   
  Array.init cardinal (fun _ -> 
      let mli = decode_triple_intf (extract_line x offset) in 
      let ml = decode_triple_impl (extract_line x offset) in 
      Bsb_db.{mli ; ml}
    )
and decode_triple_intf (pair : string) : Bsb_db.mli_kind = 
  if pair = "0" then Mli_empty 
  else 
    let cur = ref 0 in 
    let name = Ext_string.extract_until pair cur ',' in 
    let is_re =  Ext_string.extract_until pair cur ',' in 
    let case = Ext_string.extract_until pair cur ',' in 
    Mli_source(name,  is_re = "1", case = "1" )  
and decode_triple_impl (pair : string) : Bsb_db.ml_kind =     
  if pair = "0" then Ml_empty
  else 
    let cur = ref 0 in 
    let name = Ext_string.extract_until pair cur ',' in 
    let is_re =  Ext_string.extract_until pair cur ',' in 
    let case = Ext_string.extract_until pair cur ',' in 
    Ml_source (name, is_re = "1", case = "1")

#if 0 then 
let linear (x : Bsb_db.ts) : ts = 
  Ext_array.map  x String_map.to_sorted_array

let write_build_cache ~dir (bs_files : Bsb_db.ts)  : unit = 
  let oc = open_out_bin (Filename.concat dir bsbuild_cache) in 
  output_string oc module_info_magic_number ;
  output_value oc (linear bs_files);
  close_out oc 
#else 
let write_build_cache ~dir (bs_files : Bsb_db.ts)  : unit = 
  let oc = open_out_bin (Filename.concat dir bsbuild_cache) in 
  output_string oc module_info_magic_number ;
  let buf = Buffer.create 10_000 in 
  let buf2 = Buffer.create 60_000 in 
  encode bs_files buf buf2; 
  Buffer.output_buffer oc buf ; 
  Buffer.output_buffer oc buf2 ; 
  close_out oc 
#end
#if 0 then 
let read_build_cache ~dir  : ts = 
  let ic = open_in_bin (Filename.concat dir bsbuild_cache) in 
  let buffer = really_input_string ic (String.length module_info_magic_number) in
  assert(buffer = module_info_magic_number); 
  let data : ts = input_value ic in 
  close_in ic ;
  data 
#else 
let read_build_cache ~dir  : (string array * _) array = 
  let ic = open_in_bin (Filename.concat dir bsbuild_cache) in 
  let len = in_channel_length ic in 
  let all_content = really_input_string ic len in 
  let offset = ref 0 in 
  let cur_module_info_magic_number = extract_line all_content offset in 
  assert (cur_module_info_magic_number = module_info_magic_number); 
  decode all_content offset
#end
let cmp (a : string) b = String_map.compare_key a b   

let rec binarySearchAux arr (lo : int) (hi : int) (key : string)  : _ option = 
  let mid = (lo + hi)/2 in 
  let midVal = Array.unsafe_get arr mid in 
  let c = cmp key midVal [@bs] in 
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
    let c = cmp key lo [@bs] in 
    if c < 0 then None
    else
      let hi = Array.unsafe_get sorted (len - 1) in 
      let c2 = cmp key hi [@bs]in 
      if c2 > 0 then None
      else binarySearchAux sorted 0 (len - 1) key

let find_opt ((sorted,_) as x  : t) key : _ option = 
  let i = find_opt_aux sorted key in 
  match i with 
  | None -> None 
  | Some index -> Some ((snd x).(index))