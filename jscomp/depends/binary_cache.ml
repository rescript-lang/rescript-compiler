
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


type ml_kind =
  | Ml of string 
  | Re of string 
  | Ml_empty
type mli_kind = 
  | Mli of string 
  | Rei of string
  | Mli_empty

type module_info = 
  {
    mli : mli_kind ; 
    ml : ml_kind ; 
    mll : string option 
  }

type t = module_info String_map.t 

let module_info_magic_number = "BSBUILD20161012"

let write_build_cache bsbuild (bs_files : module_info String_map.t)  = 
  let oc = open_out_bin bsbuild in 
  output_string oc module_info_magic_number ;
  output_value oc bs_files ;
  close_out oc 

let read_build_cache bsbuild : module_info String_map.t = 
  let ic = open_in bsbuild in 
  let buffer = really_input_string ic (String.length module_info_magic_number) in
  assert(buffer = module_info_magic_number); 
  let data : module_info String_map.t = input_value ic in 
  close_in ic ;
  data 


let bsbuild_cache = ".bsbuild"


(* TODO check duplication *)
let module_info_of_ml exist ml : module_info =
  match exist with 
  | None -> { ml  = Ml ml ; mli = Mli_empty ; mll = None }
  | Some x -> { x with ml = Ml ml}

let module_info_of_re exist ml : module_info =
  match exist with 
  | None -> { ml  = Re ml ; mli = Mli_empty ; mll = None }
  | Some x -> { x with ml = Re ml} 

let module_info_of_mli exist mli : module_info = 
  match exist with 
  | None -> { mli  = Mli mli ; ml = Ml_empty ; mll = None }
  | Some x -> { x with mli = Mli mli} 

let module_info_of_rei exist mli : module_info = 
  match exist with 
  | None -> { mli  = Rei mli ; ml = Ml_empty ; mll = None }
  | Some x -> { x with mli = Rei mli} 

let module_info_of_mll exist mll : module_info = 
  match exist with 
  | None -> { mll  = Some mll ; ml = Ml_empty ; mli = Mli_empty }
  | Some x -> { x with mll = Some mll} 

let simple_concat (x : string)  y =
  if x = Filename.current_dir_name then y else 
  if y = Filename.current_dir_name then x else 
    Filename.concat x y

let map_update ?dir (map : t)  name : t  = 
  let prefix   = 
    match dir with
    | None -> fun x ->  x
    | Some v -> fun x ->  simple_concat v x in
  let module_name = Ext_filename.module_name_of_file_if_any name in 
  let handle name v cb =
    String_map.add module_name
      (cb v (prefix name ) ) map 
  in 
  let aux v name = 
    if Filename.check_suffix name ".ml" then handle name  v  module_info_of_ml  else
    if Filename.check_suffix name ".mll" then handle name  v  module_info_of_mll  else 
    if Filename.check_suffix name ".mli" then handle name  v  module_info_of_mli else 
    if Filename.check_suffix name ".re" then handle name v module_info_of_re else 
    if Filename.check_suffix name ".rei" then handle name v module_info_of_rei else 
      map    in 
  match String_map.find module_name map with 
  | exception Not_found 
    -> aux None name 
  | v -> 
    aux (Some v ) name
