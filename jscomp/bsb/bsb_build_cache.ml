
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
  | Ml_source of string 
  | Re_source of string 
  | Ml_empty
type mli_kind = 
  | Mli_source of string 
  | Rei_source of string
  | Mli_empty

type module_info = 
  {
    mli : mli_kind ; 
    ml : ml_kind ; 
  }


type t = module_info String_map.t 

(** indexed by the group *)

let module_info_magic_number = "BSBUILD20161019"

let dir_of_module_info (x : module_info)
  = 
  match x with 
  | { mli; ml;  } -> 
    begin match mli with 
      | Mli_source s | Rei_source s -> 
        Filename.dirname s 
      | Mli_empty -> 
        begin match ml with 
          | Ml_source s | Re_source s -> 
            Filename.dirname s 
          | Ml_empty -> Ext_string.empty
        end
    end

let filename_sans_suffix_of_module_info (x : module_info) =
  match x with 
  | { mli; ml;  } -> 
    begin match mli with 
      | Mli_source s | Rei_source s -> 
         s 
      | Mli_empty -> 
        begin match ml with 
          | Ml_source s | Re_source s -> 
             s 
          | Ml_empty -> assert false
        end
    end

let bsbuild_cache = ".bsbuild"    

let write_build_cache ~dir (bs_files : t array)  = 
  let oc = open_out_bin (Filename.concat dir bsbuild_cache) in 
  output_string oc module_info_magic_number ;
  output_value oc bs_files ;
  close_out oc 

let read_build_cache ~dir  : t array = 
  let ic = open_in_bin (Filename.concat dir bsbuild_cache) in 
  let buffer = really_input_string ic (String.length module_info_magic_number) in
  assert(buffer = module_info_magic_number); 
  let data : t array = input_value ic in 
  close_in ic ;
  data 




let empty_module_info = {mli = Mli_empty ;  ml = Ml_empty}


let adjust_module_info x suffix name_sans_extension =
  match suffix with 
  | ".ml" -> {x with ml = Ml_source  name_sans_extension}
  | ".re" -> {x with ml = Re_source  name_sans_extension}
  | ".mli" ->  {x with mli = Mli_source name_sans_extension }
  | ".rei" -> { x with mli = Rei_source name_sans_extension }
  | _ -> 
    Ext_pervasives.failwithf ~loc:__LOC__ 
      "don't know what to do with %s%s" 
       name_sans_extension suffix

let map_update ~dir (map : t)  
    file_name : t  = 
  
  let module_name = Ext_filename.module_name_of_file_if_any file_name in 
  let suffix = Ext_filename.get_extension file_name in 
  let file_name_sans_extension = 
      Ext_filename.chop_extension (Filename.concat dir file_name) in 
  String_map.adjust 
    module_name 
    (fun _ -> 
       adjust_module_info 
         empty_module_info 
         suffix 
         file_name_sans_extension )
    (fun v -> 
       adjust_module_info v suffix file_name_sans_extension
    )
    map
