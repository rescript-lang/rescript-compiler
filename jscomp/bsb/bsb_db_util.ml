
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
open Bsb_db
let dir_of_module_info (x : module_info)
  = 
  Filename.dirname x.name_sans_extension
     
let conflict_module_info modname a b = 
  Bsb_exception.conflict_module
    modname
    (dir_of_module_info a)
    (dir_of_module_info b)

(* merge data info from two directories*)    
let merge (acc : t) (sources : t) : t =
  String_map.merge acc sources (fun modname k1 k2 ->
      match k1 , k2 with
      | None , None ->
        assert false
      | Some a, Some b  ->
        conflict_module_info modname 
          a
          b
      | Some v, None  -> Some v
      | None, Some v ->  Some v
    )

let sanity_check (map : t) = 
  String_map.iter map (fun m module_info -> 
      match module_info.ml_info, module_info.mli_info with 
      | Ml_empty, _ ->      
        Bsb_exception.no_implementation m 
      | Ml_source(impl_is_re,_), Mli_source(intf_is_re,_)   
        ->
        if impl_is_re <> intf_is_re then
          Bsb_exception.not_consistent m
      | Ml_source _ , Mli_empty -> ()    
    )    

(* invariant check:
  ml and mli should have the same case, same path
*)  
let check (x : module_info) name_sans_extension =  
  if x.name_sans_extension <> name_sans_extension then 
    Bsb_exception.invalid_spec 
      (Printf.sprintf 
         "implementation and interface have different path names or different cases %s vs %s"
         x.name_sans_extension name_sans_extension)

let adjust_module_info 
  (x : module_info option) 
  (suffix : string) 
  (name_sans_extension : string) 
  (upper : case) : module_info =
  match suffix with 
  | ".ml" -> 
    let ml_info = Ml_source  ( false, upper) in 
    (match x with 
    | None -> 
      {name_sans_extension ; ml_info ; mli_info = Mli_empty}
    | Some x -> 
      check x name_sans_extension;
      {x with ml_info })
  | ".re" -> 
    let ml_info = Ml_source  ( true, upper)in
    (match x with None -> 
      {name_sans_extension; ml_info  ; mli_info = Mli_empty} 
    | Some x -> 
      check x name_sans_extension;
      {x with ml_info})
  | ".mli" ->  
    let mli_info = Mli_source (false, upper) in 
    (match x with None -> 
      {name_sans_extension; mli_info ; ml_info = Ml_empty}
    | Some x -> 
      check x name_sans_extension;
      {x with mli_info })
  | ".rei" -> 
    let mli_info = Mli_source (true, upper) in
    (match x with None -> 
      { name_sans_extension; mli_info ; ml_info = Ml_empty}
    | Some x -> 
      check x name_sans_extension;
      { x with mli_info})
  | _ -> 
    Ext_pervasives.failwithf ~loc:__LOC__ 
      "don't know what to do with %s%s" 
      name_sans_extension suffix

let collect_module_by_filename 
  ~(dir : string) (map : t) (file_name : string) : t  = 
  let module_name, upper = 
    Ext_modulename.module_name_of_file_if_any_with_upper file_name in 
  let suffix = Ext_path.get_extension file_name in 
  let name_sans_extension = 
    Ext_path.chop_extension (Filename.concat dir file_name) in 
  String_map.adjust 
    map
    module_name 
    (fun (opt_module_info : module_info option)-> 
       adjust_module_info 
         opt_module_info
         suffix 
         name_sans_extension upper )
