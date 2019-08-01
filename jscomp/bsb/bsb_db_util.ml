
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
type module_info = Bsb_db.module_info
type t = Bsb_db.t
type case = Bsb_db.case


     
let conflict_module_info modname (a : module_info) (b : module_info) = 
  Bsb_exception.conflict_module
    modname
    a.dir
    b.dir

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
      if module_info.info = Mli then
        Bsb_exception.no_implementation m 
    )    

(* invariant check:
  ml and mli should have the same case, same path
*)  
let check (x : module_info) 
  name_sans_extension 
  case 
  is_re 
  (module_info : Bsb_db.info)
  =  
  let x_ml_info = x.info in  
  (if x.name_sans_extension <> name_sans_extension 
   || x.case <> case 
   || x.is_re <> is_re 
   || x_ml_info = module_info 
   || x_ml_info = Ml_mli
   then 
     Bsb_exception.invalid_spec 
       (Printf.sprintf 
          "implementation and interface have different path names or different cases %s vs %s"
          x.name_sans_extension name_sans_extension));
  x.info <- Ml_mli;      
  x

  
let adjust_module_info 
  (x : module_info option) 
  (suffix : string) 
  (name_sans_extension : string) 
  (case : case) : module_info =
  let dir = Filename.dirname name_sans_extension in 
  let info, is_re = 
    match suffix with 
    | ".ml" -> Bsb_db.Ml, false
    | ".re" -> Ml, true
    | ".mli" -> Mli, false
    | ".rei" -> Mli, true 
    | _ -> 
      Ext_pervasives.failwithf ~loc:__LOC__ 
        "don't know what to do with %s%s" 
        name_sans_extension suffix in 
   match x with 
    | None -> 
      {dir ; name_sans_extension ; info ; is_re ; case }
    | Some x -> 
      check x name_sans_extension case is_re info      
  
let collect_module_by_filename 
  ~(dir : string) (map : t) (file_name : string) : t  = 
  let module_name, upper = 
    Ext_filename.module_name_with_case file_name in 
  let suffix = Ext_path.get_extension file_name in 
  let name_sans_extension = 
     Filename.concat dir (Ext_filename.chop_extension_maybe file_name) in 
  String_map.adjust 
    map
    module_name 
    (fun (opt_module_info : module_info option)-> 
       adjust_module_info 
         opt_module_info
         suffix 
         name_sans_extension upper )
