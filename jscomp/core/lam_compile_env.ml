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








(* module E = Js_exp_make  
module S = Js_stmt_make
 *)

type env_value = 
  | Ml of Js_cmj_format.cmj_load_info
  (* | Runtime  of Js_cmj_load.cmj_load_info *)
  (** 
     [Runtime (pure, path, cmj_format)]
     A built in module probably from our runtime primitives, 
      so it does not have any [signature]

  *)
  | External  
  (** Also a js file, but this belong to third party 
  *)




type ident_info = {
  name : string;
  arity : Js_cmj_format.arity; 
  closed_lambda : Lam.t option 
}

(*
   refer: [Env.find_pers_struct]
   [ find_in_path_uncap !load_path (name ^ ".cmi")]
*)



let cached_tbl  : env_value Lam_module_ident.Hash.t
   = Lam_module_ident.Hash.create 31
let (+>) = Lam_module_ident.Hash.add cached_tbl


(* For each compilation we need reset to make it re-entrant *)
let reset () = 
  Translmod.reset ();
  Lam_module_ident.Hash.clear cached_tbl 





(** We should not provide "#moduleid" as output
    since when we print it in the end, it will 
    be escaped quite ugly
*)
let add_js_module 
    (hint_name : External_ffi_types.module_bind_name)
    (module_name : string) : Ident.t 
  = 
  let id = 
    Ident.create 
      (match hint_name with 
       | Phint_name hint_name -> 
         Ext_string.capitalize_ascii hint_name 
       (* make sure the module name is capitalized
          TODO: maybe a warning if the user hint is not good
       *)
       | Phint_nothing -> 
         Ext_modulename.js_id_name_of_hint_name module_name
      )
  in
  let lam_module_ident = 
    Lam_module_ident.of_external id module_name in  
  match Lam_module_ident.Hash.find_key_opt cached_tbl lam_module_ident with   
  | None ->
    Lam_module_ident.Hash.add 
      cached_tbl 
      lam_module_ident
      External;
    id
  | Some old_key ->
    old_key.id 






let query_external_id_info (module_id : Ident.t) (name : string) : ident_info =
  let oid  = Lam_module_ident.of_ml module_id in
  let cmj_table = 
    match Lam_module_ident.Hash.find_opt cached_tbl oid with 
    | None -> 
      let cmj_load_info = 
        Js_cmj_load.find_cmj_exn (module_id.name ^ Literals.suffix_cmj) in
      oid  +> Ml cmj_load_info  ;
      cmj_load_info.cmj_table
    | Some (Ml { cmj_table } )
      -> cmj_table
    (* | Some (Runtime _) -> assert false *)
    | Some External  -> assert false in 
  let arity , closed_lambda =  
    Js_cmj_format.query_by_name cmj_table name 
  in
  { 
    name; 
    arity;
    closed_lambda
    (* TODO shall we cache the arity ?*) 
  } 











let get_package_path_from_cmj 
    ( id : Lam_module_ident.t) 
   = 
  match Lam_module_ident.Hash.find_opt cached_tbl id with 
  | Some (Ml {cmj_table ; cmj_path}) -> 
     (cmj_path, 
          Js_cmj_format.get_npm_package_path cmj_table, 
          Js_cmj_format.get_cmj_case cmj_table )
  | Some (
   External (*| 
   Runtime _ *) ) -> 
    assert false  
      (* called by {!Js_name_of_module_id.string_of_module_id}
        can not be External
      *)
  | None -> 
    begin match id.kind with 
    | Runtime 
    | External _ -> assert false
    | Ml -> 
      let ({Js_cmj_format.cmj_table} as cmj_load_info) = 
        Js_cmj_load.find_cmj_exn (Lam_module_ident.name id ^ Literals.suffix_cmj) in           
      id +> Ml cmj_load_info;  
      (cmj_load_info.cmj_path, 
       Js_cmj_format.get_npm_package_path cmj_table, 
       Js_cmj_format.get_cmj_case cmj_table )              
    end 

let add = Lam_module_ident.Hash_set.add



(* Conservative interface *)
let is_pure_module (oid : Lam_module_ident.t)  = 
  match oid.kind with 
  | Runtime -> true 
  | External _ -> false
  | Ml  -> 
    begin match Lam_module_ident.Hash.find_opt cached_tbl oid with 
    | None -> 
      begin 
        match Js_cmj_load.find_cmj_exn (Lam_module_ident.name oid ^ Literals.suffix_cmj) with
        | cmj_load_info -> 
          oid +> Ml cmj_load_info ;
          Js_cmj_format.is_pure cmj_load_info.cmj_table
        | exception _ -> false 
      end 
    | Some (Ml{cmj_table}(*|Runtime {cmj_table}*)) ->
       Js_cmj_format.is_pure cmj_table
    | Some External ->  false
    end 
    

let get_required_modules 
    extras 
    (hard_dependencies 
     : Lam_module_ident.Hash_set.t) : Lam_module_ident.t list =  
  Lam_module_ident.Hash.iter cached_tbl (fun id  _  ->
      if not @@ is_pure_module id 
      then add  hard_dependencies id);
  Lam_module_ident.Hash_set.iter extras (fun id  -> 
      (if not @@ is_pure_module  id 
       then add hard_dependencies id : unit)
    );
  Lam_module_ident.Hash_set.elements hard_dependencies
