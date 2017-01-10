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








module E = Js_exp_make  
module S = Js_stmt_make

type module_id = Lam_module_ident.t

type ml_module_info = { 
  signatures : Types.signature ;
  cmj_table : Js_cmj_format.t
}

type env_value = 
  | Visit of ml_module_info
  | Runtime  of bool * Js_cmj_format.t
  (** A built in module probably from our runtime primitives, 
      so it does not have any [signature]
  *)
  | External  
  (** Also a js file, but this belong to third party 
  *)

type module_info = {
  signature :  Types.signature ;
  pure : bool 
}

type primitive_description =  Primitive.description

type key = 
  Ident.t * Env.t  * bool (** we need register which global variable is an dependency *)


type ident_info = {
  id : Ident.t;
  name : string;
  signatures : Types.signature;
  arity : Lam.function_arities; 
  closed_lambda : Lam.t option 
}

(*
   refer: [Env.find_pers_struct]
   [ find_in_path_uncap !load_path (name ^ ".cmi")]
*)



let cached_tbl  = Lam_module_ident.Hash.create 31

(* For each compilation we need reset to make it re-entrant *)
let reset () = 
  Translmod.reset ();
  Lam_module_ident.Hash.clear cached_tbl 

(* FIXME: JS external instead *)
let add_js_module ?id module_name : Ident.t 
  = 
  let id = 
    match id with
    | None -> Ext_ident.create_js_module module_name 
    | Some id -> id in
   let lam_module_ident = 
     Lam_module_ident.of_external id module_name in  
   match Lam_module_ident.Hash.find_key_opt cached_tbl lam_module_ident with   
   | None -> 
    Lam_module_ident.Hash.add 
     cached_tbl 
     lam_module_ident
     External;
     id
   | Some old_key -> old_key.id 
      



let add_cached_tbl = Lam_module_ident.Hash.add cached_tbl

let find_and_add_if_not_exist (id, pos) env ~not_found ~found =
  let oid  = Lam_module_ident.of_ml id in
  begin match Lam_module_ident.Hash.find_opt cached_tbl oid with 
    | None -> 
      let cmj_table = Config_util.find_cmj (id.name ^ Js_config.cmj_ext) in
      begin match
          Type_util.find_serializable_signatures_by_path
            ( id) env with 
      | None -> not_found id 
      | Some signature -> 
        add_cached_tbl oid (Visit {signatures = signature; 
                                   cmj_table ;  } ) ;
        let name =  (Type_util.get_name signature pos ) in
        let arity, closed_lambda =        
          begin match String_map.find_opt name cmj_table.values with
            | Some {arity; closed_lambda} -> arity, closed_lambda
            | None -> NA, None 
          end in
        found {id; 
               name ;
               signatures = signature ;
               arity ;
               closed_lambda = 
                 if Js_config.get_cross_module_inline () then
                   closed_lambda
                 else None
              }
      end
    | Some (Visit { signatures = serializable_sigs ; cmj_table = { values ; _} } ) -> 
      let name = (Type_util.get_name serializable_sigs pos ) in
      let arity , closed_lambda =  (
        match  String_map.find_opt name values with
        | Some {arity; closed_lambda;_} -> 
          arity, closed_lambda 
        | None -> (NA, None)
      ) in
      found { id;
              name; 
              signatures = serializable_sigs;
              arity;
              closed_lambda = 
                if Js_config.get_cross_module_inline () then
                  closed_lambda
                else None
                (* TODO shall we cache the arity ?*) 
            } 
    | Some (Runtime _) -> assert false
    | Some External  -> assert false
  end


(* TODO: it does not make sense to cache
   [Runtime] 
   and [externals]*)
type _ t = 
  | No_env :  Js_cmj_format.t t 
  | Has_env : Env.t  -> module_info t 


let query_and_add_if_not_exist (type u)
    (oid : Lam_module_ident.t) 
    (env : u t) ~not_found ~found:(found : u -> _) =
  match Lam_module_ident.Hash.find_opt cached_tbl oid with 
  | None -> 
    begin match oid.kind with
      | Runtime  -> 
        let cmj_table = 
          Config_util.find_cmj (Lam_module_ident.name oid ^ Js_config.cmj_ext) in           
        add_cached_tbl oid (Runtime (true,cmj_table)) ; 
        begin match env with 
          | Has_env _ -> 
            found {signature = []; pure = true}
          | No_env -> 
            found cmj_table
        end
      | Ml 
        -> 
        let cmj_table = 
          Config_util.find_cmj (Lam_module_ident.name oid ^ Js_config.cmj_ext) in           
        begin match env with 
          | Has_env env -> 
            begin match 
                Type_util.find_serializable_signatures_by_path ( oid.id) env with 
            | None -> not_found () (* actually when [not_found] in the call site, we throw... *)
            | Some signature -> 
              add_cached_tbl oid (Visit {signatures = signature; cmj_table }) ;
              found  { signature ; pure = cmj_table.effect = None} 
            end
          | No_env -> 
            found cmj_table
        end

      | External _  -> 
        add_cached_tbl oid External;
        (** This might be wrong, if we happen to expand  an js module
            we should assert false (but this in general should not happen)
        *)
        begin match env with 
          | Has_env _ 
            -> 
            found {signature = []; pure = false}
          | No_env -> 
            found (Js_cmj_format.no_pure_dummy)
        end

    end
  | Some (Visit {signatures  ; cmj_table =  cmj_table; _}) -> 
    begin match env with 
      | Has_env _ -> 
        found   { signature =  signatures  ; pure = (cmj_table.effect = None)} 
      | No_env  -> found cmj_table
    end

  | Some (Runtime (pure, cmj_table)) -> 
    begin match env with 
      | Has_env _ -> 
        found {signature = []  ; pure }
      | No_env -> 
        found cmj_table
    end
  | Some External -> 
    begin match env with 
      | Has_env _ -> 
        found {signature = []  ; pure  = false}
      | No_env -> found Js_cmj_format.no_pure_dummy
    end

(* Conservative interface *)
let is_pure id  = 
  query_and_add_if_not_exist id No_env
    ~not_found:(fun _ -> false) 
    ~found:(fun x -> x.effect = None)




let get_package_path_from_cmj module_system ( id : Lam_module_ident.t) = 
  query_and_add_if_not_exist id No_env
    ~not_found:(fun _ -> `NotFound) 
    ~found:(fun x -> Js_config.query_package_infos x.npm_package_path module_system)


(* TODO: [env] is not hard dependency *)

let get_requried_modules env
    (extras : module_id list ) 
    (hard_dependencies 
     : _ Hash_set_poly.t) : module_id list =  

  let mem (x : Lam_module_ident.t) = 
    not (is_pure x ) || Hash_set_poly.mem hard_dependencies  x 
  in
  Lam_module_ident.Hash.iter (fun (id : module_id)  _  ->
      if mem id 
      then Hash_set_poly.add hard_dependencies id) cached_tbl ;
  List.iter (fun id -> 
      if mem id 
      then Hash_set_poly.add hard_dependencies id
    ) extras;
  Hash_set_poly.elements hard_dependencies
