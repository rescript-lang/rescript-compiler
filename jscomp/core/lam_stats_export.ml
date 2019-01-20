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






let pp = Format.fprintf 
(* we should exclude meaninglist names and do the convert as well *)

(* let meaningless_names  = ["*opt*"; "param";] *)



let single_na = Js_cmj_format.single_na

let values_of_export 
  (meta : Lam_stats.t) 
  (export_map  : Lam.t Ident_map.t)
  : Js_cmj_format.cmj_value String_map.t 
  = 
  Ext_list.fold_left meta.exports  String_map.empty    
    (fun (x : Ident.t) acc   ->
       let arity : Js_cmj_format.arity =
         match Ident_hashtbl.find_opt meta.ident_tbl x with 
         | Some (FunctionId {arity ; _}) -> Single arity 
         | Some (ImmutableBlock(elems)) ->  
           Submodule(Ext_array.map elems (fun x -> 
               match x with 
               | NA -> Lam_arity.na
               | SimpleForm lam -> Lam_arity_analysis.get_arity  meta lam)
             )
         | Some _ 
         | None ->
           begin match Ident_map.find_opt export_map x with 
             | Some (Lprim {primitive = Pmakeblock (_,_, Immutable); args }) ->
               Submodule (Ext_array.of_list_map args (fun lam -> 
                   Lam_arity_analysis.get_arity meta lam))
             | Some _
             | None -> single_na
           end
       in
       let persistent_closed_lambda = 
         if not !Js_config.cross_module_inline then None
         else match Ident_map.find_opt export_map x with 
         | Some lambda  -> 
           if Lam_analysis.safe_to_inline lambda
           (* when inlning a non function, we have to be very careful,
              only truly immutable values can be inlined
           *)
           then
             if Lam_inline_util.should_be_functor x.name lambda (* can also be submodule *)
             then
               if Lam_closure.is_closed lambda (* TODO: seriealize more*)
               then Some lambda
               else None
             else 
               let lam_size = Lam_analysis.size lambda in
               (* TODO:
                  1. global need re-assocate when do the beta reduction 
                  2. [lambda_exports] is not precise
               *)
               let free_variables =
                 Lam_closure.free_variables Ident_set.empty Ident_map.empty lambda in
               if  lam_size < Lam_analysis.small_inline_size  && 
                   Ident_map.is_empty free_variables
               then 
                 begin
                   Ext_log.dwarn ~__POS__ "%s recorded for inlining @." x.name ;
                   Some lambda
                 end
               else None
           else
             None
         | None -> None  in 
       String_map.add  acc x.name  Js_cmj_format.{arity ; persistent_closed_lambda }
    )

(* ATTENTION: all runtime modules, if it is not hard required, 
  it should be okay to not reference it 
*)
let get_dependent_module_effect 
  (meta : Lam_stats.t) 
  (maybe_pure : string option) 
  (external_ids : Lam_module_ident.t list) = 
  if maybe_pure = None then
    let non_pure_module =  
      Ext_list.find_first_not external_ids
        (fun id -> 
           id.kind = Runtime ||
           Lam_compile_env.query_and_add_if_not_exist id 
             (Has_env meta.env )
             ~not_found:(fun _ -> false ) 
             ~found:(fun {pure} -> pure)
        ) in 
    Ext_option.map  non_pure_module (fun x -> Lam_module_ident.name x)
  else 
    maybe_pure



(* Note that 
   [lambda_exports] is 
   lambda expression to be exported
   for the js backend, we compile to js 
   for the inliner, we try to seriaize it -- 
   relies on other optimizations to make this happen
   {[
     exports.Make = function () {.....}
   ]}
   TODO: check that we don't do this in browser environment
*)
let export_to_cmj 
    (meta : Lam_stats.t ) 
    effect 
    export_map
    cmj_case
  : Js_cmj_format.t = 
  let values =  values_of_export meta export_map in
  
  Js_cmj_format.mk
    ~values
    ~effect 
    ~npm_package_path: (Js_packages_state.get_packages_info ())
   ~cmj_case 
    (* FIXME: make sure [-o] would not change its case 
      add test for ns/non-ns
    *)
  

