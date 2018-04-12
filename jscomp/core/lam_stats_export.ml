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
  = 
  List.fold_left
    (fun   acc (x : Ident.t)  ->

       let arity : Js_cmj_format.arity =
         match Ident_hashtbl.find_opt meta.ident_tbl x with 
         | Some (FunctionId {arity ; _}) -> Single arity 
         | Some (ImmutableBlock(elems,_)) ->  
           Submodule(elems |> Array.map (fun (x : Lam_id_kind.element) -> 
               match x with 
               | NA -> Lam_arity.NA
               | SimpleForm lam -> Lam_stats_util.get_arity  meta lam)
             )
         | Some _ 
         | None ->
          begin match Ident_map.find_opt x export_map with 
          | Some (Lprim {primitive = Pmakeblock (_,_, Immutable); args }) ->
            Submodule (args |> Ext_array.of_list_map (fun lam -> 
            Lam_stats_util.get_arity meta lam
            ))
          | Some _
          | None -> single_na
          end
         
         (*Single (Lam_stats_util.arity_of_var meta x) *)
       in
       let closed_lambda = 
         match Ident_map.find_opt x export_map with 
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
                   Ext_log.dwarn __LOC__ "%s recorded for inlining @." x.name ;
                   Some lambda
                 end
               else None
           else
             None
         | None
           -> None  in 
       String_map.add x.name  Js_cmj_format.({arity ; closed_lambda }) acc          
    )
    String_map.empty
    meta.exports 

let get_effect (meta : Lam_stats.t) maybe_pure external_ids = 
  match maybe_pure with
  | None ->  
    Ext_option.bind ( Ext_list.find_first_not 
                        (fun (id : Lam_module_ident.t) -> 
                           Lam_compile_env.query_and_add_if_not_exist id 
                             (Has_env meta.env )
                             ~not_found:(fun _ -> false ) ~found:(fun i -> 
                                 i.pure)
                        ) external_ids) (fun x -> Lam_module_ident.name x)
  | Some _ -> maybe_pure



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
    maybe_pure
    external_ids 
    export_map
    cmj_case
  : Js_cmj_format.t = 
  let values =  values_of_export meta export_map in
  let effect = get_effect meta maybe_pure external_ids in
  {values; 
   effect ; 
   npm_package_path = Js_packages_state.get_packages_info ();
   cmj_case ;
    (* FIXME: make sure [-o] would not change its case 
      add test for ns/non-ns
    *)
  }

