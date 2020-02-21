(* Copyright (C) 2020 Authors of BuckleScript
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


type action_table = 
  (Parsetree.expression option -> unit) Map_string.t
(** global configurations below *)
let common_actions_table :
  (string *  (Parsetree.expression option -> unit)) list =
  [
  ]


let structural_config_table : action_table =
  Map_string.of_list
    (( "no_export" ,
       (fun x ->
          Js_config.no_export := (
            match x with
            |Some e -> Ast_payload.assert_bool_lit e
            | None -> true)
       ))
     :: common_actions_table)

let signature_config_table : action_table =
  Map_string.of_list common_actions_table


let rec iter_on_bs_config_stru (x :Parsetree.structure) =   
  match x with 
  | [] -> () 
  | {pstr_desc = Pstr_attribute (({txt = "bs.config"; loc}, payload) as attr)}::_ -> 
    Bs_ast_invariant.mark_used_bs_attribute attr;
    Ext_list.iter (Ast_payload.ident_or_record_as_config loc payload)
      (Ast_payload.table_dispatch structural_config_table)
  | {pstr_desc = Pstr_attribute _} :: rest -> 
    iter_on_bs_config_stru rest 
  | non_attr :: _ -> ()    

let rec iter_on_bs_config_sigi (x :Parsetree.signature) =   
  match x with 
  | [] -> () 
  | {psig_desc = Psig_attribute (({txt = "bs.config"; loc}, payload) as attr)}::_ -> 
    Bs_ast_invariant.mark_used_bs_attribute attr;
    Ext_list.iter (Ast_payload.ident_or_record_as_config loc payload)
      (Ast_payload.table_dispatch signature_config_table)
  | {psig_desc = Psig_attribute _} :: rest -> 
    iter_on_bs_config_sigi rest 
  | non_attr :: _ -> ()      