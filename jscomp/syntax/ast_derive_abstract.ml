(* Copyright (C) 2017 Authors of BuckleScript
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


let derivingName = "abstract"
module U = Ast_derive_util
open Ast_helper
type tdcls = Parsetree.type_declaration list 

let handle_config (config : Parsetree.expression option) = 
  match config with 
  | Some config -> 
    U.invalid_config config
  | None -> ()

let handleTdcl (tdcl : Parsetree.type_declaration) =   
  let core_type = U.core_type_of_type_declaration tdcl in 
  let loc = tdcl.ptype_loc in 
  let name = tdcl.ptype_name.txt in 
  let newTdcl = {
    tdcl with 
    ptype_kind = Ptype_abstract;
    ptype_attributes = [];
    (* avoid non-terminating*)
  } in 
  match tdcl.ptype_kind with 
  | Ptype_record label_declarations -> 

    let labels = 
      Ext_list.map (fun x -> x.Parsetree.pld_name) label_declarations in 
    let types =   
      Ext_list.map (fun x -> x.Parsetree.pld_type) label_declarations in 
    let ty =   
      Ext_list.fold_right2 (fun label typ acc -> 
          Typ.arrow
            label.Asttypes.txt typ acc 
        ) labels types core_type in 
    newTdcl, [Val.mk  {loc; txt = name}
                ~attrs:[Ast_attributes.bs_obj]
                ~prim:[""] ty
             ]
  | Ptype_abstract 
  | Ptype_variant _ 
  | Ptype_open -> 
    U.notApplicable tdcl.ptype_loc derivingName; 
    newTdcl, []

let handleTdcls tdcls =     
  let tdcls, code = 
    List.fold_right (fun tdcl (tdcls, sts)  -> 
        match handleTdcl tdcl with 
          ntdcl, value_descriptions -> 
          ntdcl::tdcls, 
          Ext_list.map_append (fun x -> Str.primitive x) value_descriptions sts

      ) tdcls ([],[])  in 
  Str.type_ tdcls, code 


let handleTdclsInSig (tdcls : Parsetree.type_declaration list) =   
  let tdcls, code = 
    List.fold_right (fun tdcl (tdcls, sts)  -> 
        match handleTdcl tdcl with 
          ntdcl, value_descriptions -> 
          ntdcl::tdcls, 
          Ext_list.map_append (fun x -> Sig.value x) value_descriptions sts

      ) tdcls ([],[])  in 
  Sig.type_ tdcls, code 
