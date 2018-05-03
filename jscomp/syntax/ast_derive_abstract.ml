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



let get_optional_attrs =
  [Ast_attributes.bs_get; Ast_attributes.bs_return_undefined]
(** For this attributes, its type was wrapped as an option,
   so we can still reuse existing frame work
*)  

let get_attrs = [ Ast_attributes.bs_get_arity]
let set_attrs = [Ast_attributes.bs_set]
let handleTdcl (tdcl : Parsetree.type_declaration) =
  let core_type = U.core_type_of_type_declaration tdcl in
  let loc = tdcl.ptype_loc in
  let type_name = tdcl.ptype_name.txt in
  let newTdcl = {
    tdcl with
    ptype_kind = Ptype_abstract;
    ptype_attributes = [];
    (* avoid non-terminating*)
  } in
  match tdcl.ptype_kind with
  | Ptype_record label_declarations ->
    let is_private = tdcl.ptype_private = Private in
    let has_optional_field =
      List.exists (fun ({pld_type; pld_attributes} : Parsetree.label_declaration) ->
          Ast_attributes.has_bs_optional pld_attributes
        ) label_declarations in
    let setter_accessor, makeType, labels =
      Ext_list.fold_right
        (fun
          ({pld_name =
              {txt = label_name; loc = label_loc} as pld_name;
            pld_type;
            pld_mutable;
            pld_attributes;
            pld_loc
           }:
             Parsetree.label_declaration) (acc, maker, labels) ->
          let prim_as_name, newLabel =
            match Ast_attributes.iter_process_bs_string_as pld_attributes with
            | None ->
              label_name, pld_name
            | Some new_name ->
              new_name, {pld_name with txt = new_name}
          in
          let prim = [prim_as_name] in 
          let is_optional = Ast_attributes.has_bs_optional pld_attributes in
          let maker, getter_declaration =
            if is_optional then
              let optional_type = Ast_core_type.lift_option_type pld_type in
              (Ast_core_type.opt_arrow pld_loc label_name optional_type maker,
              Val.mk ~loc:pld_loc pld_name 
                ~attrs:get_optional_attrs ~prim
                (Typ.arrow ~loc "" core_type optional_type)
                )
            else
              Typ.arrow ~loc:pld_loc label_name pld_type maker,
              Val.mk ~loc:pld_loc pld_name ~attrs:get_attrs
              ~prim:(
                ["" ; (* Not needed actually*)
                External_ffi_types.to_string 
                (Ffi_bs (
                  [{arg_type = Nothing; arg_label = External_arg_spec.empty_label}],
                  Return_identity,
                  Js_get {js_get_name = prim_as_name; js_get_scopes = []}
                  ))] )
               (Typ.arrow ~loc "" core_type pld_type)
          in
          let acc =
           getter_declaration :: acc in
          let is_current_field_mutable = pld_mutable = Mutable in
          let acc =
            if is_current_field_mutable then
              let setter_type =
                (Typ.arrow "" core_type
                   (Typ.arrow ""
                      pld_type (* setter *)
                      (Ast_literal.type_unit ()))) in
              Val.mk ~loc:pld_loc
                {loc = label_loc; txt = label_name ^ "Set"}
                (* setter *)
                ~attrs:set_attrs
                ~prim setter_type
              :: acc
            else acc in
          acc,
          maker,
          (is_optional, newLabel)::labels
        ) label_declarations
        ([],
         (if has_optional_field then
            Typ.arrow ~loc "" (Ast_literal.type_unit ()) core_type
          else  core_type),
         [])
    in
    newTdcl,
    (if is_private then
       setter_accessor
     else
       let myPrims =
        External_process.pval_prim_of_option_labels
          labels
          has_optional_field
        in
       let myMaker =
         Val.mk  ~loc
           {loc; txt = type_name}
           ~prim:myPrims makeType in
       (myMaker :: setter_accessor))

  | Ptype_abstract
  | Ptype_variant _
  | Ptype_open ->
    (* Looks obvious that it does not make sense to warn *)
    (* U.notApplicable tdcl.ptype_loc derivingName;  *)
    tdcl, []

let handleTdclsInStr tdcls =
  let tdcls, code =
    List.fold_right (fun tdcl (tdcls, sts)  ->
        match handleTdcl tdcl with
          ntdcl, value_descriptions ->
          ntdcl::tdcls,
          Ext_list.map_append (fun x -> Str.primitive x) value_descriptions sts

      ) tdcls ([],[])  in
  Str.type_ tdcls :: code
(* still need perform transformation for non-abstract type*)

let handleTdclsInSig tdcls =
  let tdcls, code =
    List.fold_right (fun tdcl (tdcls, sts)  ->
        match handleTdcl tdcl with
          ntdcl, value_descriptions ->
          ntdcl::tdcls,
          Ext_list.map_append (fun x -> Sig.value x) value_descriptions sts

      ) tdcls ([],[])  in
  Sig.type_ tdcls :: code
