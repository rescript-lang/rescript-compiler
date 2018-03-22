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

(* see #2337
   TODO: relax it to allow (int -> int [@bs])
*)
let rec checkNotFunciton (ty : Parsetree.core_type) =
  match ty.ptyp_desc with
  | Ptyp_poly (_,ty) -> checkNotFunciton ty
  | Ptyp_alias (ty,_) -> checkNotFunciton ty
  | Ptyp_arrow _ ->
    Location.raise_errorf
      ~loc:ty.ptyp_loc
      "syntactic function type is not allowed when working with abstract bs.deriving, create a named type as work around"
  | Ptyp_any
  | Ptyp_var _
  | Ptyp_tuple _
  | Ptyp_constr _
  | Ptyp_object _
  | Ptyp_class _
  | Ptyp_variant _
  | Ptyp_package _
  | Ptyp_extension _ -> ()


let get_optional_attrs =
  [Ast_attributes.bs_get; Ast_attributes.bs_return_undefined]
let get_attrs = [ Ast_attributes.bs_get ]
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
      List.exists (fun ({pld_type} : Parsetree.label_declaration) ->
          Ast_core_type.is_user_option pld_type
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
          let () = checkNotFunciton pld_type in
          (* TODO: explain why *)
          let prim, newLabel =
            match Ast_attributes.iter_process_bs_string_as pld_attributes with
            | None ->
              [label_name], pld_name
            | Some new_name ->
              [new_name], {pld_name with txt = new_name}
          in
          let is_option = Ast_core_type.is_user_option pld_type in
          let getter_type =
            Typ.arrow ~loc "" core_type pld_type in
          let acc =
            Val.mk pld_name
              ~attrs:(
                if is_option then get_optional_attrs
                else get_attrs)
              ~prim getter_type :: acc in
          let is_current_field_mutable = pld_mutable = Mutable in
          let acc =
            if is_current_field_mutable then
              let setter_type =
                (Typ.arrow "" core_type
                   (Typ.arrow ""
                      (if is_option then
                         Ast_core_type.extract_option_type_exn pld_type
                       else pld_type)
                      (Ast_literal.type_unit ()))) in
              Val.mk
                {loc = label_loc; txt = label_name ^ "Set"}
                (* setter *)
                ~attrs:set_attrs
                ~prim setter_type
              :: acc
            else acc in
          acc,
          (if  is_option then
             Ast_core_type.opt_arrow pld_loc label_name pld_type maker
           else Typ.arrow ~loc:pld_loc label_name pld_type maker
          ),
          (is_option, newLabel)::labels
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
