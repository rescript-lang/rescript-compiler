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
    let ty =   
      Ext_list.fold_right (fun (label_declaration : Parsetree.label_declaration) acc -> 
          Typ.arrow
            label_declaration.pld_name.txt label_declaration.pld_type acc 
        ) label_declarations  core_type in 

    let maker =   
      Val.mk  {loc; txt = name}
        ~attrs:[Ast_attributes.bs_obj]
        ~prim:[""] ty in 
    let setter_accessor =     
      Ext_list.fold_right (fun (x: Parsetree.label_declaration) acc -> 
          let pld_name = x.pld_name.txt in 
          let pld_loc = x.pld_name.loc in 
          let pld_type = x.pld_type in
          let () = checkNotFunciton pld_type in 
          let setter = 
            Val.mk 
              {loc = pld_loc; txt = pld_name}
              ~attrs:[Ast_attributes.bs_get]
              ~prim:[pld_name]
              (Typ.arrow "" core_type pld_type) :: acc in 
          match x.pld_mutable with 
          | Mutable -> 
            Val.mk 
              {loc = pld_loc; txt = pld_name ^ "Set"}
              ~attrs:[Ast_attributes.bs_set]
              ~prim:[pld_name]
              (Typ.arrow "" core_type (Typ.arrow "" pld_type (Ast_literal.type_unit ()))) :: setter
          | Immutable -> setter 
        ) label_declarations []
    in 
    newTdcl, 
    (maker :: setter_accessor)

  | Ptype_abstract 
  | Ptype_variant _ 
  | Ptype_open -> 
    (* Looks obvious that it does not make sense to warn *)
    (* U.notApplicable tdcl.ptype_loc derivingName;  *)
    tdcl, []

let handleTdcls tdcls =     
  let tdcls, code = 
    List.fold_right (fun tdcl (tdcls, sts)  -> 
        match handleTdcl tdcl with 
          ntdcl, value_descriptions -> 
          ntdcl::tdcls, 
          Ext_list.map_append (fun x -> Str.primitive x) value_descriptions sts

      ) tdcls ([],[])  in 
  Str.type_ tdcls :: code  
(* still need perform transformation for non-abstract type*)

