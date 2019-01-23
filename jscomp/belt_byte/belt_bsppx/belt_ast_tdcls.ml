(* Copyright (C) 2018 Authors of BuckleScript
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

open Ast_helper

(**
   [newTdcls tdcls newAttrs]
   functional update attributes of last declaration *)
let newTdcls
    (tdcls : Parsetree.type_declaration list)
    (newAttrs : Parsetree.attributes)
  : Parsetree.type_declaration list
  =
  match tdcls with
  | [ x ] ->
    [{ x with Parsetree.ptype_attributes = newAttrs}]
  | _ ->
    Ext_list.map_last
      (fun last x ->
         if last then
           { x with
             Parsetree.ptype_attributes = newAttrs}
         else x )
      tdcls

(* @perf this could be faster, by not being done alongside the rest.
  Right now we process the types a billion times for different things (also because I'm trying to 
  keep the codebase not too different from bucklescript).
  For native optional is boxed and records are actually backed by records, not external JS obj.
  Those two differences mean we need to do a bit more processing of the types and labels.
  
  For example we can't just make the type annotated with `bs.deriving abstract` actually abstract 
  in the same way. Inside the generated module the type needs to stay record, so we can generate a
  constructor function which can have as its body a record literal. 
  Since we have to keep the type like that, we then need to add a module signature to constrain the
  generated module (and turn the annotated type into an abstract type).
  We turn the type into an abstract type from the user's perspective because that allows us to use
  whatever underlying representation we want without affecting the interface exposed to the user.
  
                    ben - June 8th 2018
*)
let turn_bs_optional_into_optional (tdcls : Parsetree.type_declaration list) =
  List.map (fun tdcl -> match tdcl.Parsetree.ptype_kind with 
  | Ptype_record labels -> 
    {tdcl with ptype_kind = Ptype_record (List.map (fun ({Parsetree.pld_type; pld_loc; pld_attributes} as dcl : Parsetree.label_declaration) ->
          let has_optional_field = Belt_ast_attributes.has_bs_optional pld_attributes in
          if has_optional_field then 
          (* @Incomplete remove ALL attributes when we might want to only remove the bs.optional.
              
                     Ben - June 8th 2018
           *)
            { dcl with
              Parsetree.pld_type = {ptyp_desc =
               Ptyp_constr(
                 {txt = Lident "option";
                  loc = pld_loc}
                  , [pld_type]);
                  ptyp_loc = pld_loc;
                ptyp_attributes = []
              };
            }
          else dcl
        ) labels)}
  | _ -> tdcl) tdcls

let handleTdclsInSigi
    (self : Bs_ast_mapper.mapper)
    (sigi : Parsetree.signature_item)
    (tdcls : Parsetree.type_declaration list)
  : Ast_signature.item =
  begin match Belt_ast_attributes.process_derive_type
                (Ext_list.last tdcls).ptype_attributes  with
  | {bs_deriving = Some actions; explict_nonrec}, newAttrs
    ->
    let loc = sigi.psig_loc in
    let originalTdclsNewAttrs = newTdcls tdcls newAttrs in (* remove the processed attr*)
    let newTdclsNewAttrs = self.type_declaration_list self originalTdclsNewAttrs in
    if Ast_payload.isAbstract actions then
      let  codes = Belt_ast_derive_abstract.handleTdclsInSig originalTdclsNewAttrs in
      Ast_signature.fuseAll ~loc
        (
          Sig.include_ ~loc
            (Incl.mk ~loc
               (Mty.typeof_ ~loc
                  (Mod.constraint_ ~loc
                     (Mod.structure ~loc [
                         { pstr_loc = loc;
                           pstr_desc =
                             Pstr_type (turn_bs_optional_into_optional newTdclsNewAttrs)
                         }] )
                     (Mty.signature ~loc [])) ) )
          :: 
          (* include module type of struct [processed_code for checking like invariance ]end *)
          self.signature self  codes
        )
    else
      Ast_signature.fuseAll ~loc
        ( {psig_desc = Psig_type newTdclsNewAttrs; psig_loc = loc}::
         self.signature
           self
           (Ast_derive.gen_signature tdcls actions explict_nonrec))
  | {bs_deriving = None }, _  ->
    Bs_ast_mapper.default_mapper.signature_item self sigi

  end


let handleTdclsInStru
    (self : Bs_ast_mapper.mapper)
    (str : Parsetree.structure_item)
    (tdcls : Parsetree.type_declaration list)
  : Ast_structure.item =
  begin match
      Belt_ast_attributes.process_derive_type
        ((Ext_list.last tdcls).ptype_attributes) with
  | {bs_deriving = Some actions;
     explict_nonrec
    }, newAttrs ->
    let loc = str.pstr_loc in
    let originalTdclsNewAttrs = newTdcls tdcls newAttrs in
    let newStr : Parsetree.structure_item =
      { pstr_desc = Pstr_type (self.type_declaration_list self originalTdclsNewAttrs);
        pstr_loc = loc}
    in
    if Ast_payload.isAbstract actions then
      let (codes, codes_sig) = Belt_ast_derive_abstract.handleTdclsInStr originalTdclsNewAttrs in
      (* the codes_sig will hide the implementation of the type that is a record. *)
      Ast_structure.constraint_ ~loc
        (self.structure self codes)
        (self.signature self codes_sig)
    else
      Ast_structure.fuseAll ~loc
        (newStr ::
         self.structure self
           (
             List.map
               (fun action ->
                  Ast_derive.gen_structure_signature
                    loc
                    tdcls action explict_nonrec
               )    actions
           ))
  | {bs_deriving = None }, _  ->
    Bs_ast_mapper.default_mapper.structure_item self str
  end


