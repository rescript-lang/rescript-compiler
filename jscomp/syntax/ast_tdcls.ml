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
    Ext_list.map_last tdcls
      (fun last x ->
         if last then
           { x with
             Parsetree.ptype_attributes = newAttrs}
         else x )
      

#if BS_NATIVE then

let turn_bs_optional_into_optional (tdcls : Parsetree.type_declaration list) =
   List.map (fun tdcl -> match tdcl.Parsetree.ptype_kind with
   | Ptype_record labels ->
     {tdcl with ptype_kind = Ptype_record (List.map (fun ({Parsetree.pld_type; pld_loc; pld_attributes} as dcl : Parsetree.label_declaration) ->
           let has_optional_field = Ast_attributes.has_bs_optional pld_attributes in
           if has_optional_field then
             { dcl with
               Parsetree.pld_type = {dcl.pld_type with ptyp_desc =
                Ptyp_constr(
                  {txt = Lident "option";
                   loc = pld_loc}
                   , [pld_type]);
                   ptyp_loc = pld_loc;
               };
               pld_attributes = Ext_list.exclude pld_attributes (fun x -> (Ast_attributes.is_optional x) || (Ast_attributes.is_bs_as x))
             }
           else dcl
         ) labels)}
   | _ -> tdcl) tdcls

#end


let handleTdclsInSigi
    (self : Bs_ast_mapper.mapper)
    (sigi : Parsetree.signature_item)
    (tdcls : Parsetree.type_declaration list)
  : Ast_signature.item =
  begin match Ast_attributes.process_derive_type
                (Ext_list.last tdcls).ptype_attributes  with
  | {bs_deriving = Some actions; explict_nonrec}, newAttrs
    ->
    let loc = sigi.psig_loc in
    let originalTdclsNewAttrs = newTdcls tdcls newAttrs in (* remove the processed attr*)
    let newTdclsNewAttrs = self.type_declaration_list self originalTdclsNewAttrs in
    let kind = Ast_derive_abstract.isAbstract actions in
    if kind <> Not_abstract then
#if BS_NATIVE then
      let  codes = Native_ast_derive_abstract.handleTdclsInSig ~light:(kind = Light_abstract) originalTdclsNewAttrs in
#else
      let  codes = Ast_derive_abstract.handleTdclsInSig ~light:(kind = Light_abstract) originalTdclsNewAttrs in
#end
      Ast_signature.fuseAll ~loc
        (
          Sig.include_ ~loc
            (Incl.mk ~loc
               (Mty.typeof_ ~loc
                  (Mod.constraint_ ~loc
                     (Mod.structure ~loc [
#if BS_NATIVE then
                         Ast_compatible.rec_type_str ~loc (turn_bs_optional_into_optional newTdclsNewAttrs)
#else
                         Ast_compatible.rec_type_str ~loc newTdclsNewAttrs
#end
                         ] )
                     (Mty.signature ~loc [])) ) )
          :: (* include module type of struct [processed_code for checking like invariance ]end *)
          self.signature self  codes
        )
    else
      Ast_signature.fuseAll ~loc
        ( 
          Ast_compatible.rec_type_sig ~loc newTdclsNewAttrs
        ::
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
      Ast_attributes.process_derive_type
        ((Ext_list.last tdcls).ptype_attributes) with
  | {bs_deriving = Some actions;
     explict_nonrec
    }, newAttrs ->
    let loc = str.pstr_loc in
    let originalTdclsNewAttrs = newTdcls tdcls newAttrs in
    let newStr : Parsetree.structure_item =
      Ast_compatible.rec_type_str ~loc (self.type_declaration_list self originalTdclsNewAttrs)
    in
    let kind = Ast_derive_abstract.isAbstract actions in 
    if kind <> Not_abstract then
#if BS_NATIVE then
      let (codes, codes_sig) = Native_ast_derive_abstract.handleTdclsInStr ~light:(kind = Light_abstract) originalTdclsNewAttrs in
       (* the codes_sig will hide the implementation of the type that is a record. *)
       Ast_structure.constraint_ ~loc
         (self.structure self codes)
         (self.signature self codes_sig)
#else
      let codes =
          Ast_derive_abstract.handleTdclsInStr ~light:(kind = Light_abstract) originalTdclsNewAttrs in
      (* use [tdcls2] avoid nonterminating *)
      Ast_structure.fuseAll ~loc
        (
          Ast_structure.constraint_ ~loc [newStr] []
          :: (* [include struct end : sig end] for error checking *)
          self.structure self codes)
#end

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


