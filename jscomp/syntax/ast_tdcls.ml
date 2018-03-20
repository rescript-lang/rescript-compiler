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
    let newTdcls = newTdcls tdcls newAttrs in
    let newSigi =
      self.signature_item self {sigi with psig_desc = Psig_type newTdcls} in
    if Ast_payload.isAbstract actions then
      let  codes = Ast_derive_abstract.handleTdclsInSig newTdcls in
      Ast_signature.fuseAll ~loc
        (
          Sig.include_ ~loc
            (Incl.mk ~loc
               (Mty.typeof_ ~loc
                  (Mod.constraint_ ~loc
                     (Mod.structure ~loc [
                         { pstr_loc = loc;
                           pstr_desc =
                             Pstr_type
                               (match newSigi.psig_desc with
                                | Psig_type x -> x
                                | _ -> assert false)
                         }] )
                     (Mty.signature ~loc [])) ) )
          ::
          self.signature self
            codes
        )
    else
      Ast_signature.fuseAll ~loc
        (newSigi::
         self.signature
           self
           (
             Ast_derive.gen_signature tdcls actions explict_nonrec))
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
    let tdcls2 = newTdcls tdcls newAttrs in
    let newStr =
      self.structure_item self
        {str with pstr_desc = Pstr_type tdcls2} in
    if Ast_payload.isAbstract actions then
      let codes = Ast_derive_abstract.handleTdclsInStr tdcls2 in
      (* use [tdcls2] avoid nonterminating *)
      Ast_structure.fuseAll ~loc
        (
          Ast_structure.constraint_ ~loc
            [newStr] []::
          self.structure self
            codes)
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


