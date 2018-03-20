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


let handlePrimitiveInSig
    (self : Bs_ast_mapper.mapper)
    ({pval_attributes;
      pval_type;
      pval_loc;
      pval_prim;
      pval_name ;
     } as prim : Parsetree.value_description)
    (sigi : Parsetree.signature_item)
  : Parsetree.signature_item
  =
  let pval_type = self.typ self pval_type in
  let pval_attributes = self.attributes self pval_attributes in
  let pval_type, pval_prim, pval_attributes =
    match pval_prim with
    | [ v ] ->
      External_process.handle_attributes_as_string
        pval_loc
        pval_name.txt
        pval_type
        pval_attributes v
    | _ ->
      Location.raise_errorf
        ~loc:pval_loc
        "only a single string is allowed in bs external" in
  {sigi with
   psig_desc =
     Psig_value
       {prim with
        pval_type ;
        pval_prim ;
        pval_attributes
       }}

let handlePrimitiveInStru
    (self : Bs_ast_mapper.mapper)
    ({pval_attributes;
      pval_prim;
      pval_type;
      pval_name;
      pval_loc} as prim : Parsetree.value_description)
    (str : Parsetree.structure_item)
    : Parsetree.structure_item =
  let pval_type = self.typ self pval_type in
  let pval_attributes = self.attributes self pval_attributes in
  let pval_type, pval_prim, pval_attributes =
    match pval_prim with
    | [ v] ->
      External_process.handle_attributes_as_string
        pval_loc
        pval_name.txt
        pval_type pval_attributes v

    | _ -> Location.raise_errorf
             ~loc:pval_loc "only a single string is allowed in bs external" in
  {str with
   pstr_desc =
     Pstr_primitive
       {prim with
        pval_type ;
        pval_prim;
        pval_attributes
       }}
