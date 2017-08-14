(* Copyright (C) 2017- Authors of BuckleScript
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


(**
   {[
     module List = XX__List    
   ]}
   vs
   {[
     module List = List__XX
   ]}
*)
open Ast_helper
let loc = Location.none
(* module pkg_name = pkg_name-cunit *)
let make_structure_item pkg_name cunit : Parsetree.structure_item =
  Str.module_ 
    (Mb.mk {txt = cunit; loc }
       (Mod.ident 
          {txt = Lident 
               (Bsb_package_name.make ~pkg:pkg_name cunit)
          ; loc}))

let make_signature_item pkg_name cunit : Parsetree.signature_item = 
  Sig.module_
    (Md.mk {txt = cunit; loc}
       (Mty.alias 
          {txt = Lident 
            (Bsb_package_name.make ~pkg:pkg_name cunit)
          ; loc})
    )        

let make_structure pkg_name cunits : Parsetree.structure =     
  cunits |> List.map (make_structure_item pkg_name)

let make_signature pkg_name cunits  : Parsetree.signature = 
  cunits |> List.map (make_signature_item pkg_name)  