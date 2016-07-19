(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

val type_deriving_structure: 
  Parsetree.type_declaration ->
  Ast_payload.action list ->
  bool -> 
  Ast_structure.t
val type_deriving_signature: 
  Parsetree.type_declaration ->
  Ast_payload.action list -> 
  bool -> 
  Ast_signature.t

type gen = {
  structure_gen : Parsetree.type_declaration -> bool -> Ast_structure.t ;
  signature_gen : Parsetree.type_declaration -> bool -> Ast_signature.t ; 
  expression_gen : (Parsetree.core_type -> Parsetree.expression) ; 
}

val derive_table: (Parsetree.expression option -> gen) String_map.t
