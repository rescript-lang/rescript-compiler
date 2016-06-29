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


val gen_fn_run : 
  Ast_helper.loc ->
  int ->
  Parsetree.expression ->
  Parsetree.expression list -> Parsetree.expression_desc

val gen_method_run : 
  Ast_helper.loc ->
  int ->
  Parsetree.expression ->
  Parsetree.expression list -> Parsetree.expression_desc

(** turn {[ fun [@uncurry] (x,y) -> x]} into an uncurried function 
    TODO: Future 
    {[ fun%bs this (a,b,c) -> 
    ]}

    [function] can only take one argument, that is the reason we did not adopt it
*)
val uncurry_fn_gen :
  Ast_helper.loc ->
  Parsetree.pattern ->
  Parsetree.expression -> Parsetree.expression_desc

val uncurry_method_gen : 
  Ast_helper.loc ->
  Parsetree.pattern -> 
  Parsetree.expression -> Parsetree.expression_desc




val destruct_arrow : 
  Ast_helper.loc ->
  Parsetree.core_type ->
  Parsetree.core_type -> Ast_mapper.mapper -> Parsetree.core_type


val destruct_tuple_pat : 
  Parsetree.pattern -> Parsetree.pattern list

val destruct_tuple_exp : 
  Parsetree.expression -> Parsetree.expression list

val lift_js_type : 
  loc:Ast_helper.loc -> Parsetree.core_type -> Parsetree.core_type

val find_uncurry_attrs_and_remove : 
  Parsetree.attributes ->
  Parsetree.attribute option * Parsetree.attribute list

val from_labels : loc:Ast_helper.loc -> Asttypes.label list -> Parsetree.core_type

val down_with_name : 
  loc:Ast_helper.loc ->
  Parsetree.expression -> string -> Parsetree.expression_desc

val handle_debugger : 
  Location.t -> Ast_payload.t -> Parsetree.expression_desc

val handle_raw : 
  Location.t -> Ast_payload.t -> Parsetree.expression
val handle_raw_structure : 
  Location.t -> Ast_payload.t -> Parsetree.structure_item
