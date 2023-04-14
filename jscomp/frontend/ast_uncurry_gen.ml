(* Copyright (C) 2020- Hongbo Zhang, Authors of ReScript
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

(* Handling `fun [@this]` used in `object [@bs] end` *)
let to_method_callback loc (self : Bs_ast_mapper.mapper) label
    (self_pat : Parsetree.pattern) body : Parsetree.expression_desc =
  let self_pat = self.pat self self_pat in
  (match Ast_pat.is_single_variable_pattern_conservative self_pat with
  | None -> Bs_syntaxerr.err self_pat.ppat_loc Bs_this_simple_pattern
  | Some self -> Stack.push self Js_config.self_stack);
  Bs_syntaxerr.optional_err loc label;
  let rec aux acc (body : Parsetree.expression) =
    match Ast_attributes.process_attributes_rev body.pexp_attributes with
    | Nothing, attrs -> (
      match body.pexp_desc with
      | Pexp_fun (arg_label, _, arg, body) ->
        Bs_syntaxerr.optional_err loc arg_label;
        aux ((arg_label, self.pat self arg, attrs) :: acc) body
      | _ -> (self.expr self body, acc))
    | _, _ -> (self.expr self body, acc)
  in
  let result, rev_extra_args = aux [(label, self_pat, [])] body in
  let body =
    Ext_list.fold_left rev_extra_args result (fun e (label, p, attrs) ->
        Ast_helper.Exp.fun_ ~loc ~attrs label None p e)
  in
  let arity = List.length rev_extra_args in
  let arity_s = string_of_int arity in
  Stack.pop Js_config.self_stack |> ignore;
  Parsetree.Pexp_apply
    ( Exp.ident ~loc
        {loc; txt = Ldot (Ast_literal.Lid.js_oo, "unsafe_to_method")},
      [
        ( Nolabel,
          Exp.constraint_ ~loc
            (Exp.record ~loc
               [({loc; txt = Ast_literal.Lid.hidden_field arity_s}, body)]
               None)
            (Typ.constr ~loc
               {
                 loc;
                 txt = Ldot (Ast_literal.Lid.js_meth_callback, "arity" ^ arity_s);
               }
               [Typ.any ~loc ()]) );
      ] )

let to_uncurry_fn (e : Parsetree.expression) (self : Bs_ast_mapper.mapper)
    (label : Asttypes.arg_label) pat body async : Parsetree.expression =
  let loc = e.pexp_loc in
  Bs_syntaxerr.optional_err loc label;
  let rec aux acc (body : Parsetree.expression) =
    match Ast_attributes.process_attributes_rev body.pexp_attributes with
    | Nothing, _ -> (
      match body.pexp_desc with
      | Pexp_fun (arg_label, _, arg, body) ->
        Bs_syntaxerr.optional_err loc arg_label;
        aux ((arg_label, self.pat self arg) :: acc) body
      | _ -> (self.expr self body, acc))
    | _, _ -> (self.expr self body, acc)
  in
  let first_arg = self.pat self pat in

  let result, rev_extra_args = aux [(label, first_arg)] body in
  let result = Ast_async.add_promise_type ~async result in
  let body =
    Ext_list.fold_left rev_extra_args result (fun e (label, p) ->
        Ast_helper.Exp.fun_ ~loc label None p e)
  in
  let body = Ast_async.add_async_attribute ~async body in

  let arity = List.length rev_extra_args in
  Bs_syntaxerr.err_large_arity loc arity;
  let fun_exp = Ast_uncurried.uncurriedFun ~loc ~arity body in
  {
    e with
    pexp_desc = fun_exp.pexp_desc;
    pexp_attributes = fun_exp.pexp_attributes @ e.pexp_attributes;
  }
