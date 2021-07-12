(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2015 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Parsetree
open Ast_iterator

let err = Syntaxerr.ill_formed_ast

let empty_record loc = err loc "Records cannot be empty."
let empty_variant loc = err loc "Variant types cannot be empty."
let invalid_tuple loc = err loc "Tuples must have at least 2 components."
let no_args loc = err loc "Function application with no argument."
let empty_let loc = err loc "Let with no bindings."
let empty_type loc = err loc "Type declarations cannot be empty."
let complex_id loc = err loc "Functor application not allowed here."

let simple_longident id =
  let rec is_simple = function
    | Longident.Lident _ -> true
    | Longident.Ldot (id, _) -> is_simple id
    | Longident.Lapply _ -> false
  in
  if not (is_simple id.txt) then complex_id id.loc

let iterator =
  let super = Ast_iterator.default_iterator in
  let type_declaration self td =
    super.type_declaration self td;
    let loc = td.ptype_loc in
    match td.ptype_kind with
    | Ptype_record [] -> empty_record loc
    | Ptype_variant [] -> empty_variant loc
    | _ -> ()
  in
  let typ self ty =
    super.typ self ty;
    let loc = ty.ptyp_loc in
    match ty.ptyp_desc with
    | Ptyp_tuple ([] | [_]) -> invalid_tuple loc
    | Ptyp_class (id, _) -> simple_longident id
    | Ptyp_package (_, cstrs) ->
      List.iter (fun (id, _) -> simple_longident id) cstrs
    | _ -> ()
  in
  let pat self pat =
    begin match pat.ppat_desc with
    | Ppat_construct (_, Some ({ppat_desc = Ppat_tuple _} as p))
      when Builtin_attributes.explicit_arity pat.ppat_attributes ->
        super.pat self p (* allow unary tuple, see GPR#523. *)
    | _ ->
        super.pat self pat
    end;
    let loc = pat.ppat_loc in
    match pat.ppat_desc with
    | Ppat_tuple ([] | [_]) -> invalid_tuple loc
    | Ppat_record ([], _) -> empty_record loc
    | Ppat_construct (id, _) -> simple_longident id
    | Ppat_record (fields, _) ->
      List.iter (fun (id, _) -> simple_longident id) fields
    | _ -> ()
  in
  let expr self exp =
    begin match exp.pexp_desc with
    | Pexp_construct (_, Some ({pexp_desc = Pexp_tuple _} as e))
      when Builtin_attributes.explicit_arity exp.pexp_attributes ->
        super.expr self e (* allow unary tuple, see GPR#523. *)
    | _ ->
        super.expr self exp
    end;
    let loc = exp.pexp_loc in
    match exp.pexp_desc with
    | Pexp_tuple ([] | [_]) -> invalid_tuple loc
    | Pexp_record ([], _) -> empty_record loc
    | Pexp_apply (_, []) -> no_args loc
    | Pexp_let (_, [], _) -> empty_let loc
    | Pexp_ident id
    | Pexp_construct (id, _)
    | Pexp_field (_, id)
    | Pexp_setfield (_, id, _)
    | Pexp_new id
    | Pexp_open (_, id, _) -> simple_longident id
    | Pexp_record (fields, _) ->
      List.iter (fun (id, _) -> simple_longident id) fields
    | _ -> ()
  in
  let extension_constructor self ec =
    super.extension_constructor self ec;
    match ec.pext_kind with
    | Pext_rebind id -> simple_longident id
    | _ -> ()
  in
  let class_expr self ce =
    super.class_expr self ce;
    let loc = ce.pcl_loc in
    match ce.pcl_desc with
    | Pcl_apply (_, []) -> no_args loc
    | Pcl_constr (id, _) -> simple_longident id
    | _ -> ()
  in
  let module_type self mty =
    super.module_type self mty;
    match mty.pmty_desc with
    | Pmty_alias id -> simple_longident id
    | _ -> ()
  in
  let open_description self opn =
    super.open_description self opn;
    simple_longident opn.popen_lid
  in
  let with_constraint self wc =
    super.with_constraint self wc;
    match wc with
    | Pwith_type (id, _)
    | Pwith_module (id, _) -> simple_longident id
    | _ -> ()
  in
  let module_expr self me =
    super.module_expr self me;
    match me.pmod_desc with
    | Pmod_ident id -> simple_longident id
    | _ -> ()
  in
  let structure_item self st =
    super.structure_item self st;
    let loc = st.pstr_loc in
    match st.pstr_desc with
    | Pstr_type (_, []) -> empty_type loc
    | Pstr_value (_, []) -> empty_let loc
    | _ -> ()
  in
  let signature_item self sg =
    super.signature_item self sg;
    let loc = sg.psig_loc in
    match sg.psig_desc with
    | Psig_type (_, []) -> empty_type loc
    | _ -> ()
  in
  { super with
    type_declaration
  ; typ
  ; pat
  ; expr
  ; extension_constructor
  ; class_expr
  ; module_expr
  ; module_type
  ; open_description
  ; with_constraint
  ; structure_item
  ; signature_item
  }

let structure st = iterator.structure iterator st
let signature sg = iterator.signature iterator sg
