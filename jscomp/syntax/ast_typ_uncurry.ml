(* Copyright (C) 2020 Authors of BuckleScript
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



let method_id  = 
  Ast_literal.Lid.js_meth

let method_call_back_id  = 
  Ast_literal.Lid.js_meth_callback

type typ = Parsetree.core_type

type 'a cxt = Ast_helper.loc -> Bs_ast_mapper.mapper -> 'a

type uncurry_type_gen = 
  (Asttypes.arg_label ->
   typ ->
   typ  ->
   typ) cxt

module Typ = Ast_helper.Typ
let generic_lift txt loc (args : typ list) (result : typ) = 
  let mk_args loc (n : int) (tys : typ list) : typ = 
    Typ.variant ~loc 
      [ Rtag (
            {loc; txt = "Arity_" ^ string_of_int n}
            ,
            [], (n = 0),  tys)] Closed None
  in  
  let xs =
    match args with 
    | [ ] -> [mk_args loc 0   [] ; result ]
    | [ x ] -> [ mk_args loc 1 [x] ; result ] 
    | _ -> 
      [mk_args loc (List.length args ) [Typ.tuple ~loc args] ; result ]
  in 
  Typ.constr ~loc {txt ; loc} xs


let lift_method_type loc  args_type result_type = 
  generic_lift  method_id loc args_type result_type

let lift_js_method_callback loc args_type result_type
  = 
  generic_lift method_call_back_id loc args_type result_type
 

let to_method_callback_type  loc (mapper : Bs_ast_mapper.mapper) (label : Asttypes.arg_label)
    (first_arg : Parsetree.core_type) 
    (typ : Parsetree.core_type)  =
  Bs_syntaxerr.err_if_label loc label;

  let rec aux (acc : typ list) (typ : typ) : typ * typ list = 
    (* in general, 
       we should collect [typ] in [int -> typ] before transformation, 
       however: when attributes [bs] and [bs.this] found in typ, 
       we should stop 
    *)
    match Ast_attributes.process_attributes_rev typ.ptyp_attributes with 
    | Nothing, _   -> 
      begin match typ.ptyp_desc with 
        | Ptyp_arrow (label, arg, body)
          -> 
          Bs_syntaxerr.err_if_label typ.ptyp_loc label;
          aux (mapper.typ mapper arg :: acc) body 
        | _ -> mapper.typ mapper typ, acc 
      end
    | _, _ -> mapper.typ mapper typ, acc  
  in 
  let first_arg = mapper.typ mapper first_arg in
  let result, rev_extra_args = aux  [first_arg] typ in 
  let args  = List.rev rev_extra_args in 
  (* let filter_args (args : typ list)  =  
    match args with 
    | [{ptyp_desc = 
          (Ptyp_constr ({txt = Lident "unit"}, []) 
          )}]
      -> []
    | _ -> args in *)
lift_js_method_callback loc args result 


let to_method_type loc 
  (mapper : Bs_ast_mapper.mapper) 
  (label : Asttypes.arg_label )
  (first_arg : Parsetree.core_type) (typ : Parsetree.core_type) = 
  Bs_syntaxerr.err_if_label loc label;

  let rec aux (acc : typ list) (typ : typ) : typ * typ list = 
    (* in general, 
       we should collect [typ] in [int -> typ] before transformation, 
       however: when attributes [bs] and [bs.this] found in typ, 
       we should stop 
    *)
    match Ast_attributes.process_attributes_rev typ.ptyp_attributes with 
    | Nothing, _   -> 
      begin match typ.ptyp_desc with 
        | Ptyp_arrow (label, arg, body)
          -> 
          Bs_syntaxerr.err_if_label typ.ptyp_loc label;
          aux (mapper.typ mapper arg :: acc) body 
        | _ -> mapper.typ mapper typ, acc 
      end
    | _, _ -> mapper.typ mapper typ, acc  
  in 
  let first_arg = mapper.typ mapper first_arg in
  let result, rev_extra_args = aux  [first_arg] typ in 
  let args  = List.rev rev_extra_args in 
  let filter_args (args : typ list)  =  
    match args with 
    | [{ptyp_desc = 
          (Ptyp_constr ({txt = Lident "unit"}, []) 
          )}]
      -> []
    | _ -> args in
    let args = filter_args args in
    lift_method_type loc args result 


let to_uncurry_type   loc (mapper : Bs_ast_mapper.mapper) (label : Asttypes.arg_label)
    (first_arg : Parsetree.core_type) 
    (typ : Parsetree.core_type)  =
  (* no need to error for optional here, 
     since we can not make it
     TODO: still error out for external? 
     Maybe no need to error on optional at all
     it just does not make sense
  *)
  let first_arg = mapper.typ mapper first_arg in
  let typ = mapper.typ mapper typ in 
  
  let fn_type = Typ.arrow ~loc label first_arg typ in 
  let arity = Ast_core_type.get_uncurry_arity fn_type in 
  match arity with 
  | Some 0
    -> 
    Typ.constr ({txt = Ldot (Ast_literal.Lid.js_fn, "arity0") ; loc }) [typ]
  | Some n  -> 
    Typ.constr ({txt = Ldot (Ast_literal.Lid.js_fn, "arity" ^ string_of_int n); loc })
      [fn_type]
  | None -> assert false  



