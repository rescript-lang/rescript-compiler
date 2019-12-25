(* Copyright (C) 2019- Authors of BuckleScript
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


type mapper = Bs_ast_mapper.mapper 

let default_expr_mapper = Bs_ast_mapper.default_mapper.expr
let default_typ_mapper = Bs_ast_mapper.default_mapper.typ
let default_str_mapper = Bs_ast_mapper.default_mapper.structure_item
let default_sig_mapper = Bs_ast_mapper.default_mapper.signature_item

let expr_mapper (self : mapper) ( e : Parsetree.expression) = 
  match e.pexp_desc with 
  | Pexp_apply(fn, args) -> 
    Ast_exp_apply.app_exp_mapper e self fn args 
  | Pexp_constant (
    Pconst_string
    (s, (Some delim)))
  ->
    Ast_utf8_string_interp.transform e s delim
  | Pexp_fun (arg_label, _, pat , body)
    when Ast_compatible.is_arg_label_simple arg_label ->
    begin match Ext_list.exclude_with_val
          e.pexp_attributes
          Ast_attributes.is_bs with
      | None -> default_expr_mapper self e
      | Some pexp_attributes -> default_expr_mapper self {e with pexp_attributes = pexp_attributes}
    end
  | _  -> default_expr_mapper self e

let typ_mapper (self : mapper) (typ : Parsetree.core_type) =
  match typ with
  | {ptyp_attributes ;
     ptyp_desc = Ptyp_arrow (label, args, body);
     ptyp_loc = loc
    } ->
    begin match Ext_list.exclude_with_val
          ptyp_attributes
          Ast_attributes.is_bs with
      | None -> default_typ_mapper self typ
      | Some ptyp_attributes -> default_typ_mapper self {typ with ptyp_attributes = ptyp_attributes}
    end
  | _ -> default_typ_mapper self typ

let structure_item_mapper (self : mapper) (str : Parsetree.structure_item) =
  match str.pstr_desc with
  | Pstr_type (
      _rf,
    (_ :: _ as tdcls )) ->
      Ast_tdcls.handleTdclsInStru self str tdcls
  | _ -> default_str_mapper self str

let signature_item_mapper (self : mapper) (sigi : Parsetree.signature_item) =
  match sigi.psig_desc with
  | Psig_type (
      _rf,
       (_ :: _ as tdcls)) ->  (*FIXME: check recursive handling*)
      Ast_tdcls.handleTdclsInSigi self sigi tdcls
  | _ -> default_sig_mapper self sigi

let my_mapper : mapper = {
  Bs_ast_mapper.default_mapper with 
  expr = expr_mapper;
  typ = typ_mapper;
  signature_item =  signature_item_mapper;
  structure_item = structure_item_mapper;
}

let () = 
  Ppx_driver.main  
    (fun x -> my_mapper.structure my_mapper x)
    (fun x -> my_mapper.signature my_mapper x)