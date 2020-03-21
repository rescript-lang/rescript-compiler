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


type typ = Parsetree.core_type

type 'a cxt = Ast_helper.loc -> Bs_ast_mapper.mapper -> 'a

type uncurry_type_gen = 
  (Asttypes.arg_label ->
   typ ->
   typ  ->
   typ) cxt

module Typ = Ast_helper.Typ



let to_method_callback_type  loc 
  (mapper : Bs_ast_mapper.mapper) (label : Asttypes.arg_label)
    (first_arg : Parsetree.core_type) 
    (typ : Parsetree.core_type)  =
  let first_arg = mapper.typ mapper first_arg in
  let typ = mapper.typ mapper typ in 
  let meth_type = Typ.arrow ~loc label first_arg typ in 
  let arity = Ast_core_type.get_uncurry_arity meth_type in 
  match arity with 
  | Some n ->  
    Typ.constr {
      txt = Ldot (Ast_literal.Lid.js_meth_callback,
                  "arity"^string_of_int n);
      loc
    } [meth_type]
  | None -> assert false


let self_type_lit = "self_type"   
let generate_method_type
    loc
    (mapper : Bs_ast_mapper.mapper)
    ?alias_type 
    method_name 
    lbl 
    pat 
    e
     : Parsetree.core_type =
  let arity = Ast_pat.arity_of_fun pat e in    
  let result = Typ.var ~loc method_name in   
  let self_type loc = Typ.var ~loc self_type_lit in 

  let self_type =
    let v = self_type loc  in
    match alias_type with 
    | None -> v 
    | Some ty -> Typ.alias ~loc ty self_type_lit
  in  
  if arity = 0 then
    to_method_callback_type loc mapper  Nolabel self_type result      
  else
    let tyvars =
      Ext_list.mapi (lbl :: Ast_pat.labels_of_fun e) (fun i x -> 
        x, Typ.var ~loc (method_name ^ string_of_int i)
        ) 
      (* Ext_list.init arity (fun i -> Typ.var ~loc (method_name ^ string_of_int i)) *)
    in
    match tyvars with
    | (label,x) :: rest ->
      let method_rest =
        Ext_list.fold_right rest result (fun (label,v) acc -> 
          Typ.arrow ~loc  label v acc)
      in         
      (to_method_callback_type loc mapper  Nolabel self_type
         (Typ.arrow ~loc  label x method_rest))
    | _ -> assert false

  

let to_method_type loc 
  (mapper : Bs_ast_mapper.mapper) 
  (label : Asttypes.arg_label )
  (first_arg : Parsetree.core_type) (typ : Parsetree.core_type) = 
  let first_arg = mapper.typ mapper first_arg in
  let typ = mapper.typ mapper typ in 
  let meth_type = Typ.arrow ~loc label first_arg typ in 
  let arity = Ast_core_type.get_uncurry_arity meth_type in 
  match arity with 
  | Some 0 -> 
    Typ.constr (
      {txt = 
         Ldot( Ast_literal.Lid.js_meth,"arity0");
       loc
      }) [typ]
  | Some n -> 
      Typ.constr (
        {txt = 
          Ldot(Ast_literal.Lid.js_meth,"arity" ^ string_of_int n);
          loc
      }) [meth_type]
  | None -> assert false

let generate_arg_type loc (mapper  : Bs_ast_mapper.mapper)
  method_name label pat body  : Ast_core_type.t = 
let arity = Ast_pat.arity_of_fun pat body in   
let result = Typ.var ~loc method_name in   
if arity = 0 then
  to_method_type loc mapper Nolabel (Ast_literal.type_unit ~loc ()) result 

else
  let tyvars =
    Ext_list.mapi (label::Ast_pat.labels_of_fun body) (fun i x -> 
     x, Typ.var ~loc (method_name ^ string_of_int i))
  in
  begin match tyvars with
    | (label,x) :: rest ->
      let method_rest =
        Ext_list.fold_right rest result (fun (label,v) acc -> 
          Typ.arrow ~loc label v acc)
      in         
      to_method_type loc mapper label x method_rest
    | _ -> assert false
  end     
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



