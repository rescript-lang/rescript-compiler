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

open Ast_helper 
type 'a cxt = Ast_helper.loc -> Ast_mapper.mapper -> 'a
type loc = Location.t 
type args = (string * Parsetree.expression) list
type label_exprs = (Longident.t Asttypes.loc * Parsetree.expression) list

let js_obj_type_id () = 
  if Js_config.get_env () = Browser then
    Ast_literal.Lid.pervasives_js_obj
  else Ast_literal.Lid.js_obj 
    
let uncurry_type_id () = 
  if Js_config.get_env () = Browser then 
     Ast_literal.Lid.pervasives_fn
  else 
    Ast_literal.Lid.js_fn

let method_id () = 
  if Js_config.get_env () = Browser then 
     Ast_literal.Lid.pervasives_meth
  else 
    Ast_literal.Lid.js_meth

let method_call_back_id () = 
  if Js_config.get_env () = Browser then 
    Ast_literal.Lid.pervasives_meth_callback
  else 
    Ast_literal.Lid.js_meth_callback
let arity_lit = "Arity_"

let mk_args loc n tys = 
  Typ.variant ~loc 
    [ Rtag (arity_lit ^ string_of_int n, [], (n = 0),  tys)] Closed None

let generic_lift txt loc args result  = 
  let xs =
    match args with 
    | [ ] -> [mk_args loc 0   [] ; result ]
    | [ x ] -> [ mk_args loc 1 [x] ; result ] 
    | _ -> 
      [mk_args loc (List.length args ) [Typ.tuple ~loc args] ; result ]
  in 
  Typ.constr ~loc {txt ; loc} xs

let lift_curry_type  loc   = 
  generic_lift  ( uncurry_type_id ()) loc

let lift_method_type loc  = 
  generic_lift  (method_id ()) loc

let lift_js_method_callback loc
  = 
  generic_lift (method_call_back_id ()) loc 
(** Note that currently there is no way to consume [Js.meth_callback]
    so it is fine to encode it with a freedom, 
    but we need make it better for error message.
    - all are encoded as 
    {[ 
      type fn =  (`Args_n of _ , 'result ) Js.fn
      type method = (`Args_n of _, 'result) Js.method
      type method_callback = (`Args_n of _, 'result) Js.method_callback
    ]}
    For [method_callback], the arity is never zero, so both [method] 
    and  [fn] requires (unit -> 'a) to encode arity zero
*)


let to_js_type loc  x  = 
  Typ.constr ~loc {txt = js_obj_type_id (); loc} [x]



let arrow = Typ.arrow




let js_property loc obj name =
  let downgrade ~loc () = 
    let var = Typ.var ~loc "a" in 
    Ast_comb.arrow_no_label ~loc
      (to_js_type loc var) var
  in
  Ast_comb.local_extern_cont loc  
    ~pval_prim:["js_unsafe_downgrade"] 
    ~pval_type:(downgrade ~loc ())
    ~local_fun_name:"cast" 
    (fun down -> Exp.send ~loc (Exp.apply ~loc down ["", obj]) name  )

let generic_run ty loc arity fn args : Parsetree.expression_desc = 
  let fn_type, args_type, result_type = Ast_comb.tuple_type_pair ~loc `Run arity  in 
  let string_arity = string_of_int arity in
  let pval_prim, pval_type = 
    match ty with 
    | `Fn | `PropertyFn -> 
      [Literals.js_fn_run; string_arity], 
      arrow ~loc ""  (lift_curry_type loc args_type result_type ) fn_type
    | `Method -> 
      [Literals.js_method_run ; string_arity], 
      arrow ~loc "" (lift_method_type loc args_type result_type) fn_type
  in
  Ast_comb.create_local_external loc ~pval_prim ~pval_type 
    (("", fn) :: List.map (fun x -> "",x) args )

(* TODO: 
   have a final checking for property arities 
     [#=], 
   remove [case_set] support *)


let generic_apply  kind loc 
    (self : Ast_mapper.mapper) 
    (obj : Parsetree.expression) 
    (args : args ) cb   =
  let obj = self.expr self obj in
  let args =
    List.map (fun (label,e) ->
        if label <> "" then
          Location.raise_errorf ~loc "label is not allowed here"        ;
        self.expr self e
      ) args in
  let len = List.length args in 
  match args with 
  | [ {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)}]
    -> 
    generic_run kind loc 0 (cb loc obj)
      []
  | _ -> 
    generic_run kind loc len (cb loc obj) args

let uncurry_fn_apply loc self fn args = 
  generic_apply `Fn loc self fn args (fun _ obj -> obj )

let property_apply loc self obj name (args : args) 
  =  generic_apply `PropertyFn loc self obj args 
    (fun loc obj -> Exp.mk ~loc (js_property loc obj name))

let method_apply loc self obj name args = 
  generic_apply `Method loc self obj args 
    (fun loc obj -> Exp.mk ~loc (js_property loc obj name))


let gen_fn_mk loc arity arg  : Parsetree.expression_desc = 
  let pval_prim = [ Literals.js_fn_mk; string_of_int arity]  in
  let fn_type , args_type, result_type  = Ast_comb.tuple_type_pair ~loc `Make arity  in 
  let pval_type = arrow ~loc "" fn_type (lift_curry_type loc args_type result_type) in
  Ast_comb.create_local_external loc ~pval_prim ~pval_type [("", arg)]

let gen_method_callback_mk loc arity arg  : Parsetree.expression_desc = 
  let pval_prim = [ Literals.js_fn_method; string_of_int arity]  in
  let fn_type , args_type, result_type = Ast_comb.tuple_type_pair ~loc `Make  arity  in 
  let pval_type = 
    arrow ~loc "" fn_type (lift_js_method_callback loc args_type result_type)
  in
  Ast_comb.create_local_external loc ~pval_prim ~pval_type [("", arg)]


(** TODO: how to handle attributes *)
let to_uncurry_type loc (mapper : Ast_mapper.mapper)
    (first_arg : Parsetree.core_type) 
    (typ : Parsetree.core_type)  = 
  let rec aux acc (typ : Parsetree.core_type) = 
    (* in general, 
       we should collect [typ] in [int -> typ] before transformation, 
       however: when attributes [bs] and [bs.this] found in typ, 
       we should stop 
    *)
    match Ast_attributes.process_attributes_rev typ.ptyp_attributes with 
    | `Nothing, _   -> 
      begin match typ.ptyp_desc with 
      | Ptyp_arrow (label, arg, body)
        -> 
        if label <> "" then
          Location.raise_errorf ~loc:typ.ptyp_loc "label is not allowed";
        aux (mapper.typ mapper arg :: acc) body 
      | _ -> mapper.typ mapper typ, acc 
      end
    | _, _ -> mapper.typ mapper typ, acc  
  in 
  let first_arg = mapper.typ mapper first_arg in
  let result, rev_extra_args = 
    aux  [first_arg] typ in 

  match rev_extra_args with 
  | [{ptyp_desc = Ptyp_constr ({txt = Lident "unit"}, [])}]
    ->
    lift_curry_type loc [] result 
  | _
    -> 
    lift_curry_type loc (List.rev rev_extra_args) result 


let to_method_type loc (mapper : Ast_mapper.mapper)
    (first_arg : Parsetree.core_type) 
    (typ : Parsetree.core_type)  = 
  let rec aux acc (typ : Parsetree.core_type) = 
    (* in general, 
       we should collect [typ] in [int -> typ] before transformation, 
       however: when attributes [bs] and [bs.this] found in typ, 
       we should stop 
    *)
    match Ast_attributes.process_attributes_rev typ.ptyp_attributes with 
    |  `Nothing, _ -> 
      begin match typ.ptyp_desc with 
      | Ptyp_arrow (label, arg, body)
        -> 
        if label <> "" then
          Location.raise_errorf ~loc:typ.ptyp_loc "label is not allowed";
        aux (mapper.typ mapper arg :: acc) body 
      | _ -> mapper.typ mapper typ, acc 
      end
    | _, _ -> mapper.typ mapper typ, acc  
  in 
  let first_arg = mapper.typ mapper first_arg in
  let result, rev_extra_args = 
    aux  [first_arg] typ in 

  match rev_extra_args with 
  | [{ptyp_desc = Ptyp_constr ({txt = Lident "unit"}, [])}]
    ->
    lift_method_type loc [] result 
  | _
    -> 
    lift_method_type loc (List.rev rev_extra_args) result 

  
let to_method_callback_type 
    loc 
    (mapper : Ast_mapper.mapper)
    (first_arg : Parsetree.core_type) 
    (typ : Parsetree.core_type)  = 
  let rec aux acc (typ : Parsetree.core_type) = 
    match Ast_attributes.process_attributes_rev typ.ptyp_attributes with 
    |  `Nothing , _ -> 
      begin match typ.ptyp_desc with 
        | Ptyp_arrow (label, arg, body)
          -> 
          if label <> "" then
            Location.raise_errorf ~loc:typ.ptyp_loc "label is not allowed";
          aux (mapper.typ mapper arg :: acc) body 
        | _ -> mapper.typ mapper typ, acc 
      end 
    | _, _ -> mapper.typ mapper typ, acc  
  in 
  let first_arg = mapper.typ mapper first_arg in 
  let result, rev_extra_args = aux  [first_arg] typ in 
  lift_js_method_callback loc (List.rev rev_extra_args) result




let to_uncurry_fn loc (mapper : Ast_mapper.mapper)  pat body 
  = 
  let rec aux acc (body : Parsetree.expression) = 
    match Ast_attributes.process_attributes_rev body.pexp_attributes with 
    | `Nothing, _ -> 
      begin match body.pexp_desc with 
        | Pexp_fun (label,_, arg, body)
          -> 
          if label <> "" then
            Location.raise_errorf ~loc "label is not allowed";
          aux (mapper.pat mapper arg :: acc) body 
        | _ -> mapper.expr mapper body, acc 
      end 
    | _, _ -> mapper.expr mapper body, acc  
  in 
  let first_arg = mapper.pat mapper pat in  
  let result, rev_extra_args = aux [first_arg] body in 
  match rev_extra_args with 
  | [ {ppat_desc = Ppat_construct ({txt = Lident "()"}, None)}]
    ->  
    gen_fn_mk loc 0 
      (Ast_comb.fun_no_label ~loc (Ast_literal.pat_unit ~loc () ) result)

  | _ -> 
    gen_fn_mk loc (List.length rev_extra_args) 
      (List.fold_left (fun e p -> Ast_comb.fun_no_label ~loc p e )
         result rev_extra_args )

let to_method_callback loc (self : Ast_mapper.mapper) pat body  
  = 
  let rec aux acc (body : Parsetree.expression) = 
    match Ast_attributes.process_attributes_rev body.pexp_attributes with 
    | `Nothing, _ -> 
      begin match body.pexp_desc with 
        | Pexp_fun (label,_, arg, body)
          -> 
          if label <> "" then
            Location.raise_errorf ~loc "label is not allowed";
          aux (self.pat self arg :: acc) body 
        | _ -> self.expr self body, acc 
      end 
    | _, _ -> self.expr self body, acc  
  in 
  let first_arg = self.pat self pat in  
  let result, rev_extra_args = aux [first_arg] body in 
  let len = List.length rev_extra_args in 
  gen_method_callback_mk loc len 
    (List.fold_left 
       (fun e p -> Ast_comb.fun_no_label ~loc p e) result rev_extra_args )



let from_labels ~loc (labels : Asttypes.label list) : Parsetree.core_type = 
  let arity = List.length labels in 
  let tyvars = (Ext_list.init arity (fun i ->      
      Typ.var ~loc ("a" ^ string_of_int i))) in 
  let result_type =
    to_js_type loc  
     (Typ.object_ ~loc (List.map2 (fun x y -> x ,[], y) labels tyvars) Closed)
  in 
  List.fold_right2 
    (fun label tyvar acc -> arrow ~loc label tyvar acc) labels tyvars  result_type

let handle_debugger loc payload = 
  if Ast_payload.as_empty_structure payload then
    let predef_unit_type = Ast_literal.type_unit ~loc () in
    let pval_prim = ["js_debugger"] in
    Ast_comb.create_local_external loc 
      ~pval_prim
      ~pval_type:(arrow "" predef_unit_type predef_unit_type)
      [("",  Ast_literal.val_unit ~loc ())]
  else Location.raise_errorf ~loc "bs.raw can only be applied to a string"


let handle_raw loc payload = 
  begin match Ast_payload.as_string_exp payload with 
    | None -> 
      Location.raise_errorf ~loc "bs.raw can only be applied to a string"
    | Some exp -> 
      let pval_prim = ["js_pure_expr"] in
      { exp with pexp_desc = Ast_comb.create_local_external loc 
                     ~pval_prim
                     ~pval_type:(arrow "" 
                                   (Ast_literal.type_string ~loc ()) 
                                   (Ast_literal.type_any ~loc ()) )

                     ["",exp]}
  end

let handle_raw_structure loc payload = 
  begin match Ast_payload.as_string_exp payload with 
    | Some exp 
      -> 
      let pval_prim = ["js_pure_stmt"] in 
      Ast_helper.Str.eval 
        { exp with pexp_desc =
                     Ast_comb.create_local_external loc 
                       ~pval_prim
                       ~pval_type:(arrow ""
                                     (Ast_literal.type_string ~loc ())
                                     (Ast_literal.type_any ~loc ()))
                       ["",exp]}
    | None
      -> 
      Location.raise_errorf ~loc "bs.raw can only be applied to a string"
  end

let record_as_js_object 
    loc 
    (self : Ast_mapper.mapper)
    (label_exprs : label_exprs)
     : Parsetree.expression_desc = 
  let labels, args = 
    Ext_list.split_map (fun ({Location.txt ; loc}, e) -> 
        match txt with
        | Longident.Lident x -> (x, (x, self.expr self e))
        | Ldot _ | Lapply _ ->  
          Location.raise_errorf ~loc "invalid js label "
  ) label_exprs in 
  let pval_prim = [ "" ] in 
  let pval_attributes = [Ast_attributes.bs_obj] in 
  let pval_type = from_labels ~loc labels in 
  Ast_comb.create_local_external loc 
    ~pval_prim
    ~pval_type ~pval_attributes 
    args 
