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
type 'a cxt = Ast_helper.loc -> Bs_ast_mapper.mapper -> 'a
type loc = Location.t 

type exp = Parsetree.expression

type label_exprs = (Longident.t Asttypes.loc * Parsetree.expression) list
type uncurry_expression_gen = 
  (Parsetree.pattern ->
   Parsetree.expression ->
   Parsetree.expression_desc) cxt







let arrow = Ast_compatible.arrow


let js_property loc obj (name : string) =
  Parsetree.Pexp_send
    ((Ast_compatible.app1 ~loc
        (Exp.ident ~loc
           {loc;
            txt = Ldot (Lident "Js_internalOO", Literals.unsafe_downgrade)})
        obj), 
        {loc; txt = name}
        )

(* TODO: 
   have a final checking for property arities 
     [#=], 
*)
let jsInternal = 
  Ast_literal.Lid.js_internal
let unsafeInvariantApply : Longident.t =
  Longident.Ldot
   (jsInternal,
   "unsafeInvariantApply")

let generic_apply loc 
    (self : Bs_ast_mapper.mapper) 
    (obj : Parsetree.expression) 
    (args : Parsetree.expression list) (cb : loc -> exp-> exp)   =
  let obj = self.expr self obj in
  let args =
    Ext_list.map args (fun e -> self.expr self e) in
  let fn = cb loc obj in   
  let args  = 
    match args with 
    | [ {pexp_desc =
           Pexp_construct ({txt = Lident "()"}, None)}]
      -> []
    | _ -> args in
  let arity = List.length args in       
  if arity = 0 then 
    Parsetree.Pexp_apply 
      (Exp.ident {txt = Ldot (jsInternal, "run0");loc}, [Nolabel,fn])
  else 
    let txt : Longident.t = 
      Ldot (jsInternal, "run" ^ string_of_int arity) in 
    Parsetree.Pexp_apply (
      Exp.ident {txt = unsafeInvariantApply; loc},
      [Nolabel,
       Exp.apply (Exp.ident {txt ; loc}) ((Nolabel,fn) :: 
                                          Ext_list.map args (fun x -> Asttypes.Nolabel,x))])                        

let generic_method_apply  loc 
    (self : Bs_ast_mapper.mapper) 
    (obj : Parsetree.expression) 
    (args : Parsetree.expression list) (cb : loc -> exp-> exp)   =
  let obj = self.expr self obj in
  let args =
    Ext_list.map args (fun e -> self.expr self e) in
  let fn = cb loc obj in   
  let args  = 
    match args with 
    | [ {pexp_desc =
           Pexp_construct ({txt = Lident "()"}, None)}]
      -> []
    | _ -> args in
  let arity = List.length args in       
  let txt : Longident.t = 
    Ldot(Lident "Js_internalOO", Literals.method_run ^ string_of_int arity) in 
  Parsetree.Pexp_apply (Exp.ident {txt ; loc}, (Nolabel,fn) :: Ext_list.map args (fun x -> Asttypes.Nolabel,x))



let uncurry_fn_apply loc self fn args = 
  generic_apply  loc self fn args (fun _ obj -> obj )

let property_apply loc self obj name args  
  =  generic_apply loc self obj args 
    (fun loc obj -> Exp.mk ~loc (js_property loc obj name))

let method_apply loc self obj name args = 
  generic_method_apply  loc self obj args 
    (fun loc obj -> Exp.mk ~loc (js_property loc obj name))

 

let to_method_callback  loc (self : Bs_ast_mapper.mapper)  pat body 
  = 
  let rec aux acc (body : Parsetree.expression) = 
    match Ast_attributes.process_attributes_rev body.pexp_attributes with 
    | Nothing, _ -> 
      begin match body.pexp_desc with 
        | Pexp_fun (arg_label,_, arg, body)
          -> 
          if arg_label <> Nolabel  then
            Bs_syntaxerr.err loc Label_in_uncurried_bs_attribute;
          aux (self.pat self arg :: acc) body 
        | _ -> self.expr self body, acc 
      end 
    | _, _ -> self.expr self body, acc  
  in 
  let first_arg = self.pat self pat in  
  (if not @@ Ast_pat.is_single_variable_pattern_conservative first_arg then
     Bs_syntaxerr.err first_arg.ppat_loc  Bs_this_simple_pattern);  
  let result, rev_extra_args = aux [first_arg] body in 
  let body = 
    Ext_list.fold_left rev_extra_args result (fun e p -> Ast_compatible.fun_ ~loc p e )
  in
  let len = List.length rev_extra_args in   
  let arity = len  in 
  if arity < 10 then 
    let txt = Longident.Ldot (Lident "Js_internalOO",  Literals.fn_method ^ string_of_int arity) in
    Parsetree.Pexp_apply (Exp.ident {txt;loc} , [ Nolabel, body])
  else 
    let pval_prim =
      [  "#fn_method"; string_of_int arity]  in
    let fn_type , args_type, result_type  = Ast_comb.tuple_type_pair ~loc `Make arity  in 
    let pval_type = arrow ~loc  fn_type 
        (Ast_typ_uncurry.lift_js_method_callback loc args_type result_type) in
    Ast_external_mk.local_extern_cont loc ~pval_prim ~pval_type 
      (fun prim -> Ast_compatible.app1 ~loc prim body) 


let to_uncurry_fn  loc (self : Bs_ast_mapper.mapper) (label : Asttypes.arg_label) pat body 
  = 
  (match label with 
  | Optional _ -> Bs_syntaxerr.err loc Label_in_uncurried_bs_attribute
  | _ -> ());  
  let rec aux acc (body : Parsetree.expression) = 
    match Ast_attributes.process_attributes_rev body.pexp_attributes with 
    | Nothing, _ -> 
      begin match body.pexp_desc with 
        | Pexp_fun (arg_label,_, arg, body)
          -> 
          (match arg_label with 
          | Optional _ -> Bs_syntaxerr.err loc Label_in_uncurried_bs_attribute
          | _ -> ());
          aux ((arg_label, self.pat self arg) :: acc) body 
        | _ -> self.expr self body, acc 
      end 
    | _, _ -> self.expr self body, acc  
  in 
  let first_arg = self.pat self pat in  

  let result, rev_extra_args = aux [label,first_arg] body in 
  let body = 
    Ext_list.fold_left rev_extra_args result (fun e (label,p) -> Ast_helper.Exp.fun_ ~loc label None p e)
  in
  let len = List.length rev_extra_args in   
  let arity = 
    match rev_extra_args with 
    | [ l,p]
      ->
       Ast_pat.is_unit_cont ~yes:0 ~no:len p           
    | _ -> len 
  in 
  if arity = 0 && label = Nolabel then 
    let txt = 
      Longident.Ldot (jsInternal, "mk0") in
    Parsetree.Pexp_apply (Exp.ident {txt;loc} , [ Nolabel, body])
  else 
    Parsetree.Pexp_record ([
        {
          txt = Ldot (Ast_literal.Lid.js_fn, "_" ^ string_of_int arity); 
          loc
        },body], None) 






let ocaml_obj_as_js_object
    loc (mapper : Bs_ast_mapper.mapper)
    (self_pat : Parsetree.pattern)
    (clfs : Parsetree.class_field list) =
  let self_type_lit = "self_type"   in 

  (** Attention: we should avoid type variable conflict for each method  
      Since the method name is unique, there would be no conflict 
      OCaml does not allow duplicate instance variable and duplicate methods, 
      but it does allow duplicates between instance variable and method name, 
      we should enforce such rules 
      {[
        object 
          val x = 3
          method x = 3 
        end [@bs]
      ]} should not compile with a meaningful error message
  *)

  let generate_val_method_pair 
      loc (mapper : Bs_ast_mapper.mapper)
      (val_name : string Asttypes.loc) is_mutable = 

    let result = Typ.var ~loc val_name.txt in 
    result , 
    ((val_name , [], result ) ::
     (if is_mutable then 
        [{val_name with txt = val_name.txt ^ Literals.setter_suffix},[],
         Ast_typ_uncurry.to_method_type loc mapper Nolabel result (Ast_literal.type_unit ~loc ()) ]
      else 
        []) )
  in 
  (* Note mapper is only for API compatible 
   * TODO: we should check label name to avoid conflict 
  *)  
  let self_type loc = Typ.var ~loc self_type_lit in 

  let generate_arg_type loc (mapper  : Bs_ast_mapper.mapper)
      method_name arity : Ast_core_type.t = 
    let result = Typ.var ~loc method_name in   
    if arity = 0 then
      Ast_typ_uncurry.to_method_type loc mapper Nolabel (Ast_literal.type_unit ~loc ()) result 

    else
      let tyvars =
        Ext_list.init arity (fun i -> Typ.var ~loc (method_name ^ string_of_int i))
      in
      begin match tyvars with
        | x :: rest ->
          let method_rest =
            Ext_list.fold_right rest result (fun v acc -> Ast_compatible.arrow ~loc  v acc)
          in         
          Ast_typ_uncurry.to_method_type loc mapper Nolabel x method_rest
        | _ -> assert false
      end in          

  let generate_method_type
      loc
      (mapper : Bs_ast_mapper.mapper)
      ?alias_type method_name arity =
    let result = Typ.var ~loc method_name in   

    let self_type =
      let v = self_type loc  in
      match alias_type with 
      | None -> v 
      | Some ty -> Typ.alias ~loc ty self_type_lit
    in  
    if arity = 0 then
      Ast_typ_uncurry.to_method_callback_type loc mapper  Nolabel self_type result      
    else
      let tyvars =
        Ext_list.init arity (fun i -> Typ.var ~loc (method_name ^ string_of_int i))
      in
      begin match tyvars with
        | x :: rest ->
          let method_rest =
            Ext_list.fold_right rest result (fun v acc -> Ast_compatible.arrow ~loc  v acc)
          in         
          (Ast_typ_uncurry.to_method_callback_type loc mapper  Nolabel self_type
             (Ast_compatible.arrow ~loc  x method_rest))
        | _ -> assert false
      end in          


  (** we need calculate the real object type 
      and exposed object type, in some cases there are equivalent

      for public object type its [@bs.meth] it does not depend on itself
      while for label argument it is [@bs.this] which depends internal object
  *)
  let internal_label_attr_types, public_label_attr_types  = 
    Ext_list.fold_right clfs ([], []) 
      (fun ({pcf_loc  = loc} as x  : Parsetree.class_field) 
        (label_attr_types, public_label_attr_types) ->
        match x.pcf_desc with
        | Pcf_method (
            label,
            public_flag,
            Cfk_concrete
              (Fresh, e))
          ->
          begin match e.pexp_desc with
            | Pexp_poly
                (({pexp_desc = Pexp_fun (Nolabel, _, pat, e)} ),
                 None) 
              ->  
              let arity = Ast_pat.arity_of_fun pat e in
              let method_type =
                generate_arg_type x.pcf_loc mapper label.txt arity in 
              ((label, [], method_type) :: label_attr_types),
              (if public_flag = Public then
                 (label, [], method_type) :: public_label_attr_types
               else 
                 public_label_attr_types)

            | Pexp_poly( _, Some _)
              ->
              Location.raise_errorf ~loc "polymorphic type annotation not supported yet"
            | Pexp_poly (_, None) ->
              Location.raise_errorf ~loc
                "Unsupported syntax, expect syntax like `method x () = x ` "
            | _ ->
              Location.raise_errorf ~loc "Unsupported syntax in js object"               
          end
        | Pcf_val (label, mutable_flag, Cfk_concrete(Fresh, val_exp)) ->
          let  label_type, label_attr  = 
            generate_val_method_pair x.pcf_loc mapper label
              (mutable_flag = Mutable )
          in
          (Ext_list.append label_attr  label_attr_types, public_label_attr_types)
        | Pcf_val (label, mutable_flag, Cfk_concrete(Override, val_exp)) -> 
          Location.raise_errorf ~loc "override flag not support currently"
        | Pcf_val (label, mutable_flag, Cfk_virtual _) -> 
          Location.raise_errorf ~loc "virtual flag not support currently"

        | Pcf_method (_, _, Cfk_concrete(Override, _) ) -> 
          Location.raise_errorf ~loc "override flag not supported"

        | Pcf_method (_, _, Cfk_virtual _ )
          ->
          Location.raise_errorf ~loc "virtural method not supported"

        | Pcf_inherit _ 
        | Pcf_initializer _
        | Pcf_attribute _
        | Pcf_extension _
        | Pcf_constraint _ ->
          Location.raise_errorf ~loc "Only method support currently"
      ) in
  let internal_obj_type = Ast_core_type.make_obj ~loc internal_label_attr_types in
  let public_obj_type = Ast_core_type.make_obj ~loc public_label_attr_types in
  let (labels,  label_types, exprs, _) =
    Ext_list.fold_right clfs  ([], [], [], false)
      (fun (x  : Parsetree.class_field)
        (labels,
         label_types,
         exprs, aliased ) ->
        match x.pcf_desc with
        | Pcf_method (
            label,
            _public_flag,
            Cfk_concrete
              (Fresh, e))
          ->
          begin match e.pexp_desc with
            | Pexp_poly
                (({pexp_desc = Pexp_fun (Nolabel, None, pat, e)} as f),
                 None)
              ->  
              let arity = Ast_pat.arity_of_fun pat e in
              let alias_type = 
                if aliased then None 
                else Some internal_obj_type in
              let  label_type =
                generate_method_type ?alias_type
                  x.pcf_loc mapper label.txt arity in 
              (label::labels,
               label_type::label_types,
               {f with
                pexp_desc =
                  let f = Ast_pat.is_unit_cont pat ~yes:e ~no:f in                       
                  to_method_callback loc mapper self_pat f
               } :: exprs, 
               true
              )
            | Pexp_poly( _, Some _)
              ->
              Location.raise_errorf ~loc
                "polymorphic type annotation not supported yet"

            | Pexp_poly (_, None) ->
              Location.raise_errorf
                ~loc "Unsupported syntax, expect syntax like `method x () = x ` "
            | _ ->
              Location.raise_errorf ~loc "Unsupported syntax in js object"               
          end
        | Pcf_val (label, mutable_flag, Cfk_concrete(Fresh, val_exp)) ->
          let  label_type, label_attr  = 
            generate_val_method_pair x.pcf_loc mapper label
              (mutable_flag = Mutable )
          in
          (label::labels,
           label_type :: label_types, 
           (mapper.expr mapper val_exp :: exprs), 
           aliased 
          )

        | Pcf_val (label, mutable_flag, Cfk_concrete(Override, val_exp)) -> 
          Location.raise_errorf ~loc "override flag not support currently"
        | Pcf_val (label, mutable_flag, Cfk_virtual _) -> 
          Location.raise_errorf ~loc "virtual flag not support currently"

        | Pcf_method (_, _, Cfk_concrete(Override, _) ) -> 
          Location.raise_errorf ~loc "override flag not supported"

        | Pcf_method (_, _, Cfk_virtual _ )
          ->
          Location.raise_errorf ~loc "virtural method not supported"


        | Pcf_inherit _ 
        | Pcf_initializer _
        | Pcf_attribute _
        | Pcf_extension _
        | Pcf_constraint _ ->
          Location.raise_errorf ~loc "Only method support currently"
      )  in
  let pval_type =
    Ext_list.fold_right2  labels label_types public_obj_type
      (fun label label_type acc ->
         Ast_compatible.label_arrow
           ~loc:label.Asttypes.loc
           label.Asttypes.txt
           label_type acc           
      ) in
  Ast_external_mk.local_extern_cont
    loc
    ~pval_prim:(Ast_external_process.pval_prim_of_labels labels)
    (fun e ->
       Ast_compatible.apply_labels ~loc e
         (Ext_list.map2 labels exprs (fun l expr -> l.txt, expr) ) )
    ~pval_type


let record_as_js_object 
    loc 
    (self : Bs_ast_mapper.mapper)
    (label_exprs : label_exprs)
  : Parsetree.expression_desc = 

  let labels,args, arity =
    Ext_list.fold_right label_exprs ([],[],0) (fun ({txt ; loc}, e) (labels,args,i) -> 
        match txt with
        | Lident x ->
          ({Asttypes.loc = loc ; txt = x} :: labels, (x, self.expr self e) :: args, i + 1)
        | Ldot _ | Lapply _ ->  
          Location.raise_errorf ~loc "invalid js label ")  in
  Ast_external_mk.local_external_obj loc 
    ~pval_prim:(Ast_external_process.pval_prim_of_labels labels)
    ~pval_type:(Ast_core_type.from_labels ~loc arity labels) 
    args 

