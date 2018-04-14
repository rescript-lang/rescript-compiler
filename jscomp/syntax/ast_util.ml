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
type args = (string * Parsetree.expression) list
type label_exprs = (Longident.t Asttypes.loc * Parsetree.expression) list
type uncurry_expression_gen = 
  (Parsetree.pattern ->
   Parsetree.expression ->
   Parsetree.expression_desc) cxt
type uncurry_type_gen = 
  (string ->
   Parsetree.core_type ->
   Parsetree.core_type  ->
   Parsetree.core_type) cxt

let uncurry_type_id = 
  Ast_literal.Lid.js_fn

let method_id  = 
  Ast_literal.Lid.js_meth

let method_call_back_id  = 
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
  generic_lift   uncurry_type_id loc

let lift_method_type loc  = 
  generic_lift  method_id loc

let lift_js_method_callback loc
  = 
  generic_lift method_call_back_id loc 
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



let arrow = Typ.arrow


let js_property loc obj name =
  Parsetree.Pexp_send
    ((Exp.apply ~loc
        (Exp.ident ~loc
           {loc;
            txt = Ldot (Ast_literal.Lid.js_unsafe, Literals.unsafe_downgrade)})
        ["",obj]), name)

(* TODO: 
   have a final checking for property arities 
     [#=], 
*)


let generic_apply  kind loc 
    (self : Bs_ast_mapper.mapper) 
    (obj : Parsetree.expression) 
    (args : args ) cb   =
  let obj = self.expr self obj in
  let args =
    Ext_list.map (fun (label,e) ->
        if label <> "" then
          Bs_syntaxerr.err loc Label_in_uncurried_bs_attribute;
        self.expr self e
      ) args in
  let len = List.length args in 
  let arity, fn, args  = 
    match args with 
    | [ {pexp_desc =
           Pexp_construct ({txt = Lident "()"}, None)}]
      -> 
      0, cb loc obj, []
    | _ -> 
      len,  cb loc obj, args in
  if arity < 10 then 
    let txt = 
      match kind with 
      | `Fn | `PropertyFn ->  
        Longident.Ldot (Ast_literal.Lid.js_unsafe, 
                        Literals.fn_run ^ string_of_int arity)
      | `Method -> 
        Longident.Ldot(Ast_literal.Lid.js_unsafe,
                       Literals.method_run ^ string_of_int arity
                      ) in 
    Parsetree.Pexp_apply (Exp.ident {txt ; loc}, ("",fn) :: Ext_list.map (fun x -> "",x) args)
  else 
    let fn_type, args_type, result_type = Ast_comb.tuple_type_pair ~loc `Run arity  in 
    let string_arity = string_of_int arity in
    let pval_prim, pval_type = 
      match kind with 
      | `Fn | `PropertyFn -> 
        ["#fn_run"; string_arity], 
        arrow ~loc ""  (lift_curry_type loc args_type result_type ) fn_type
      | `Method -> 
        ["#method_run" ; string_arity], 
        arrow ~loc "" (lift_method_type loc args_type result_type) fn_type
    in
    Ast_external_mk.local_external loc ~pval_prim ~pval_type 
      (("", fn) :: Ext_list.map (fun x -> "",x) args )


let uncurry_fn_apply loc self fn args = 
  generic_apply `Fn loc self fn args (fun _ obj -> obj )

let property_apply loc self obj name (args : args) 
  =  generic_apply `PropertyFn loc self obj args 
    (fun loc obj -> Exp.mk ~loc (js_property loc obj name))

let method_apply loc self obj name args = 
  generic_apply `Method loc self obj args 
    (fun loc obj -> Exp.mk ~loc (js_property loc obj name))

let generic_to_uncurry_type  kind loc (mapper : Bs_ast_mapper.mapper) label
    (first_arg : Parsetree.core_type) 
    (typ : Parsetree.core_type)  =
  if label <> "" then
    Bs_syntaxerr.err loc Label_in_uncurried_bs_attribute;

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
            Bs_syntaxerr.err typ.ptyp_loc Label_in_uncurried_bs_attribute;
          aux (mapper.typ mapper arg :: acc) body 
        | _ -> mapper.typ mapper typ, acc 
      end
    | _, _ -> mapper.typ mapper typ, acc  
  in 
  let first_arg = mapper.typ mapper first_arg in
  let result, rev_extra_args = aux  [first_arg] typ in 
  let args  = List.rev rev_extra_args in 
  let filter_args args  =  
    match args with 
    | [{Parsetree.ptyp_desc = 
          (Ptyp_constr ({txt = Lident "unit"}, []) 
          )}]
      -> []
    | _ -> args in
  match kind with 
  | `Fn ->
    let args = filter_args args in
    lift_curry_type loc args result 
  | `Method -> 
    let args = filter_args args in
    lift_method_type loc args result 

  | `Method_callback
    -> lift_js_method_callback loc args result 


let to_uncurry_type  = 
  generic_to_uncurry_type `Fn
let to_method_type  =
  generic_to_uncurry_type  `Method
let to_method_callback_type  = 
  generic_to_uncurry_type `Method_callback 

let generic_to_uncurry_exp kind loc (self : Bs_ast_mapper.mapper)  pat body 
  = 
  let rec aux acc (body : Parsetree.expression) = 
    match Ast_attributes.process_attributes_rev body.pexp_attributes with 
    | `Nothing, _ -> 
      begin match body.pexp_desc with 
        | Pexp_fun (label,_, arg, body)
          -> 
          if label <> "" then
            Bs_syntaxerr.err loc Label_in_uncurried_bs_attribute;
          aux (self.pat self arg :: acc) body 
        | _ -> self.expr self body, acc 
      end 
    | _, _ -> self.expr self body, acc  
  in 
  let first_arg = self.pat self pat in  
  let () = 
    match kind with 
    | `Method_callback -> 
      if not @@ Ast_pat.is_single_variable_pattern_conservative first_arg then
        Bs_syntaxerr.err first_arg.ppat_loc  Bs_this_simple_pattern
    | _ -> ()
  in 

  let result, rev_extra_args = aux [first_arg] body in 
  let body = 
    List.fold_left (fun e p -> Ast_comb.fun_no_label ~loc p e )
      result rev_extra_args in
  let len = List.length rev_extra_args in 
  let arity = 
    match kind with 
    | `Fn  ->
      begin match rev_extra_args with 
        | [ p]
          ->
          Ast_pat.is_unit_cont ~yes:0 ~no:len p           

        | _ -> len 
      end
    | `Method_callback -> len  in 
  if arity < 10  then 
    let txt = 
      match kind with 
      | `Fn -> 
        Longident.Ldot ( Ast_literal.Lid.js_unsafe, Literals.fn_mk ^ string_of_int arity)
      | `Method_callback -> 
        Longident.Ldot (Ast_literal.Lid.js_unsafe,  Literals.fn_method ^ string_of_int arity) in
    Parsetree.Pexp_apply (Exp.ident {txt;loc} , ["",body])

  else 
    let pval_prim =
      [ (match kind with 
            | `Fn -> "#fn_mk"
            | `Method_callback -> "#fn_method"); 
        string_of_int arity]  in
    let fn_type , args_type, result_type  = Ast_comb.tuple_type_pair ~loc `Make arity  in 
    let pval_type = arrow ~loc "" fn_type (
        match kind with 
        | `Fn -> 
          lift_curry_type loc args_type result_type
        | `Method_callback -> 
          lift_js_method_callback loc args_type result_type
      ) in
    Ast_external_mk.local_extern_cont loc ~pval_prim ~pval_type 
      (fun prim -> Exp.apply ~loc prim ["", body]) 

let to_uncurry_fn   = 
  generic_to_uncurry_exp `Fn
let to_method_callback  = 
  generic_to_uncurry_exp `Method_callback 


let handle_debugger loc payload = 
  if Ast_payload.as_empty_structure payload then
    Parsetree.Pexp_apply
      (Exp.ident {txt = Ldot(Ast_literal.Lid.js_unsafe, Literals.debugger ); loc}, 
       ["", Ast_literal.val_unit ~loc ()])
  else Location.raise_errorf ~loc "bs.raw can only be applied to a string"


let handle_raw ~check_js_regex loc payload =
  begin match Ast_payload.as_string_exp ~check_js_regex payload with
    | Not_String_Lteral ->
      Location.raise_errorf ~loc
        "bs.raw can only be applied to a string"
    | Ast_payload.JS_Regex_Check_Failed ->
      Location.raise_errorf ~loc "this is an invalid js regex"
    | Correct exp ->
      let pexp_desc = 
        Parsetree.Pexp_apply (
          Exp.ident {loc; 
                     txt = 
                       Ldot (Ast_literal.Lid.js_unsafe, 
                             Literals.raw_expr)},
          ["",exp]
        )
      in
      { exp with pexp_desc }
  end

let handle_external loc x = 
  let raw_exp : Ast_exp.t = 
    Ast_helper.Exp.apply 
    (Exp.ident ~loc 
         {loc; txt = Ldot (Ast_literal.Lid.js_unsafe, 
                           Literals.raw_expr)})
      ~loc 
      [Ext_string.empty, 
        Exp.constant ~loc (Const_string (x,Some Ext_string.empty))] in 
  let empty = 
    Exp.ident ~loc 
    {txt = Ldot (Ldot(Lident"Js", "Undefined"), "empty");loc}    
  in 
  let undefined_typeof = 
    Exp.ident {loc ; txt = Ldot(Lident "Js","undefinedToOption")} in 
  let typeof = 
    Exp.ident {loc ; txt = Ldot(Lident "Js","typeof")} in 

  Exp.apply ~loc undefined_typeof [
    Ext_string.empty,
    Exp.ifthenelse ~loc
    (Exp.apply ~loc 
      (Exp.ident ~loc {loc ; txt = Ldot (Lident "Pervasives", "=")} )
      [ 
        Ext_string.empty,
        (Exp.apply ~loc typeof [Ext_string.empty,raw_exp]);
        Ext_string.empty, 
        Exp.constant ~loc (Const_string ("undefined",None))  
        ])      
      (empty)
      (Some raw_exp)
      ]


let handle_raw_structure loc payload = 
  begin match Ast_payload.as_string_exp ~check_js_regex:false payload with 
    | Correct exp 
      -> 
      let pexp_desc = 
        Parsetree.Pexp_apply(
          Exp.ident {txt = Ldot (Ast_literal.Lid.js_unsafe,  Literals.raw_stmt); loc},
          ["",exp]) in 
      Ast_helper.Str.eval 
        { exp with pexp_desc }

    | Not_String_Lteral
      -> 
      Location.raise_errorf ~loc "bs.raw can only be applied to a string"
    | JS_Regex_Check_Failed 
      ->
      Location.raise_errorf ~loc "this is an invalid js regex"
  end


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
      val_name  is_mutable = 

    let result = Typ.var ~loc val_name in 
    result , 
    ((val_name , [], result ) ::
     (if is_mutable then 
        [val_name ^ Literals.setter_suffix,[],
         to_method_type loc mapper "" result (Ast_literal.type_unit ~loc ()) ]
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
      to_method_type loc mapper "" (Ast_literal.type_unit ~loc ()) result 

    else
      let tyvars =
        Ext_list.init arity (fun i -> Typ.var ~loc (method_name ^ string_of_int i))
      in
      begin match tyvars with
        | x :: rest ->
          let method_rest =
            Ext_list.fold_right (fun v acc -> Typ.arrow ~loc "" v acc)
              rest result in         
          to_method_type loc mapper "" x method_rest
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
      to_method_callback_type loc mapper  "" self_type result      
    else
      let tyvars =
        Ext_list.init arity (fun i -> Typ.var ~loc (method_name ^ string_of_int i))
      in
      begin match tyvars with
        | x :: rest ->
          let method_rest =
            Ext_list.fold_right (fun v acc -> Typ.arrow ~loc "" v acc)
              rest result in         
          (to_method_callback_type loc mapper  "" self_type
             (Typ.arrow ~loc "" x method_rest))
        | _ -> assert false
      end in          


  (** we need calculate the real object type 
      and exposed object type, in some cases there are equivalent

      for public object type its [@bs.meth] it does not depend on itself
      while for label argument it is [@bs.this] which depends internal object
  *)
  let internal_label_attr_types, public_label_attr_types  = 
    Ext_list.fold_right
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
                (({pexp_desc = Pexp_fun ("", None, pat, e)} ),
                 None) ->  
              let arity = Ast_pat.arity_of_fun pat e in
              let method_type =
                generate_arg_type x.pcf_loc mapper label.txt arity in 
              ((label.Asttypes.txt, [], method_type) :: label_attr_types),
              (if public_flag = Public then
                 (label.Asttypes.txt, [], method_type) :: public_label_attr_types
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
            generate_val_method_pair x.pcf_loc mapper label.txt  
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
      ) clfs ([], []) in
  let internal_obj_type = Ast_core_type.make_obj ~loc internal_label_attr_types in
  let public_obj_type = Ast_core_type.make_obj ~loc public_label_attr_types in
  let (labels,  label_types, exprs, _) =
    Ext_list.fold_right
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
                (({pexp_desc = Pexp_fun ("", None, pat, e)} as f),
                 None) ->  
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
            generate_val_method_pair x.pcf_loc mapper label.txt  
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
      ) clfs  ([], [], [], false) in
  let pval_type =
    Ext_list.fold_right2
      (fun label label_type acc ->
         Typ.arrow
           ~loc:label.Asttypes.loc
           label.Asttypes.txt
           label_type acc           
      ) labels label_types public_obj_type in
  Ast_external_mk.local_extern_cont
    loc
    ~pval_prim:(External_process.pval_prim_of_labels labels)
    (fun e ->
       Exp.apply ~loc e
         (Ext_list.map2 (fun l expr -> l.Asttypes.txt, expr) labels exprs) )
    ~pval_type


let record_as_js_object 
    loc 
    (self : Bs_ast_mapper.mapper)
    (label_exprs : label_exprs)
  : Parsetree.expression_desc = 

  let labels,args, arity =
    Ext_list.fold_right (fun ({Location.txt ; loc}, e) (labels,args,i) -> 
        match txt with
        | Longident.Lident x ->
          ({Asttypes.loc = loc ; txt = x} :: labels, (x, self.expr self e) :: args, i + 1)
        | Ldot _ | Lapply _ ->  
          Location.raise_errorf ~loc "invalid js label ") label_exprs ([],[],0) in
  Ast_external_mk.local_external loc 
    ~pval_prim:(External_process.pval_prim_of_labels labels)
    ~pval_type:(Ast_core_type.from_labels ~loc arity labels) 
    args 



let isCamlExceptionOrOpenVariant = Longident.parse "Caml_exceptions.isCamlExceptionOrOpenVariant"
let obj_magic = Longident.parse "Obj.magic"

let rec checkCases (cases : Parsetree.case list) = 
  List.iter check_case cases 
and check_case case = 
  check_pat case.pc_lhs 
and check_pat (pat : Parsetree.pattern) = 
  match pat.ppat_desc with 
  | Ppat_construct _ -> ()
  | Ppat_or (l,r) -> 
    check_pat l; check_pat r 
  | _ ->  Location.raise_errorf ~loc:pat.ppat_loc "Unsupported pattern in `bs.open`" 

let convertBsErrorFunction loc  (self : Bs_ast_mapper.mapper) attrs (cases : Parsetree.case list ) =
  let txt  = "match" in 
  let txt_expr = Exp.ident ~loc {txt = Lident txt; loc} in 
  let none = Exp.constraint_ ~loc 
      (Exp.construct ~loc {txt = Lident "None" ; loc} None) 
      (Ast_core_type.lift_option_type (Typ.any ~loc ())) in
  let () = checkCases cases in  
  let cases = self.cases self cases in 
  Exp.fun_ ~attrs ~loc ""  None ( Pat.var ~loc  {txt; loc })
    (Exp.ifthenelse
    ~loc 
    (Exp.apply ~loc (Exp.ident ~loc {txt = isCamlExceptionOrOpenVariant ; loc}) ["", txt_expr ])
    (Exp.match_ ~loc 
       (Exp.constraint_ ~loc 
          (Exp.apply  ~loc (Exp.ident ~loc {txt =  obj_magic; loc}) ["", txt_expr])
          (Ast_literal.type_exn ~loc ())
       )
      (Ext_list.map_append (fun (x :Parsetree.case ) ->
           let pc_rhs = x.pc_rhs in 
           let  loc  = pc_rhs.pexp_loc in
           {
             x with pc_rhs = 
                      Exp.constraint_ ~loc 
                        (Exp.construct ~loc {txt = Lident "Some";loc} (Some pc_rhs))
                        (Ast_core_type.lift_option_type (Typ.any ~loc ())  )
           }

         ) cases 
      [
       Exp.case  (Pat.any ~loc ()) none
     ])
    )
    (Some none))
    
                       
