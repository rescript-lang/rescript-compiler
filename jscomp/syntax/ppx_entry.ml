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






(* When we design a ppx, we should keep it simple, and also think about 
   how it would work with other tools like merlin and ocamldep  *)

(**
1. extension point 
   {[ 
     [%bs.raw{| blabla |}]
   ]}
   will be desugared into 
   {[ 
     let module Js = 
     struct unsafe_js : string -> 'a end 
     in Js.unsafe_js {| blabla |}
   ]}
   The major benefit is to better error reporting (with locations).
   Otherwise

   {[

     let f u = Js.unsafe_js u 
     let _ = f (1 + 2)
   ]}
   And if it is inlined some where   
*)



open Ast_helper

type method_kind = 
  | Case_setter
  | Setter
  | Normal of string 



let record_as_js_object = ref None (* otherwise has an attribute *)

let as_js_object_attribute  : Parsetree.attribute
  = {txt = "bs.obj" ; loc = Location.none}, PStr []

let obj_type_as_js_obj_type = ref false
let uncurry_type = ref false 
let obj_type_auto_uncurry =  ref false
let non_export = ref false 

let reset () = 
  record_as_js_object := None ;
  obj_type_as_js_obj_type := false ;
  uncurry_type := false ;
  obj_type_auto_uncurry := false ;
  non_export  :=  false


let arrow = Ast_helper.Typ.arrow

let handle_record_as_js_object 
    loc 
    attr
    (label_exprs : (Longident.t Asttypes.loc * Parsetree.expression) list)
    (mapper : Ast_mapper.mapper) : Parsetree.expression_desc = 
  let labels, args = 
    Ext_list.split_map (fun ({Location.txt ; loc}, e) -> 
        match txt with
        | Longident.Lident x -> (x, (x, mapper.expr mapper e))
        | Ldot _ | Lapply _ ->  
          Location.raise_errorf ~loc "invalid js label "
  ) label_exprs in 
  let pval_prim = [ "" ] in 
  let pval_attributes = [attr] in 
  let pval_type = Ast_util.from_labels ~loc labels in 
  Ast_comb.create_local_external loc 
    ~pval_prim
    ~pval_type ~pval_attributes 
    args 



(*
  Attributes are very hard to attribute
  (since ptyp_attributes could happen in so many places), 
  and write ppx extensions correctly, 
  we can only use it locally
*)

let handle_typ 
    (super : Ast_mapper.mapper) 
    (self : Ast_mapper.mapper)
    (ty : Parsetree.core_type) = 
  match ty with
  | {ptyp_desc = Ptyp_extension({txt = "bs.obj"}, PTyp ty)}
    -> 
    Ext_ref.protect obj_type_as_js_obj_type true (fun _ -> self.typ self ty )
  | {ptyp_attributes ;
     ptyp_desc = Ptyp_arrow ("", args, body);
     ptyp_loc = loc
   } ->
    let args = self.typ self args in
    let body = self.typ self body in
    begin match  Ast_util.find_uncurry_attrs_and_remove ptyp_attributes with 
      | Some _, ptyp_attributes ->
        Ast_util.uncurry_fn_type loc ty ptyp_attributes args body 
      | None, _ -> 
        if !uncurry_type then 
          Ast_util.uncurry_fn_type loc ty ptyp_attributes args body 
        else {ty with ptyp_desc = Ptyp_arrow("", args, body)}
    end
  | {
    ptyp_desc =  Ptyp_object ( methods, closed_flag) ;
    ptyp_attributes ;
    ptyp_loc = loc 
    } -> 
    let inner_type =
      begin match Ext_list.exclude_with_fact
                    (function 
                      | {Location.txt = "uncurry"; _}, _ -> true
                      | _ -> false)
                    ptyp_attributes with 
      |   None, _  ->
        let check_auto_uncurry core_type = 
          if  !obj_type_auto_uncurry then
            Ext_ref.protect uncurry_type true (fun _ -> self.typ self core_type  )          
          else self.typ self core_type in 

        let methods = 
          List.map (fun (label, ptyp_attrs, core_type ) -> 
              match Ast_util.find_uncurry_attrs_and_remove ptyp_attrs with 
              | None, _ -> 
                label, ptyp_attrs , check_auto_uncurry  core_type
              | Some v, ptyp_attrs -> 
                label , ptyp_attrs, 
                check_auto_uncurry
                  { core_type with ptyp_attributes = v :: core_type.ptyp_attributes}
            ) methods 
        in   
        { ty with ptyp_desc = Ptyp_object(methods, closed_flag);
                  ptyp_attributes } 

      |  fact2,  ptyp_attributes -> 

        let uncurry_type_cxt  = fact2 <> None || !uncurry_type || !obj_type_auto_uncurry in 
        let methods = 
          Ext_ref.protect uncurry_type uncurry_type_cxt begin fun _ -> 
            List.map (fun (label, ptyp_attrs, core_type ) -> 
                match Ast_util.find_uncurry_attrs_and_remove ptyp_attrs with 
                | None, _ -> label, ptyp_attrs , self.typ self core_type
                | Some v, ptyp_attrs -> 
                  label , ptyp_attrs, self.typ self 
                    { core_type with ptyp_attributes = v :: core_type.ptyp_attributes}
              ) methods 
          end
        in           
        { ty with ptyp_desc = Ptyp_object(methods, closed_flag);
                  ptyp_attributes } 
      end
    in          
    if !obj_type_as_js_obj_type then 
      Ast_util.lift_js_type ~loc  inner_type
    else inner_type
  | _ -> super.typ self ty

let handle_class_obj_typ 
    (super : Ast_mapper.mapper) 
    (self : Ast_mapper.mapper)
    (ty : Parsetree.class_type) = 
  match ty with
  | {pcty_attributes ;
     pcty_desc ; (* we won't have [ class type v = u -> object[@uncurry] ]*)
     pcty_loc = loc
   } ->
    begin match  Ast_util.find_uncurry_attrs_and_remove pcty_attributes with 
    | Some _, pcty_attributes' ->
      Ext_ref.protect uncurry_type true begin fun () -> 
        self.class_type self  {ty with pcty_attributes = pcty_attributes'} 
      end
    | None, _ -> 
      if !obj_type_auto_uncurry then 
        Ext_ref.protect uncurry_type true begin fun () -> 
          super.class_type self ty
        end
      else 
        super.class_type self ty
    end







let handle_obj_method loc (obj : Parsetree.expression) 
    name (value : Parsetree.expression) e 
    (mapper : Ast_mapper.mapper) : Parsetree.expression = 
  let value = mapper.expr mapper value in 
  let obj = mapper.expr mapper obj in 
  let args = Ast_util.destruct_tuple_exp value in 
  let len = List.length args in 
  {e with pexp_desc =
            Ast_util.gen_method_run loc len 
              (Exp.mk ~loc (Ast_util.down_with_name ~loc obj name )) (obj::args)
  }

let handle_obj_fn loc (obj : Parsetree.expression) 
    name (value : Parsetree.expression) e 
    (mapper : Ast_mapper.mapper) : Parsetree.expression = 
  let obj = mapper.expr mapper obj in 
  let value = mapper.expr mapper value in 
  let method_kind = 
    if name = Literals.case_set then Case_setter
    else if Ext_string.ends_with name Literals.setter_suffix then Setter
    else Normal name in 
  let len, args = 
    match method_kind with 
    | Setter -> 
      1, [value]
    | (Case_setter | Normal _) -> 
      let args = Ast_util.destruct_tuple_exp value in 
      let arity = List.length args in  
      if method_kind = Case_setter && arity <> 2 then 
        Location.raise_errorf "case_set would expect arity of 2 "
      else  arity, args 
  in
  {e with pexp_desc = 
            Ast_util.gen_fn_run loc len 
              (Exp.mk ~loc @@ Ast_util.down_with_name ~loc obj name)
              args
  }
        (** TODO: 
            More syntax sanity check for [case_set] 
            case_set: arity 2
            _set : arity 1            
            case:
        *)


(* ./dumpast -e ' (Js.Unsafe.(!) obj) # property ' *)
let handle_obj_property loc obj name e 
    (mapper : Ast_mapper.mapper) : Parsetree.expression = 
  let obj = mapper.expr mapper obj in 
  { e with pexp_desc = Ast_util.down_with_name ~loc obj name

  }





(** object 
    for setter : we can push more into [Lsend] and enclose it with a unit type

    for getter :

    (* Invariant: we expect the typechecker & lambda emitter  
       will not do agressive inlining
       Worst things could happen
    {[
      let x = y## case 3  in 
      x 2
    ]}
       in normal case, it should be compiled into Lambda
    {[
      let x = Lsend(y,case, [3]) in 
      Lapp(x,2)
    ]}

       worst:
    {[ Lsend(y, case, [3,2])
    ]}               
       for setter(include case setter), this could 
       be prevented by type system, for getter.

       solution: we can prevent this by rewrite into 
    {[
      Fn.run1  (!x# case) v 
      ]}
       *)

      *)


let rec unsafe_mapper : Ast_mapper.mapper =   
  { Ast_mapper.default_mapper with 
    expr = (fun mapper e -> 
        match e.pexp_desc with 
        (** Its output should not be rewritten anymore *)        
        | Pexp_extension (
            {txt = "bs.raw"; loc} , payload)
          -> 
          Ast_util.handle_raw loc payload
        (** [bs.debugger], its output should not be rewritten any more*)
        | Pexp_extension ({txt = "bs.debugger"; loc} , payload)
          -> {e with pexp_desc = Ast_util.handle_debugger loc payload}
        | Pexp_extension ({txt = "bs.obj"; loc},  payload)
          -> 
            begin match payload with 
            | PStr [{pstr_desc = Pstr_eval (e,_)}]
              -> 
              Ext_ref.protect2 record_as_js_object  obj_type_as_js_obj_type
                (Some as_js_object_attribute ) true
                (fun ()-> mapper.expr mapper e ) 
            | _ -> Location.raise_errorf ~loc "Expect an expression here"
            end
        | Pexp_extension ({txt = "meth"; loc}, payload) 
          -> 
          begin match payload with 
            | PStr [{pstr_desc = 
                       Pstr_eval ({pexp_desc = Pexp_fun("", None, pat, body)} as e, _)
                    }]
              -> 
              let pat = mapper.pat mapper pat in 
              let body = mapper.expr mapper body in 
              {e with pexp_desc = Ast_util.uncurry_method_gen loc pat body}
            | _ -> Location.raise_errorf ~loc "Expect an fun expression here"
          end
        (** End rewriting *)
        | Pexp_fun ("", None, pat , body)
          ->
          let loc = e.pexp_loc in 
          begin match Ext_list.exclude_with_fact (function 
              | {Location.txt = "uncurry"; _}, _ -> true 
              | _ -> false) e.pexp_attributes with 
          | None, _ -> Ast_mapper.default_mapper.expr mapper e 
          | Some _, attrs 
            -> 
            match body.pexp_desc with 
            | Pexp_fun _ -> 
              Location.raise_errorf ~loc 
                {| `fun [@uncurry] (param0, param1) -> `
                   instead of `fun [@uncurry] param0 param1 ->` |}
            | _ -> 
              let pat = mapper.pat mapper pat in 
              let body = mapper.expr mapper body in 
              {e with pexp_desc =  Ast_util.uncurry_fn_gen loc pat body ;
                      pexp_attributes = attrs }
          end
        | Pexp_apply
            (fn, args  )
          -> (** f ## xx a b -->  (f ## x a ) b -- we just pick the first one 
                 f ## xx (a,b)
                 f #.(xx (a,b))
             *)
          let loc = e.pexp_loc in 
          begin match fn with 
            | {pexp_desc = 
                 Pexp_apply (
                   {pexp_desc = 
                      Pexp_ident  {txt = Lident ("##" | "#." as n) ; loc} ; _},
                   [("", obj) ;
                    ("", {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _} )
                   ]);
               _
              } -> 
              begin match args with 
                | [ "", value] -> 
                  if n = "##" then 
                    handle_obj_fn loc obj name value e mapper
                  else 
                    handle_obj_method loc obj name value e mapper
                | _ -> 
                  Location.raise_errorf ~loc
                    "Js object %s expect only one argument when it is a method " n 
              end
            | {pexp_desc = 
                 Pexp_ident  {txt = Lident "##" ; loc} ; _} 
              -> 
              begin match args with 
                | [("", obj) ;
                   ("", {pexp_desc = Pexp_apply(
                        {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _},
                        ["", value]
                      ) })
                  ] -> (* f##(paint (1,2)) *)
                  handle_obj_fn loc obj name value e mapper
                | [("", obj) ;
                   ("", 
                    {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _}
                   )  (* f##paint  *)
                  ] -> handle_obj_property loc obj name e mapper
                | _ -> 
                  Location.raise_errorf ~loc
                    "Js object ## expect syntax like obj##(paint (a,b)) "
              end
            | {pexp_desc =
                 Pexp_ident  {txt = Lident "#." ; loc} ; _}
              -> 
              begin match args with
                | [("", obj) ; (* f#.(paint (1,2)) *)
                   ("", {pexp_desc = Pexp_apply(
                        {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _},
                        ["", value]
                      ) })
                  ] ->
                  handle_obj_method loc obj name value e mapper
                | _ ->
                  Location.raise_errorf ~loc
                    "Js object #. expect syntax like obj#.(paint (a,b)) "
              end
            | _ -> 
              begin match args with 
                | [("", exp)] -> 
             
                  begin match Ext_list.exclude_with_fact (function 
                      | {Location.txt = "uncurry"; _}, _ -> true 
                      | _ -> false) e.pexp_attributes with 
                  | None, _ -> Ast_mapper.default_mapper.expr mapper e 
                  | Some _, attrs -> 
                    let exp = mapper.expr mapper exp in 
                    let fn = mapper.expr mapper fn in 
                    let args = Ast_util.destruct_tuple_exp exp in
                    let len = List.length args in 
                    { e with 
                      pexp_desc = Ast_util.gen_fn_run loc len fn args;
                      pexp_attributes = attrs 
                    } 
                  end
                | _ -> 
                  Ast_mapper.default_mapper.expr mapper e
              end
          end
        | Pexp_record (label_exprs, None)  -> 
            begin match !record_as_js_object with 
            | Some attr 
              (* TODO better error message when [with] detected in [%bs.obj] *)
              -> 
              { e with
                pexp_desc =  handle_record_as_js_object e.pexp_loc attr label_exprs mapper;
              }
            | None -> 
              Ast_mapper.default_mapper.expr  mapper e
            end

        | _ ->  Ast_mapper.default_mapper.expr  mapper e
      );
    typ = (fun self typ -> handle_typ Ast_mapper.default_mapper self typ);
    class_type = 
      (fun self ctyp -> handle_class_obj_typ Ast_mapper.default_mapper self ctyp);
    structure_item = (fun mapper (str : Parsetree.structure_item) -> 
        begin match str.pstr_desc with 
        | Pstr_extension ( ({txt = "bs.raw"; loc}, payload), _attrs) 
          -> 
          Ast_util.handle_raw_structure loc payload
        | _ -> Ast_mapper.default_mapper.structure_item mapper str 
        end
      )
  }




(** global configurations below *)
let common_actions_table : 
  (string *  (Parsetree.expression -> unit)) list = 
  [ "obj_type_auto_uncurry", 
    (fun e -> 
       obj_type_auto_uncurry := Ast_payload.assert_bool_lit e
    )
  ]


let structural_config_table  = 
  String_map.of_list 
    (( "non_export" , 
      (fun e -> non_export := Ast_payload.assert_bool_lit e ))
      :: common_actions_table)

let signature_config_table = 
  String_map.of_list common_actions_table


let make_call_back table ((x : Longident.t Asttypes.loc) , y) = 
  match x with 
  | {txt = Lident name; loc  } -> 
    begin match String_map.find name table with 
    | fn -> fn y
    | exception _ -> Location.raise_errorf ~loc "%s is not supported" name
    end
  | {loc} -> 
    Location.raise_errorf ~loc "invalid label for config"

let rewrite_signature : (Parsetree.signature -> Parsetree.signature) ref = 
  ref (fun  x -> 
      let result = 
        match (x : Parsetree.signature) with 
        | {psig_desc = Psig_attribute ({txt = "bs.config"; loc}, payload); _} :: rest 
          -> 
          begin 
            Ast_payload.as_record_and_process loc payload 
              (make_call_back signature_config_table) ; 
            unsafe_mapper.signature unsafe_mapper rest
          end
        | _ -> 
          unsafe_mapper.signature  unsafe_mapper x in 
      reset (); result 
    )

let rewrite_implementation : (Parsetree.structure -> Parsetree.structure) ref = 
  ref (fun (x : Parsetree.structure) -> 
      let result = 
        match x with 
        | {pstr_desc = Pstr_attribute ({txt = "bs.config"; loc}, payload); _} :: rest 
          -> 
          begin 
            Ast_payload.as_record_and_process loc payload 
              (make_call_back structural_config_table) ; 
            let rest = unsafe_mapper.structure unsafe_mapper rest in
            if !non_export then
              [Str.include_ ~loc  
                 (Incl.mk ~loc 
                    (Mod.constraint_ ~loc
                       (Mod.structure ~loc rest  )
                       (Mty.signature ~loc [])
                    ))]
            else rest 
          end
        | _ -> 
          unsafe_mapper.structure  unsafe_mapper x  in 
      reset (); result )

