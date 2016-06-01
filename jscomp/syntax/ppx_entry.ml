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
     [%unsafe{| blabla |}]
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








let js_obj_type_id () = 
  if Js_config.get_env () = Browser then
    Ast_literal.Lid.pervasives_js_obj
  else Ast_literal.Lid.js_obj 
    
let curry_type_id () = 
  if Js_config.get_env () = Browser then 
    Ast_literal.Lid.pervasives_uncurry
  else 
    Ast_literal.Lid.js_fn 


open Ast_helper 
let arrow = Ast_helper.Typ.arrow

let record_as_js_object = ref None (* otherwise has an attribute *)
let obj_type_as_js_obj_type = ref false
let uncurry_type = ref false 
let obj_type_auto_uncurry =  ref false

let lift_js_type ~loc  x  = Typ.constr ~loc {txt = js_obj_type_id (); loc} [x]
let lift_curry_type ~loc x  = Typ.constr ~loc {txt = curry_type_id (); loc} [x]

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
  let pval_prim = "" in 
  let pval_attributes = [attr] in 
  let pval_type = 
    let arity = List.length labels in 
    let tyvars = (Ext_list.init arity (fun i ->      
        Typ.var ~loc ("a" ^ string_of_int i))) in 

    let result_type =
      lift_js_type ~loc  
        @@ Typ.object_ ~loc (List.map2 (fun x y -> x ,[], y) labels tyvars) Closed

    in 
    List.fold_right2 
      (fun label tyvar acc -> arrow ~loc label tyvar acc) labels tyvars  result_type
  in 
  let local_module_name = "Tmp" in 
  let local_fun_name = "run" in 
  Ast_comb.create_local_external loc 
    ~pval_prim
    ~pval_type ~pval_attributes 
    ~local_module_name 
    ~local_fun_name
    args 

let gen_fn_run loc arity args  : Parsetree.expression_desc = 
  let pval_prim = Printf.sprintf "js_fn_run_%02d" arity  in
  let tyvars =
    Ext_list.init (arity + 1) 
      (fun i -> Typ.var ~loc ("a" ^ string_of_int i)) in
  let tuple_type_desc = 
    if arity = 0 then 
      (List.hd tyvars).ptyp_desc
      (* avoid single tuple *)
    else 
      Parsetree.Ptyp_tuple tyvars
  in 
  let uncurry_fn = 
    lift_curry_type ~loc @@ Typ.mk ~loc tuple_type_desc in
  (** could be optimized *)
  let pval_type = 
    Ext_list.reduce_from_right (fun a b -> arrow ~loc "" a b) (uncurry_fn :: tyvars) in 
  let local_module_name = "Tmp" in 
  let local_fun_name = "run" in 
  Ast_comb.create_local_external loc ~pval_prim ~pval_type 
    ~local_module_name ~local_fun_name args 

let gen_fn_mk loc arity args  : Parsetree.expression_desc = 
  let open Parsetree in 
  let ptyp_attributes = [] in 
  let pval_prim = Printf.sprintf "js_fn_mk_%02d" arity  in
  let tyvars =
        (Ext_list.init (arity + 1) (fun i -> 
             {ptyp_desc = Ptyp_var ("a" ^ string_of_int i); 
              ptyp_attributes ;
              ptyp_loc = loc})) in
  let tuple_type_desc = 
    if arity = 0 then 
      (List.hd tyvars).ptyp_desc
      (* avoid single tuple *)
    else 
      Parsetree.Ptyp_tuple tyvars
  in 
  let uncurry_fn = 
    lift_curry_type ~loc @@ Typ.mk ~loc tuple_type_desc
  in 
  let arrow = arrow ~loc "" in
  (** could be optimized *)
  let pval_type = 
    if arity = 0 then 
      arrow  (arrow  (Ast_literal.type_unit ~loc ()) (List.hd tyvars) ) uncurry_fn
    else 
      arrow (Ext_list.reduce_from_right arrow tyvars) uncurry_fn in 
  let local_module_name = "Tmp" in 
  let local_fun_name = "mk" in 
  Ast_comb.create_local_external loc ~pval_prim ~pval_type 
    ~local_module_name ~local_fun_name args 
        

let find_uncurry_attrs_and_remove (attrs : Parsetree.attributes ) = 
  Ext_list.exclude_with_fact (function 
    | ({Location.txt  = "uncurry"}, _) -> true 
    | _ -> false ) attrs 


let uncurry_fn_type loc ty attrs
    (args : Parsetree.core_type ) body  : Parsetree.core_type = 

  let fn_type : Parsetree.core_type =
    match args with
    | {ptyp_desc = 
         Parsetree.Ptyp_tuple [arg ; {ptyp_desc = Ptyp_constr ({txt = Lident "__"}, [])} ]; _} 
      ->
      Typ.tuple ~loc ~attrs [ arg ; body]
      
    | {ptyp_desc = Ptyp_tuple args; _} ->
      Typ.tuple ~loc ~attrs (List.rev (body :: List.rev args))
      
    | {ptyp_desc = Ptyp_constr ({txt = Lident "unit"}, []); _} -> body
    | v -> 
      Typ.tuple ~loc ~attrs [v ; body]
  in
  lift_curry_type ~loc fn_type




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
  | {ptyp_attributes ;
     ptyp_desc = Ptyp_arrow ("", args, body);
     ptyp_loc = loc
   } ->
    begin match  find_uncurry_attrs_and_remove ptyp_attributes with 
    | Some _, ptyp_attributes ->
        let args = self.typ self args in
        let body = self.typ self body in
        uncurry_fn_type loc ty ptyp_attributes args body 
    | None, _ -> 
        let args = self.typ self args in
        let body = self.typ self body in
        if !uncurry_type then 
          uncurry_fn_type loc ty ptyp_attributes args body 
        else {ty with ptyp_desc = Ptyp_arrow("", args, body)}
    end
  | {
    ptyp_desc =  Ptyp_object ( methods, closed_flag) ;
    ptyp_attributes ;
    ptyp_loc = loc 
    } -> 
    begin match Ext_list.exclude_with_fact2 
                  (function 
                    | {Location.txt = "bs.obj" ; _}, _  -> true 
                    | _  -> false ) 
                  (function 
                    | {Location.txt = "uncurry"; _}, _ -> true
                    | _ -> false)
                  ptyp_attributes with 
    |  None, None, _  ->
      let methods = 
        List.map (fun (label, ptyp_attrs, core_type ) -> 
            match find_uncurry_attrs_and_remove ptyp_attrs with 
            | None, _ -> label, ptyp_attrs , self.typ self core_type
            | Some v, ptyp_attrs -> 
              label , ptyp_attrs, self.typ self 
                { core_type with ptyp_attributes = v :: core_type.ptyp_attributes}
          ) methods 
      in           
      if !obj_type_as_js_obj_type then 
        lift_js_type ~loc { ty with ptyp_desc = Ptyp_object(methods, closed_flag);
                               ptyp_attributes }

      else 
        {ty with ptyp_desc = Ptyp_object (methods, closed_flag)}
    | fact1 , fact2,  ptyp_attributes -> 
      let obj_type_as_js_obj_type_cxt =  fact1 <> None || !obj_type_as_js_obj_type in
      let uncurry_type_cxt  = fact2 <> None || !uncurry_type in 
      let methods = 
        Ext_ref.protect2
          obj_type_as_js_obj_type
          uncurry_type 
          obj_type_as_js_obj_type_cxt 
          uncurry_type_cxt begin fun _ -> 
          List.map (fun (label, ptyp_attrs, core_type ) -> 
              match find_uncurry_attrs_and_remove ptyp_attrs with 
              | None, _ -> label, ptyp_attrs , self.typ self core_type
              | Some v, ptyp_attrs -> 
                label , ptyp_attrs, self.typ self 
                  { core_type with ptyp_attributes = v :: core_type.ptyp_attributes}
            ) methods 
        end
      in           
      let inner_type = { ty with ptyp_desc = Ptyp_object(methods, closed_flag);
                                 ptyp_attributes } in
      if obj_type_as_js_obj_type_cxt then       
        {ptyp_desc = 
           Ptyp_constr ({ txt = js_obj_type_id () ; loc},
                        [inner_type]);
         ptyp_attributes = [];
         ptyp_loc = loc }
      else inner_type
    end
  | _ -> super.typ self ty

let handle_ctyp 
    (super : Ast_mapper.mapper) 
    (self : Ast_mapper.mapper)
    (ty : Parsetree.class_type) = 
  match ty with
  | {pcty_attributes ;
     pcty_desc ; (* we won't have [ class type v = u -> object[@uncurry] ]*)
     pcty_loc = loc
   } ->
    begin match  find_uncurry_attrs_and_remove pcty_attributes with 
    | Some _, pcty_attributes' ->
      Ext_ref.protect uncurry_type true begin fun () -> 
        self.class_type self  {ty with pcty_attributes = pcty_attributes'} 
      end
    | None, _ -> super.class_type self ty
    end


let handle_debugger loc payload = 
  if Ast_payload.as_empty_structure payload then
    let predef_unit_type = Ast_literal.type_unit ~loc () in
    let pval_prim = "js_debugger" in
    Ast_comb.create_local_external loc 
      ~pval_prim
      ~pval_type:(arrow "" predef_unit_type predef_unit_type)
      [("",  Ast_literal.val_unit ~loc ())]
  else Location.raise_errorf ~loc "bs.raw can only be applied to a string"

(** TODO: Future 
    {[ fun%bs this (a,b,c) -> 
    ]}

    [function] can only take one argument, that is the reason we did not adopt it
*)
let handle_uncurry_generation  loc 
    (pat : Parsetree.pattern)
    (body : Parsetree.expression) 
    (e : Parsetree.expression) (mapper : Ast_mapper.mapper) = 
  let args = 
    match pat with 
    | {ppat_desc = Ppat_tuple [arg ; {ppat_desc = Ppat_var{txt = "__"}} ]; _} -> 
      [arg]
    | {ppat_desc = Ppat_tuple args; _} -> args
    | {ppat_desc = Ppat_construct ({txt = Lident "()"}, None); _} -> []
    | v -> [v]
  in
  let len = List.length args in 
  let body = mapper.expr mapper body in 
  let fun_ = 
    if len = 0 then 
      Ast_comb.fun_no_label ~loc (Ast_literal.pat_unit ~loc () ) body
    else 
      List.fold_right (fun arg body -> 
          let arg = mapper.pat mapper arg in 
          Ast_comb.fun_no_label ~loc arg body 
          ) args body in
  {e with pexp_desc = gen_fn_mk loc len [("", fun_)]}
let handle_uncurry_application 
    loc fn (pat : Parsetree.expression) (e : Parsetree.expression)
    (self : Ast_mapper.mapper) 
  : Parsetree.expression = 
  let args = 
    match pat with 
    | {Parsetree.pexp_desc = 
         Pexp_tuple [arg ; {pexp_desc = Pexp_ident{txt = Lident "__"; _}} ]
      ; _} -> 
      [arg]
    | {pexp_desc = Pexp_tuple args; _} -> args
    | {pexp_desc = Pexp_construct ({txt = Lident "()"}, None); _} -> []
    | v -> [v]
  in

  let fn = self.expr self fn in 
  let args = List.map (self.expr self) args in 
  let len = List.length args in 
  { e with pexp_desc = gen_fn_run loc len (("", fn) :: List.map (fun x -> "", x) args)}

let handle_obj_property loc obj name e 
    (mapper : Ast_mapper.mapper) : Parsetree.expression = 
  (* ./dumpast -e ' (Js.Unsafe.(!) obj) # property ' *)
  let obj = mapper.expr mapper obj in 
  let var = Typ.var ~loc "a" in 
  let down = Ast_comb.create_local_external loc  

    ~pval_prim:"js_unsafe_downgrade"
    ~pval_type:(
      Ast_comb.arrow_no_label ~loc
        (lift_js_type ~loc var) var)
        
    ~local_module_name:"Tmp"
    ~local_fun_name:"cast" ["", obj] in 
  { e with pexp_desc =
     Pexp_send
               ({pexp_desc = down ;
                 pexp_loc = loc;
                 pexp_attributes = []},
                name);
  }


type method_kind = 
  | Case_setter
  | Setter
  | Normal of string 
let handle_obj_method loc (obj : Parsetree.expression) 
    name (value : Parsetree.expression) e 
    (mapper : Ast_mapper.mapper) : Parsetree.expression = 
  let method_kind = 
    if name = Literals.case_set then Case_setter
    else if Ext_string.ends_with name Literals.setter_suffix then Setter
    else Normal name in 
  let args = 
    match method_kind with 
    | Setter -> 
      [value]
    | (Case_setter | Normal _) -> 
      let arity, args = 
        match value with 
        | {pexp_desc = 
             Pexp_tuple 
               [arg ; {pexp_desc = Pexp_ident{txt = Lident "__"; _}} ];
           _} -> 
          1, [arg]
        | {pexp_desc = Pexp_tuple args; _} -> List.length args, args
        | {pexp_desc = 
             Pexp_construct ({txt = Lident "()"}, None);
           _} -> 0, []
        | v -> 1, [v] in 
      if method_kind = Case_setter && arity <> 2 then 
        Location.raise_errorf "case_set would expect arity of 2 "
      else  args 
  in
  let len = List.length args in 
  let obj = mapper.expr mapper obj in 
  let args = List.map (mapper.expr mapper ) args in 
  let var = Typ.var ~loc "a" in 
  let down = Ast_comb.create_local_external loc  
    ~pval_prim:"js_unsafe_downgrade"
    ~pval_type:(Ast_comb.arrow_no_label ~loc
                  (lift_js_type ~loc var)
                  var )
    ~local_module_name:"Tmp"
    ~local_fun_name:"cast" ["", obj] in 
  {e with pexp_desc = gen_fn_run loc len 
    (("",
      {pexp_desc =
         Pexp_send
           ({pexp_desc = down ;
             pexp_loc = loc ;
             pexp_attributes = []},
            name);
       pexp_loc = loc ; 
       pexp_attributes = [] }) :: 
     List.map (fun x -> "", x) args
    )}
        (** TODO: 
            More syntax sanity check for [case_set] 
            case_set: arity 2
            _set : arity 1            
            case:
        *)


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
        (** Begin rewriting [bs.raw], its output should not be rewritten anymore
        *)        
        | Pexp_extension (
            {txt = "bs.raw"; loc} , payload)
          -> 
          begin match Ast_payload.as_string_exp payload with 
            | None -> 
              Location.raise_errorf ~loc "bs.raw can only be applied to a string"
            | Some exp -> 
              let pval_prim = "js_pure_expr" in
              { exp with pexp_desc = Ast_comb.create_local_external loc 
                           ~pval_prim
                           ~pval_type:(arrow "" 
                                         (Ast_literal.type_string ~loc ()) 
                                         (Ast_literal.type_any ~loc ()) )

                           ["",exp]}
          end

        (** End rewriting [bs.raw] *)

        (** Begin rewriting [bs.debugger], its output should not be rewritten any more*)
        | Pexp_extension ({txt = "bs.debugger"; loc} , payload)
          -> {e with pexp_desc = handle_debugger loc payload}
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
            begin match body.pexp_desc with 
              | Pexp_fun _ -> 
                Location.raise_errorf ~loc 
                  {| `fun [@uncurry] (param0, param1) -> `
                     instead of `fun [@uncurry] param0 param1 ->` |}
              | _ -> 
                handle_uncurry_generation loc pat body 
                  {e with pexp_attributes = attrs } mapper
            end
          end

        | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "#@"; loc}},
                      [("", fn);
                       ("", pat)])
          -> 
          handle_uncurry_application loc fn pat e mapper

        | Pexp_apply
            ({pexp_desc = 
               Pexp_apply (
                 {pexp_desc = 
                    Pexp_ident  {txt = Lident "##" ; loc} ; _},
                 [("", obj) ;
                  ("", {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _} )
                 ]);
              _
             }, args  )
          -> (** f ## xx a b -->  (f ## x a ) b -- we just pick the first one *)
          begin match args with 
          | [ "", value] -> 
              handle_obj_method loc obj name value e mapper
          | _ -> 
            Location.raise_errorf 
              "Js object ## expect only one argument when it is a method "
          end
        (* TODO: design: shall we allow 
                               {[ x #.Capital ]}
        *)
        | Pexp_apply ({pexp_desc = 
                         Pexp_ident  {txt = Lident ("#." | "##") ; loc} ; _},
                      [("", obj) ;
                       ("", 
                        ({pexp_desc = Pexp_ident {txt = Lident name;_ } ; _}
                        |{pexp_desc = Pexp_construct ({txt = Lident name;_ }, None) ; _}
                        ) )
                      ])
          -> handle_obj_property loc obj name e mapper
        | Pexp_apply (fn,
                      [("", pat)]) -> 
          let loc = e.pexp_loc in 
          begin match Ext_list.exclude_with_fact (function 
              | {Location.txt = "uncurry"; _}, _ -> true 
              | _ -> false) e.pexp_attributes with 
          | None, _ -> Ast_mapper.default_mapper.expr mapper e 
          | Some _, attrs -> 
            handle_uncurry_application loc fn pat 
              {e with pexp_attributes = attrs} mapper
          end

        | Pexp_record (label_exprs, None)   -> 
          begin match  (* exclude {[ u with ..]} syntax currently *)
              Ext_list.exclude_with_fact 
                (function({Location.txt  = "bs.obj"}, _)  -> true | _ -> false) 
                e.pexp_attributes
            with 
          | Some attr, pexp_attributes -> 
            Ext_ref.protect record_as_js_object (Some attr) begin fun () -> 
              { e with
                pexp_desc =  handle_record_as_js_object e.pexp_loc attr label_exprs mapper;
                pexp_attributes 
              }
            end
          | None , _ -> 
            begin match !record_as_js_object with 
            | Some attr 
              -> 
              { e with
                pexp_desc =  handle_record_as_js_object e.pexp_loc attr label_exprs mapper;
              }
            | None -> 
              Ast_mapper.default_mapper.expr  mapper e
            end
          end
        | _ ->  Ast_mapper.default_mapper.expr  mapper e
      );
    typ = (fun self typ -> handle_typ Ast_mapper.default_mapper self typ);
    class_type = (fun self ctyp -> handle_ctyp Ast_mapper.default_mapper self ctyp);
    structure_item = (fun mapper (str : Parsetree.structure_item) -> 
        begin match str.pstr_desc with 
        | Pstr_extension ( ({txt = "bs.raw"; loc}, payload), _attrs) 
          -> 
            begin match Ast_payload.as_string_exp payload with 
              | Some exp 
                -> 
                let pval_prim = "js_pure_stmt" in 
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
        | _ -> Ast_mapper.default_mapper.structure_item mapper str 
        end
      )
  }


let common_actions_table : 
  (string *  (Parsetree.expression -> unit)) list = 
  [ "obj_type_auto_uncurry", 
    (fun e -> 
       obj_type_as_js_obj_type := Ast_payload.assert_bool_lit e
    )
  ]


let structural_config_table  = 
  String_map.of_list common_actions_table

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
      match (x : Parsetree.signature) with 
      | {psig_desc = Psig_attribute ({txt = "bs.config"; loc}, payload); _} :: rest 
        -> 
        begin 
          Ast_payload.as_record_and_process loc payload 
            (make_call_back signature_config_table) ; 
          unsafe_mapper.signature unsafe_mapper rest
        end
      | _ -> 
        unsafe_mapper.signature  unsafe_mapper x
       )

let rewrite_implementation : (Parsetree.structure -> Parsetree.structure) ref = 
  ref (fun (x : Parsetree.structure) -> 
      match x with 
      | {pstr_desc = Pstr_attribute ({txt = "bs.config"; loc}, payload); _} :: rest 
        -> 
        begin 
          Ast_payload.as_record_and_process loc payload 
            (make_call_back structural_config_table) ; 
          unsafe_mapper.structure unsafe_mapper rest
        end
      | _ -> 
        unsafe_mapper.structure  unsafe_mapper x )

