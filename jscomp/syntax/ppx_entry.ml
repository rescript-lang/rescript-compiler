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




let record_as_js_object = ref None (* otherwise has an attribute *)


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
    begin match  Ast_util.process_attributes_rev ptyp_attributes with 
      | ptyp_attributes, `Uncurry ->
        Ast_util.destruct_arrow loc args body self 
      | ptyp_attributes, `Meth -> 
        Ast_util.destruct_arrow_as_meth loc args body self         
      | _, `Nothing -> 
        if !uncurry_type then 
          Ast_util.destruct_arrow loc args body self 
        else 
          Ast_mapper.default_mapper.typ self ty
    end
  | {
    ptyp_desc =  Ptyp_object ( methods, closed_flag) ;
    ptyp_attributes ;
    ptyp_loc = loc 
    } -> 

    let check_auto_uncurry core_type = 
      if  !obj_type_auto_uncurry then
        Ext_ref.protect uncurry_type true (fun _ -> self.typ self core_type  )          
      else self.typ self core_type in 
    let methods, ptyp_attributes  =
      begin match Ext_list.exclude_with_fact
                    (function 
                      | {Location.txt = "fn"; _}, _ -> true
                      | _ -> false)
                    ptyp_attributes with 
      | None, _  ->
        List.map (fun (label, ptyp_attrs, core_type ) -> 
            match Ast_util.process_attributes_rev ptyp_attrs with 
            | _, `Nothing -> 
              label, ptyp_attrs , check_auto_uncurry  core_type
            | ptyp_attrs, `Uncurry  -> 
              label , ptyp_attrs, 
              check_auto_uncurry
                { core_type with 
                  ptyp_attributes = 
                    Ast_util.bs_uncurry_attribute :: core_type.ptyp_attributes}
            | ptyp_attrs, `Meth 
              ->  
              label , ptyp_attrs, 
              check_auto_uncurry
                { core_type with 
                  ptyp_attributes = 
                    Ast_util.bs_meth_attribute :: core_type.ptyp_attributes}
          ) methods , ptyp_attributes
      |  Some _ ,  ptyp_attributes -> 
        Ext_ref.protect uncurry_type true begin fun _ -> 
          List.map (fun (label, ptyp_attrs, core_type ) -> 
              match Ast_util.process_attributes_rev ptyp_attrs with 
              |  _, `Nothing -> label, ptyp_attrs , self.typ self core_type
              | ptyp_attrs, `Uncurry -> 
                label , ptyp_attrs, self.typ self 
                  { core_type with 
                    ptyp_attributes = 
                      Ast_util.bs_uncurry_attribute :: core_type.ptyp_attributes}
              | ptyp_attrs, `Meth -> 
                label , ptyp_attrs, self.typ self 
                  { core_type with 
                    ptyp_attributes = 
                      Ast_util.bs_meth_attribute :: core_type.ptyp_attributes}
            ) methods 
        end, ptyp_attributes 
      end
    in          
    let inner_type =
      { ty
        with ptyp_desc = Ptyp_object(methods, closed_flag);
             ptyp_attributes } in 
    if !obj_type_as_js_obj_type then 
      Ast_util.lift_js_type ~loc inner_type          
    else inner_type
  | _ -> super.typ self ty

let handle_class_obj_typ 
    (super : Ast_mapper.mapper) 
    (self : Ast_mapper.mapper)
    (ty : Parsetree.class_type) = 
  match ty with
  | {pcty_attributes ;
     pcty_desc ; (* we won't have [ class type v = u -> object[@fn] ]*)
     pcty_loc = loc
   } ->
    begin match  Ast_util.process_attributes_rev pcty_attributes with 
    |  pcty_attributes', `Uncurry ->
      Ext_ref.protect uncurry_type true begin fun () -> 
        self.class_type self  {ty with pcty_attributes = pcty_attributes'} 
      end
    |  _, (`Meth | `Nothing) -> 
      if !obj_type_auto_uncurry then 
        Ext_ref.protect uncurry_type true begin fun () -> 
          super.class_type self ty
        end
      else 
        super.class_type self ty
    end




let handle_obj_property loc obj name e 
    (mapper : Ast_mapper.mapper) : Parsetree.expression = 
  let obj = mapper.expr mapper obj in 
  { e with pexp_desc = Ast_util.down_with_name ~loc obj name  }




let rec unsafe_mapper : Ast_mapper.mapper =   
  { Ast_mapper.default_mapper with 
    expr = (fun mapper ({ pexp_loc = loc } as e) -> 
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
                (Some Ast_util.bs_object_attribute ) true
                (fun ()-> mapper.expr mapper e ) 
            | _ -> Location.raise_errorf ~loc "Expect an expression here"
            end
        (** End rewriting *)
        | Pexp_fun ("", None, pat , body)
          ->
          begin match Ast_util.process_attributes_rev e.pexp_attributes with 
          | _, `Nothing 
            -> Ast_mapper.default_mapper.expr mapper e 
          |  attrs , `Uncurry
            -> 
            Ast_util.destruct_arrow_as_fn loc pat body mapper e attrs
          | pexp_attributes, `Meth 
            -> 
            Ast_util.destruct_arrow_as_meth_callbak loc pat body mapper e pexp_attributes
          end
        | Pexp_apply (fn, args  ) ->
          begin match fn with 
            | {pexp_desc = 
                 Pexp_apply (
                   {pexp_desc = 
                      Pexp_ident  {txt = Lident "##"  ; loc} ; _},
                   [("", obj) ;
                    ("", {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _} )
                   ]);
               _} ->  (* f##paint 1 2 *)
              Ast_util.method_run loc obj name args e mapper
            | {pexp_desc = 
                 Pexp_ident  {txt = Lident "##" ; loc} ; _} 
              -> 
              begin match args with 
                | [("", obj) ;
                   ("", {pexp_desc = Pexp_apply(
                        {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _},
                        args
                      ) })
                  ] -> (* f##(paint 1 2 ) *)
                  Ast_util.method_run loc obj name args e mapper
                | [("", obj) ;
                   ("", 
                    {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _}
                   )  (* f##paint  *)
                  ] -> handle_obj_property loc obj name e mapper
                | _ -> 
                  Location.raise_errorf ~loc
                    "Js object ## expect syntax like obj##(paint (a,b)) "
              end
            | _ -> 
              begin match Ext_list.exclude_with_fact (function 
                  | {Location.txt = "fn"; _}, _ -> true 
                  | _ -> false) e.pexp_attributes with 
              | None, _ -> Ast_mapper.default_mapper.expr mapper e 
              | Some _, attrs -> 
                Ast_util.fn_run loc fn args mapper e attrs 
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

