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
let bs_class_type = ref false 

let reset () = 
  record_as_js_object := None ;
  obj_type_as_js_obj_type := false ;
  uncurry_type := false ;
  obj_type_auto_uncurry := false ;
  bs_class_type := false;
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


let handle_class_type_field  acc =
  (fun self ({pctf_loc = loc } as ctf : Parsetree.class_type_field) -> 
     match ctf.Parsetree.pctf_desc with 
     | Pctf_method 
         (name, private_flag, virtual_flag, ty) 
       -> 
       let pctf_attributes, st = 
         Ast_util.process_method_attributes_rev ctf.pctf_attributes 
       in 
       begin match st with 
         | {get = None; set = None} -> 
           begin match ty.ptyp_desc with 
             | Ptyp_arrow ("", args, body) 
               -> 
               { ctf with 
                 pctf_desc = 
                   Pctf_method (name, 
                                private_flag,
                                virtual_flag, 
                                Ast_util.destruct_arrow_as_meth_type 
                                  ty.ptyp_loc args body self );
                 pctf_attributes 
               } :: acc 
             | Ptyp_poly (strs, {ptyp_desc = Ptyp_arrow ("", args, body); ptyp_loc})
               -> 
               {ctf with 
                pctf_desc = 
                  Pctf_method 
                    (name,
                     private_flag, 
                     virtual_flag, 
                     {ty with ptyp_desc = 
                                Ptyp_poly(strs,             
                                          Ast_util.destruct_arrow_as_meth_type
                                            ptyp_loc args body self  )});
                pctf_attributes
               }  :: acc 
             | _ -> 
               {ctf with 
                pctf_desc =  Pctf_method (name , private_flag, virtual_flag, self.typ self ty);
                pctf_attributes}
               :: acc 
           end
         | {set = Some _ } 
           -> 
           {ctf with 
            pctf_desc =
              Pctf_method (name ^ Literals.setter_suffix, 
                           private_flag,
                           virtual_flag,
                           Ast_util.destruct_arrow_as_meth_type 
                             loc 
                             ty 
                             (Ast_literal.type_unit ~loc ())
                             self 
                          );
            pctf_attributes}
           :: {ctf with 
               pctf_desc =  
                 Pctf_method (name , 
                              private_flag, 
                              virtual_flag, 
                              self.typ self ty
                             );
               pctf_attributes}
           :: acc 
         (*TODO: test on poly type *)
         | {set = None ; } -> 
           {ctf with 
            pctf_desc =  Pctf_method (name , private_flag, virtual_flag, self.typ self ty);
            pctf_attributes}
           :: acc 
       end
     | Pctf_inherit _ 
     | Pctf_val _ 
     | Pctf_constraint _
     | Pctf_attribute _ 
     | Pctf_extension _  -> 
       Ast_mapper.default_mapper.class_type_field self ctf :: acc 
  )
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
        Ast_util.destruct_arrow_as_meth_callback_type loc args body self         
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
                      | {Location.txt = "bs"; _}, _ -> true
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

let handle_obj_property loc obj name e 
    (mapper : Ast_mapper.mapper) : Parsetree.expression = 
  let obj = mapper.expr mapper obj in 
  { e with pexp_desc = Ast_util.down_with_name ~loc obj name  }




let rec unsafe_mapper : Ast_mapper.mapper =   
  { Ast_mapper.default_mapper with 
    expr = (fun self ({ pexp_loc = loc } as e) -> 
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
                (fun ()-> self.expr self e ) 
            | _ -> Location.raise_errorf ~loc "Expect an expression here"
            end
        (** End rewriting *)
        | Pexp_fun ("", None, pat , body)
          ->
          begin match Ast_util.process_attributes_rev e.pexp_attributes with 
          | _, `Nothing 
            -> Ast_mapper.default_mapper.expr self e 
          |  attrs , `Uncurry
            -> 
            Ast_util.destruct_arrow_as_fn loc pat body self e attrs
          | pexp_attributes, `Meth 
            -> 
            Ast_util.destruct_arrow_as_meth_callbak loc pat body self e pexp_attributes
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
              Ast_util.method_run loc obj name args e self
            | {pexp_desc = 
                 Pexp_apply (
                   {pexp_desc = 
                      Pexp_ident  {txt = Lident "#@"  ; loc} ; _},
                   [("", obj) ;
                    ("", {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _} )
                   ]);
               _} ->  (* f##paint 1 2 *)
              Ast_util.property_run loc obj name args e self

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
                  Ast_util.method_run loc obj name args e self
                | [("", obj) ;
                   ("", 
                    {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _}
                   )  (* f##paint  *)
                  ] -> handle_obj_property loc obj name e self
                | _ -> 
                  Location.raise_errorf ~loc
                    "Js object ## expect syntax like obj##(paint (a,b)) "
              end
            (* we can not use [:=] for precedece cases 
               like {[i @@ x##length := 3 ]} 
               is parsed as {[ (i @@ x##length) := 3]}
            *)
            | {pexp_desc = 
                 Pexp_ident {txt = Lident  "#="}
              } -> 
              begin match args with 
              | ["", 
                  {pexp_desc = 
                     Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "##"}}, 
                                 ["", obj; 
                                  "", {pexp_desc = Pexp_ident {txt = Lident name}}
                                 ]                                 
                                )}; 
                 "", arg
                ] -> 
                Ast_util.method_run loc obj (name ^ Literals.setter_suffix) ["", arg ] e self
              | _ -> Ast_mapper.default_mapper.expr self e 
              end
            | _ -> 

              begin match Ext_list.exclude_with_fact (function 
                  | {Location.txt = "bs"; _}, _ -> true 
                  | _ -> false) e.pexp_attributes with 
              | None, _ -> Ast_mapper.default_mapper.expr self e 
              | Some _, attrs -> 
                Ast_util.fn_run loc fn args self e attrs 
              end
          end
        | Pexp_record (label_exprs, None)  -> 
            begin match !record_as_js_object with 
            | Some attr 
              (* TODO better error message when [with] detected in [%bs.obj] *)
              -> 
              { e with
                pexp_desc =  handle_record_as_js_object e.pexp_loc attr label_exprs self;
              }
            | None -> 
              Ast_mapper.default_mapper.expr  self e
            end

        | _ ->  Ast_mapper.default_mapper.expr self e
      );
    typ = (fun self typ -> handle_typ Ast_mapper.default_mapper self typ);
    class_signature = 
      (fun self ({pcsig_self; pcsig_fields } as csg) -> 
         if !bs_class_type then 
           let pcsig_self = self.typ self pcsig_self in 
           {
             pcsig_self ;
             pcsig_fields = List.fold_right (fun  f  acc ->
               handle_class_type_field  acc self f 
             )  pcsig_fields []
           }
         else 
           Ast_mapper.default_mapper.class_signature self csg 
      );
    structure_item = (fun self (str : Parsetree.structure_item) -> 
        begin match str.pstr_desc with 
        | Pstr_extension ( ({txt = "bs.raw"; loc}, payload), _attrs) 
          -> 
          Ast_util.handle_raw_structure loc payload
        | _ -> Ast_mapper.default_mapper.structure_item self str 
        end
      )
  }




(** global configurations below *)
let common_actions_table : 
  (string *  (Parsetree.expression option -> unit)) list = 
  [ "obj_type_auto_uncurry", 
    (fun e -> 
       obj_type_auto_uncurry := 
         (match e with Some e -> Ast_payload.assert_bool_lit e
         | None -> true)
    ); 
    "bs_class_type", 
    (fun e -> 
       bs_class_type := 
         (match e with Some e -> Ast_payload.assert_bool_lit e 
         | None -> true)
    )
  ]


let structural_config_table  = 
  String_map.of_list 
    (( "non_export" , 
      (fun x -> 
         non_export := (
           match x with 
           |Some e -> Ast_payload.assert_bool_lit e 
           | None -> true)
      ))
      :: common_actions_table)

let signature_config_table : 
  (Parsetree.expression option -> unit) String_map.t= 
  String_map.of_list common_actions_table


let make_call_back table 
    ((x : Longident.t Asttypes.loc) , 
     (y :Parsetree.expression)) = 
  match x with 
  | {txt = Lident name; loc  } -> 
    begin match String_map.find name table with 
      | fn -> 
        let y = 
          match y with 
          | {pexp_desc = Pexp_ident {txt = Lident name2} } when name2 = name -> 
            None 
          | _ -> Some y in
        fn y
      | exception _ -> Location.raise_errorf ~loc "%s is not supported" name
    end
  | _ -> 
    Location.raise_errorf ~loc:x.loc "invalid label for config"

let rewrite_signature : 
  (Parsetree.signature  -> Parsetree.signature) ref = 
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

