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




let record_as_js_object = ref false (* otherwise has an attribute *)


let obj_type_as_js_obj_type = ref false
let uncurry_type = ref false 
let obj_type_auto_uncurry =  ref false
let no_export = ref false 
let bs_class_type = ref false 

let reset () = 
  record_as_js_object := false ;
  obj_type_as_js_obj_type := false ;
  uncurry_type := false ;
  obj_type_auto_uncurry := false ;
  bs_class_type := false;
  no_export  :=  false




let handle_class_type_field  acc =
  (fun self ({pctf_loc = loc } as ctf : Parsetree.class_type_field) -> 
     match ctf.Parsetree.pctf_desc with 
     | Pctf_method 
         (name, private_flag, virtual_flag, ty) 
       -> 
       begin match Ast_attributes.process_method_attributes_rev ctf.pctf_attributes with 
         | {get = None; set = None}, _  -> 
           let ty = 
             match ty.ptyp_desc with 
             | Ptyp_arrow (label, args, body) 
               ->
               Ast_util.to_method_type
                 ty.ptyp_loc  self label args body

             | Ptyp_poly (strs, {ptyp_desc = Ptyp_arrow (label, args, body);
                                 ptyp_loc})
               ->
               {ty with ptyp_desc = 
                          Ptyp_poly(strs,             
                                    Ast_util.to_method_type
                                      ptyp_loc  self label args body  )}
             | _ -> 
               self.typ self ty
           in 
           {ctf with 
            pctf_desc = 
              Pctf_method (name , private_flag, virtual_flag, ty)}
           :: acc 

         | st , pctf_attributes
           -> 
           let get_acc = 
             match st.set with 
             | Some `No_get -> acc 
             | None 
             | Some `Get -> 
               let lift txt = 
                 Typ.constr ~loc {txt ; loc} [ty] in
               let (null,undefined) =                
                 match st with 
                 | {get = Some (null, undefined) } -> (null, undefined)
                 | {get = None} -> (false, false ) in 
               let ty = 
                 match (null,undefined) with 
                 | false, false -> ty
                 | true, false -> lift Ast_literal.Lid.js_null
                 | false, true -> lift Ast_literal.Lid.js_undefined
                 | true , true -> lift Ast_literal.Lid.js_null_undefined in 
               {ctf with 
                pctf_desc =  
                  Pctf_method (name , 
                               private_flag, 
                               virtual_flag, 
                               self.typ self ty
                              );
                pctf_attributes}
               :: acc  
           in 
           if st.set = None then get_acc 
           else 
             {ctf with 
              pctf_desc =
                Pctf_method (name ^ Literals.setter_suffix, 
                             private_flag,
                             virtual_flag,
                             Ast_util.to_method_type
                               loc self "" ty
                               (Ast_literal.type_unit ~loc ())
                            );
              pctf_attributes}
             :: get_acc 
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
    Ext_ref.non_exn_protect obj_type_as_js_obj_type true 
      (fun _ -> self.typ self ty )
  | {ptyp_attributes ;
     ptyp_desc = Ptyp_arrow (label, args, body);
     (* let it go without regard label names, 
        it will report error later when the label is not empty
     *)     
     ptyp_loc = loc
   } ->
    begin match  Ast_attributes.process_attributes_rev ptyp_attributes with 
      | `Uncurry , ptyp_attributes ->
        Ast_util.to_uncurry_type loc self label args body 
      |  `Meth_callback, ptyp_attributes ->
        Ast_util.to_method_callback_type loc self label args body
      | `Method, ptyp_attributes ->
        Ast_util.to_method_type loc self label args body
      | `Nothing , _ -> 
        if !uncurry_type then 
          Ast_util.to_uncurry_type loc  self  label args body
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
        Ext_ref.non_exn_protect uncurry_type true 
          (fun _ -> self.typ self core_type  )          
      else self.typ self core_type in 
    let methods, ptyp_attributes  =
      begin match Ext_list.exclude_with_fact
                    (function 
                      | {Location.txt = "bs"; _}, _ -> true
                      | _ -> false)
                    ptyp_attributes with 
      | None, _  ->
        List.map (fun (label, ptyp_attrs, core_type ) -> 
            match Ast_attributes.process_attributes_rev ptyp_attrs with 
            | `Nothing,  _ -> 
              label, ptyp_attrs , check_auto_uncurry  core_type
            |  `Uncurry, _  -> 
              label , ptyp_attrs, 
              check_auto_uncurry
                { core_type with 
                  ptyp_attributes = 
                    Ast_attributes.bs :: core_type.ptyp_attributes}
            | `Method, ptyp_attrs 
              ->  
              label , ptyp_attrs, 
              check_auto_uncurry
                { core_type with 
                  ptyp_attributes = 
                    Ast_attributes.bs_method :: core_type.ptyp_attributes}
            | `Meth_callback, ptyp_attrs 
              ->  
              label , ptyp_attrs, 
              check_auto_uncurry
                { core_type with 
                  ptyp_attributes = 
                    Ast_attributes.bs_this :: core_type.ptyp_attributes}
          ) methods , ptyp_attributes
      |  Some _ ,  ptyp_attributes -> 
        Ext_ref.non_exn_protect uncurry_type true begin fun _ -> 
          List.map (fun (label, ptyp_attrs, core_type ) -> 
              match Ast_attributes.process_attributes_rev ptyp_attrs with 
              | `Nothing,  _ -> label, ptyp_attrs , self.typ self core_type
              |  `Uncurry, ptyp_attrs -> 
                label , ptyp_attrs, self.typ self 
                  { core_type with 
                    ptyp_attributes = 
                      Ast_attributes.bs :: core_type.ptyp_attributes}
              |  `Method, ptyp_attrs -> 
                label , ptyp_attrs, self.typ self 
                  { core_type with 
                    ptyp_attributes = 
                      Ast_attributes.bs_method :: core_type.ptyp_attributes}
              |  `Meth_callback, ptyp_attrs -> 
                label , ptyp_attrs, self.typ self 
                  { core_type with 
                    ptyp_attributes = 
                      Ast_attributes.bs_this :: core_type.ptyp_attributes}
            ) methods 
        end, ptyp_attributes 
      end
    in          
    let inner_type =
      { ty
        with ptyp_desc = Ptyp_object(methods, closed_flag);
             ptyp_attributes } in 
    if !obj_type_as_js_obj_type then 
      Ast_comb.to_js_type loc inner_type          
    else inner_type
  | _ -> super.typ self ty





let rec unsafe_mapper : Ast_mapper.mapper =   
  { Ast_mapper.default_mapper with 
    expr = (fun self ({ pexp_loc = loc } as e) -> 
        match e.pexp_desc with 
        (** Its output should not be rewritten anymore *)        
        | Pexp_extension (
            {txt = "bs.raw"; loc} , payload)
          -> 
          Ast_util.handle_raw loc payload
        | Pexp_extension (
            {txt = "bs.re"; loc} , payload)
          -> 
          Ast_util.handle_regexp loc payload

        (** [bs.debugger], its output should not be rewritten any more*)
        | Pexp_extension ({txt = "bs.debugger"; loc} , payload)
          -> {e with pexp_desc = Ast_util.handle_debugger loc payload}
        | Pexp_extension ({txt = "bs.obj"; loc},  payload)
          -> 
            begin match payload with 
            | PStr [{pstr_desc = Pstr_eval (e,_)}]
              -> 
              Ext_ref.non_exn_protect2 
                record_as_js_object
                obj_type_as_js_obj_type
                true
                true
                (fun () -> self.expr self e ) 
            | _ -> Location.raise_errorf ~loc "Expect an expression here"
            end
        | Pexp_extension({txt ; loc}, PTyp typ) 
          when Ext_string.starts_with txt Literals.bs_deriving_dot -> 
          self.expr self @@ 
          (Ast_payload.table_dispatch 
            Ast_derive.derive_table 
            ({loc ;
              txt =
                Lident 
                  (Ext_string.tail_from txt (String.length Literals.bs_deriving_dot))}, None)).expression_gen typ
            
        (** End rewriting *)
        | Pexp_fun ("", None, pat , body)
          ->
          begin match Ast_attributes.process_attributes_rev e.pexp_attributes with 
          | `Nothing, _ 
            -> Ast_mapper.default_mapper.expr self e 
          |   `Uncurry, pexp_attributes
            -> 
            {e with 
             pexp_desc = Ast_util.to_uncurry_fn loc self pat body  ;
             pexp_attributes}
          | `Method , _
            ->  Location.raise_errorf ~loc "bs.meth is not supported in function expression"
          | `Meth_callback , pexp_attributes
            -> 
            {e with pexp_desc = Ast_util.to_method_callback loc  self pat body ;
                    pexp_attributes }
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
              {e with pexp_desc = Ast_util.method_apply loc self obj name args }
            | {pexp_desc = 
                 Pexp_apply (
                   {pexp_desc = 
                      Pexp_ident  {txt = Lident "#@"  ; loc} ; _},
                   [("", obj) ;
                    ("", {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _} )
                   ]);
               _} ->  (* f##paint 1 2 *)
              {e with pexp_desc = Ast_util.property_apply loc self obj name args  }

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
                  {e with pexp_desc = Ast_util.method_apply loc self obj name args}
                | [("", obj) ;
                   ("", 
                    {pexp_desc = Pexp_ident {txt = Lident name;_ } ; _}
                   )  (* f##paint  *)
                  ] -> 
                  { e with pexp_desc = 
                             Ast_util.js_property loc (self.expr self obj) name  
                  }

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
                 { e with
                   pexp_desc =
                     Ast_util.method_apply loc self obj 
                       (name ^ Literals.setter_suffix) ["", arg ]  }
              | _ -> Ast_mapper.default_mapper.expr self e 
              end
            | _ -> 

              begin match Ext_list.exclude_with_fact (function 
                  | {Location.txt = "bs"; _}, _ -> true 
                  | _ -> false) e.pexp_attributes with 
              | None, _ -> Ast_mapper.default_mapper.expr self e 
              | Some _, pexp_attributes -> 
                {e with pexp_desc = Ast_util.uncurry_fn_apply loc self fn args ;
                        pexp_attributes }
              end
          end
        | Pexp_record (label_exprs, None)  -> 
          (* TODO better error message when [with] detected in [%bs.obj] *)
          if !record_as_js_object then
            { e with
              pexp_desc =  
                Ast_util.record_as_js_object loc self label_exprs;
            }
          else 
            Ast_mapper.default_mapper.expr  self e
        | _ ->  Ast_mapper.default_mapper.expr self e
      );
    typ = (fun self typ -> handle_typ Ast_mapper.default_mapper self typ);
    class_type = 
      (fun self ({pcty_attributes} as ctd) -> 
         match Ast_attributes.process_class_type_decl_rev 
                 pcty_attributes with 
         | `Nothing,  _ -> 
           Ast_mapper.default_mapper.class_type
             self ctd 
         | `Has, pcty_attributes -> 
           Ext_ref.non_exn_protect bs_class_type true begin fun _ -> 
             Ast_mapper.default_mapper.class_type
               self {ctd with pcty_attributes}
           end
      );
    class_signature = begin fun self ({pcsig_self; pcsig_fields } as csg) -> 
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
    end;
    signature_item =  begin fun (self : Ast_mapper.mapper) (sigi : Parsetree.signature_item) -> 
      match sigi.psig_desc with 
      | Psig_type [{ptype_attributes} as tdcl] -> 
        begin match Ast_attributes.process_derive_type ptype_attributes with 
        | {bs_deriving = `Has_deriving actions; explict_nonrec}, ptype_attributes
          -> Ast_signature.fuse 
               {sigi with 
                psig_desc = Psig_type [self.type_declaration self {tdcl with ptype_attributes}]
               }
               (self.signature 
                  self @@ 
                Ast_derive.type_deriving_signature tdcl actions explict_nonrec)
        | {bs_deriving = `Nothing }, _ -> 
          {sigi with psig_desc = Psig_type [ self.type_declaration self tdcl] } 
        end
      | Psig_value
          ({pval_attributes; 
            pval_type; 
            pval_loc;
            pval_prim;
            pval_name ;
           } as prim) 
        when Ast_attributes.process_external pval_attributes
        -> 
        let pval_type = self.typ self pval_type in 
        let pval_attributes =
          (Ast_attributes.mk_bs_type ~loc:pval_loc pval_type)
          :: pval_attributes in
        let pval_type, pval_prim = 
          match pval_prim with 
          | [ v ] -> 
            Ast_external_attributes.handle_attributes_as_string
              pval_loc 
              pval_name.txt 
              pval_type 
              pval_attributes v
          | _ -> Location.raise_errorf "only a single string is allowed in bs external" in
        {sigi with 
         psig_desc = 
           Psig_value
             {prim with
              pval_type ; 
              pval_prim ;
              pval_attributes 
                 }}

      | _ -> Ast_mapper.default_mapper.signature_item self sigi
    end;
    structure_item = begin fun self (str : Parsetree.structure_item) -> 
        begin match str.pstr_desc with 
        | Pstr_extension ( ({txt = "bs.raw"; loc}, payload), _attrs) 
          -> 
          Ast_util.handle_raw_structure loc payload
        | Pstr_type [ {ptype_attributes} as tdcl ]-> 
          begin match Ast_attributes.process_derive_type ptype_attributes with 
          | {bs_deriving = `Has_deriving actions;
             explict_nonrec 
            }, ptype_attributes -> 
            Ast_structure.fuse 
              {str with 
               pstr_desc =
                 Pstr_type 
                   [ self.type_declaration self {tdcl with ptype_attributes}]}
              (self.structure self @@ Ast_derive.type_deriving_structure
                 tdcl actions explict_nonrec )
          | {bs_deriving = `Nothing}, _  -> 
            {str with 
             pstr_desc = 
               Pstr_type
                 [ self.type_declaration self tdcl]}
          end
        | Pstr_primitive 
            ({pval_attributes; 
              pval_prim; 
              pval_type;
              pval_name;
              pval_loc} as prim) 
          when Ast_attributes.process_external pval_attributes
          -> 
          let pval_type = self.typ self pval_type in 
          let pval_type, pval_prim = 
            match pval_prim with 
            | [ v] -> 
              Ast_external_attributes.handle_attributes_as_string
                pval_loc
                pval_name.txt
                pval_type pval_attributes v

            | _ -> Location.raise_errorf "only a single string is allowed in bs external" in
          {str with 
           pstr_desc = 
             Pstr_primitive
               {prim with
                pval_type ; 
                pval_prim;
                pval_attributes 
               }}
          
        | _ -> Ast_mapper.default_mapper.structure_item self str 
        end
    end
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
    (( "no_export" , 
      (fun x -> 
         no_export := (
           match x with 
           |Some e -> Ast_payload.assert_bool_lit e 
           | None -> true)
      ))
      :: common_actions_table)

let signature_config_table : 
  (Parsetree.expression option -> unit) String_map.t= 
  String_map.of_list common_actions_table



let rewrite_signature : 
  (Parsetree.signature  -> Parsetree.signature) ref = 
  ref (fun  x -> 
      let result = 
        match (x : Parsetree.signature) with 
        | {psig_desc = Psig_attribute ({txt = "bs.config"; loc}, payload); _} :: rest 
          -> 
          begin 
            Ast_payload.as_record_and_process loc payload 
            |> List.iter (Ast_payload.table_dispatch signature_config_table) ; 
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
            |> List.iter (Ast_payload.table_dispatch structural_config_table) ; 
            let rest = unsafe_mapper.structure unsafe_mapper rest in
            if !no_export then
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

