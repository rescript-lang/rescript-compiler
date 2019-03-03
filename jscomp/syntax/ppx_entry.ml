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


let record_as_js_object = ref false (* otherwise has an attribute *)
let no_export = ref false

let () =
  Ast_derive_projector.init ();
  Ast_derive_js_mapper.init ()

let reset () =
  record_as_js_object := false ;
  no_export  :=  false

type mapper = Bs_ast_mapper.mapper
let expr_mapper  (self : mapper) (e : Parsetree.expression) =
        match e.pexp_desc with
        (** Its output should not be rewritten anymore *)
        | Pexp_extension extension ->
          Ast_exp_extension.handle_extension record_as_js_object e self extension
        | Pexp_constant (
#if OCAML_VERSION =~ ">4.03.0" then
            Pconst_string
#else            
            Const_string 
#end            
            (s, (Some delim)))
          ->
            Ast_utf8_string_interp.transform e s delim
        (** End rewriting *)
        | Pexp_function cases ->
          (* {[ function [@bs.exn]
                | Not_found -> 0
                | Invalid_argument -> 1
              ]}*)
          begin match
              Ast_attributes.process_pexp_fun_attributes_rev e.pexp_attributes
            with
            | `Nothing, _ ->
              Bs_ast_mapper.default_mapper.expr self  e
            | `Exn, pexp_attributes ->
              Ast_util.convertBsErrorFunction e.pexp_loc self  pexp_attributes cases
          end
        | Pexp_fun (arg_label, _, pat , body)
          when Ast_compatible.is_arg_label_simple arg_label
          ->
          begin match Ast_attributes.process_attributes_rev e.pexp_attributes with
            | Nothing, _
              -> Bs_ast_mapper.default_mapper.expr self e
            | Uncurry _, pexp_attributes
              ->
              {e with
               pexp_desc = Ast_util.to_uncurry_fn e.pexp_loc self pat body  ;
               pexp_attributes}
            | Method _ , _
              ->  Location.raise_errorf ~loc:e.pexp_loc "bs.meth is not supported in function expression"
            | Meth_callback _, pexp_attributes
              ->
              {e with pexp_desc = Ast_util.to_method_callback e.pexp_loc  self pat body ;
                      pexp_attributes }
          end
        | Pexp_apply (fn, args  ) ->
          Ast_exp_apply.handle_exp_apply e self fn args
        | Pexp_record (label_exprs, opt_exp)  ->
          if !record_as_js_object then
            (match opt_exp with
             | None ->
               { e with
                 pexp_desc =
                   Ast_util.record_as_js_object e.pexp_loc self label_exprs;
               }
             | Some e ->
               Location.raise_errorf
                 ~loc:e.pexp_loc "`with` construct is not supported in bs.obj ")
          else
            (* could be supported using `Object.assign`?
               type
               {[
                 external update : 'a Js.t -> 'b Js.t -> 'a Js.t = ""
                 constraint 'b :> 'a
               ]}
            *)
            Bs_ast_mapper.default_mapper.expr  self e
        | Pexp_object {pcstr_self;  pcstr_fields} ->
          begin match Ast_attributes.process_bs e.pexp_attributes with
            | `Has, pexp_attributes
              ->
              {e with
               pexp_desc =
                 Ast_util.ocaml_obj_as_js_object
                   e.pexp_loc self pcstr_self pcstr_fields;
               pexp_attributes
              }
            | `Nothing , _ ->
              Bs_ast_mapper.default_mapper.expr  self e
          end
        | _ ->  Bs_ast_mapper.default_mapper.expr self e


let typ_mapper (self : mapper) typ = 
  Ast_core_type_class_type.handle_core_type self typ record_as_js_object

let class_type_mapper (self : mapper) ({pcty_attributes; pcty_loc} as ctd : Parsetree.class_type) = 
  match Ast_attributes.process_bs pcty_attributes with
  | `Nothing,  _ ->
    Bs_ast_mapper.default_mapper.class_type self ctd
  | `Has, pcty_attributes ->
      (match ctd.pcty_desc with
      | Pcty_signature ({pcsig_self; pcsig_fields })
        ->
        let pcsig_self = self.typ self pcsig_self in
        {ctd with
         pcty_desc = Pcty_signature {
             pcsig_self ;
             pcsig_fields = Ast_core_type_class_type.handle_class_type_fields self pcsig_fields
           };
         pcty_attributes
        }               
#if OCAML_VERSION =~ ">4.03.0" then 
     | Pcty_open _ (* let open M in CT *)
#end
      | Pcty_constr _
      | Pcty_extension _
      | Pcty_arrow _ ->
        Location.raise_errorf ~loc:pcty_loc "invalid or unused attribute `bs`")
        (* {[class x : int -> object
             end [@bs]
           ]}
           Actually this is not going to happpen as below is an invalid syntax
           {[class type x = int -> object
               end[@bs]]}
        *)

let signature_item_mapper (self : mapper) (sigi : Parsetree.signature_item) =        
      match sigi.psig_desc with
      | Psig_type (
#if OCAML_VERSION =~ ">4.03.0" then        
          _rf, 
#end          
           (_ :: _ as tdcls)) ->  (*FIXME: check recursive handling*)
          Ast_tdcls.handleTdclsInSigi self sigi tdcls
      | Psig_value prim when Ast_attributes.process_external prim.pval_attributes
        ->
        Ast_external.handleExternalInSig self prim sigi
      | _ -> Bs_ast_mapper.default_mapper.signature_item self sigi

let structure_item_mapper (self : mapper) (str : Parsetree.structure_item) =
  match str.pstr_desc with
  | Pstr_extension ( ({txt = ("bs.raw"| "raw") ; loc}, payload), _attrs)
    ->
    Ast_util.handle_raw_structure loc payload
  | Pstr_extension (({txt = ("bs.debugger.chrome" | "debugger.chrome") ;loc}, payload),_)
    ->          
    if !Js_config.debug then 
      let open Ast_helper in 
      Str.eval ~loc (Ast_compatible.app1 ~loc 
                       (Exp.ident ~loc {txt = Ldot(Ldot (Lident"Belt","Debug"), "setupChromeDebugger");loc} )
                       (Ast_literal.val_unit ~loc ())
                    )
    else Ast_structure.dummy_item loc
  | Pstr_type (
#if OCAML_VERSION =~ ">4.03.0" then
          _rf, 
#end          
          (_ :: _ as tdcls )) (* [ {ptype_attributes} as tdcl ] *)->
          Ast_tdcls.handleTdclsInStru self str tdcls
   | Pstr_primitive prim when Ast_attributes.process_external prim.pval_attributes
      ->
      Ast_external.handleExternalInStru self prim str
   | _ -> Bs_ast_mapper.default_mapper.structure_item self str


    
let rec unsafe_mapper : mapper =
  { Bs_ast_mapper.default_mapper with
    expr = expr_mapper;
    typ = typ_mapper ;
    class_type = class_type_mapper;      
    signature_item =  signature_item_mapper ;
    value_bindings = Ast_tuple_pattern_flatten.value_bindings_mapper;
    structure_item = structure_item_mapper
  }


type action_table = 
  (Parsetree.expression option -> unit) String_map.t
(** global configurations below *)
let common_actions_table :
  (string *  (Parsetree.expression option -> unit)) list =
  [
  ]


let structural_config_table : action_table =
  String_map.of_list
    (( "no_export" ,
       (fun x ->
          no_export := (
            match x with
            |Some e -> Ast_payload.assert_bool_lit e
            | None -> true)
       ))
     :: common_actions_table)

let signature_config_table : action_table =
  String_map.of_list common_actions_table


let rewrite_signature (x : Parsetree.signature) =  
  let result = 
    match x with
    | {psig_desc = Psig_attribute ({txt = "bs.config"; loc}, payload); _} :: rest
      ->          
      Ext_list.iter (Ast_payload.ident_or_record_as_config loc payload) 
        (Ast_payload.table_dispatch signature_config_table) ;
      unsafe_mapper.signature unsafe_mapper rest          
    | _ ->
      unsafe_mapper.signature  unsafe_mapper x in
  reset ();
  (* Keep this check, since the check is not inexpensive*)
  Bs_ast_invariant.emit_external_warnings_on_signature result;
  result

(* Note we also drop attributes like [@@@bs.deriving ] for convenience*)    
let rewrite_implementation (x : Parsetree.structure) =  
  let result =
    match x with
    | {pstr_desc = Pstr_attribute ({txt = "bs.config"; loc}, payload); _} :: rest
      ->
      begin
        Ext_list.iter (Ast_payload.ident_or_record_as_config loc payload)
          (Ast_payload.table_dispatch structural_config_table) ;
        let rest = unsafe_mapper.structure unsafe_mapper rest in
        if !no_export then
          Ast_helper.[Str.include_ ~loc
             (Incl.mk ~loc
                (Mod.constraint_ ~loc
                   (Mod.structure ~loc rest  )
                   (Mty.signature ~loc [])
                ))]
        else rest
      end
    | _ ->
      unsafe_mapper.structure  unsafe_mapper x  in
  reset ();
  (* Keep this check since it is not inexpensive*)
  Bs_ast_invariant.emit_external_warnings_on_structure result;
  result




