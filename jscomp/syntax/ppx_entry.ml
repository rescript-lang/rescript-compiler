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
let no_export = ref false 

let () = 
  Ast_derive_projector.init ();
  Ast_derive_js_mapper.init ()

let reset () = 
  record_as_js_object := false ;
  no_export  :=  false



let newTdcls 
  (tdcls : Parsetree.type_declaration list) 
  (newAttrs : Parsetree.attributes) : Parsetree.type_declaration list =   
  match tdcls with 
  | [ x ] -> 
    [{ x with Parsetree.ptype_attributes = newAttrs}]
  | _ -> 
    Ext_list.map_last 
      (fun last x -> 
         if last then { x with Parsetree.ptype_attributes = newAttrs} else x )
      tdcls 



let rec unsafe_mapper : Bs_ast_mapper.mapper =   
  { Bs_ast_mapper.default_mapper with 
    expr = (fun self ({ pexp_loc = loc } as e) -> 
        match e.pexp_desc with 
        (** Its output should not be rewritten anymore *)        
        | Pexp_extension (
            {txt = ("bs.raw" | "raw"); loc} , payload)
          -> 
          Ast_util.handle_raw loc payload
        | Pexp_extension (
            {txt = ("bs.re" | "re"); loc} , payload)
          ->
          Exp.constraint_ ~loc
            (Ast_util.handle_raw ~check_js_regex:true loc payload)
            (Ast_comb.to_js_re_type loc)
        | Pexp_extension ({txt = "bs.external" | "external" ; loc }, payload) -> 
          begin match Ast_payload.as_ident payload with 
            | Some {txt = Lident x}
              -> Ast_util.handle_external loc x
            (* do we need support [%external gg.xx ] 

               {[ Js.Undefined.to_opt (if Js.typeof x == "undefined" then x else Js.Undefined.empty ) ]}
            *)

            | None | Some _ -> 
              Location.raise_errorf ~loc 
                "external expects a single identifier"
          end 
        | Pexp_extension ({txt = "bs.time"| "time"; loc}, payload)  
          -> 
          (
            match payload with 
            | PStr [{pstr_desc = Pstr_eval (e,_)}] -> 
              let locString = 
                if loc.loc_ghost then 
                  "GHOST LOC"
                else 
                  let loc_start = loc.loc_start in 
                  let (file, lnum, __) = Location.get_pos_info loc_start in                  
                  Printf.sprintf "%s %d"
                    file lnum in   
              let e = self.expr self e in 
              Exp.sequence ~loc
                (Exp.apply ~loc     
                   (Exp.ident ~loc {loc; 
                                    txt = 
                                      Ldot (Ldot (Lident "Js", "Console"), "timeStart")   
                                   })
                   ["", Exp.constant ~loc (Const_string (locString,None))]
                )     
                ( Exp.let_ ~loc Nonrecursive
                    [Vb.mk ~loc (Pat.var ~loc {loc; txt = "timed"}) e ;
                    ]
                    (Exp.sequence ~loc
                       (Exp.apply ~loc     
                          (Exp.ident ~loc {loc; 
                                           txt = 
                                             Ldot (Ldot (Lident "Js", "Console"), "timeEnd")   
                                          })
                          ["", Exp.constant ~loc (Const_string (locString,None))]
                       )    
                       (Exp.ident ~loc {loc; txt = Lident "timed"})
                    )
                )
            | _ -> 
              Location.raise_errorf 
                ~loc "expect a boolean expression in the payload"
          )
        | Pexp_extension({txt = "bs.assert" | "assert";loc},payload) 
          ->
          (
            match payload with 
            | PStr [ {pstr_desc = Pstr_eval( e,_)}] -> 

              let locString = 
                if loc.loc_ghost then 
                  "ASSERT FAILURE"
                else 
                  let loc_start = loc.loc_start in 
                  let (file, lnum, cnum) = Location.get_pos_info loc_start in
                  let enum = 
                    loc.Location.loc_end.Lexing.pos_cnum -
                    loc_start.Lexing.pos_cnum + cnum in
                  Printf.sprintf "File %S, line %d, characters %d-%d"
                    file lnum cnum enum in   
              let raiseWithString  locString =      
                (Exp.apply ~loc 
                   (Exp.ident ~loc {loc; txt = 
                                           Ldot(Ldot (Lident "Js","Exn"),"raiseError")})
                   ["",

                    Exp.constant (Const_string (locString,None))    
                   ])
              in 
              (match e.pexp_desc with
               | Pexp_construct({txt = Lident "false"},None) -> 
                 (* The backend will convert [assert false] into a nop later *)
                 if !Clflags.no_assert_false  then 
                   Exp.assert_ ~loc 
                     (Exp.construct ~loc {txt = Lident "false";loc} None)
                 else 
                   (raiseWithString locString)
               | Pexp_constant (Const_string (r, _)) -> 
                 if !Clflags.noassert then 
                   Exp.assert_ ~loc (Exp.construct ~loc {txt = Lident "true"; loc} None)
                   (* Need special handling to make it type check*)
                 else   
                   raiseWithString r
               | _ ->    
                 let e = self.expr self  e in 
                 if !Clflags.noassert then 
                   (* pass down so that it still type check, but the backend will
                      make it a nop
                   *)
                   Exp.assert_ ~loc e
                 else 
                   Exp.ifthenelse ~loc
                     (Exp.apply ~loc
                        (Exp.ident {loc ; txt = Ldot(Lident "Pervasives","not")})
                        ["", e]
                     )
                     (raiseWithString locString)
                     None
              )
            | _ -> 
              Location.raise_errorf 
                ~loc "expect a boolean expression in the payload"
          )
          (*
          [%%bs.import Bs_internalAVLSet.(a,b,c)]
          *)
        | Pexp_extension
            ({txt = ("bs.node" | "node"); loc},
             payload)
          ->          
          let strip s =
            match s with 
            | "_module" -> "module" 
            | x -> x  in 
          begin match Ast_payload.as_ident payload with
            | Some {txt = Lident
                        ( "__filename"
                        | "__dirname"
                        | "_module"
                        | "require" as name); loc}
              ->
              let exp =
                Ast_util.handle_external loc (strip name)  in
              let typ =
                Ast_core_type.lift_option_type  
                @@                 
                if name = "_module" then
                  Typ.constr ~loc
                    { txt = Ldot (Lident "Node", "node_module") ;
                      loc} []   
                else if name = "require" then
                  (Typ.constr ~loc
                     { txt = Ldot (Lident "Node", "node_require") ;
                       loc} [] )  
                else
                  Ast_literal.type_string ~loc () in                  
              Exp.constraint_ ~loc exp typ                
            | Some _ | None ->
              begin match payload with 
                | PTyp _ -> 
                  Location.raise_errorf 
                    ~loc "Illegal payload, expect an expression payload instead of type payload"              
                | PPat _ ->
                  Location.raise_errorf 
                    ~loc "Illegal payload, expect an expression payload instead of pattern  payload"        
                | _ -> 
                  Location.raise_errorf 
                    ~loc "Illegal payload"
              end

          end             
        |Pexp_constant (Const_string (s, (Some delim))) 
          ->         
          if Ext_string.equal delim Literals.unescaped_js_delimiter then 
            let js_str = Ast_utf8_string.transform loc s in 
            { e with pexp_desc = 
                       Pexp_constant (Const_string (js_str, Some Literals.escaped_j_delimiter))}
          else if Ext_string.equal delim Literals.unescaped_j_delimiter then 
            Ast_utf8_string_interp.transform_interp loc s             
          else e 

        (** [bs.debugger], its output should not be rewritten any more*)
        | Pexp_extension ({txt = ("bs.debugger"|"debugger"); loc} , payload)
          -> {e with pexp_desc = Ast_util.handle_debugger loc payload}
        | Pexp_extension ({txt = ("bs.obj" | "obj"); loc},  payload)
          -> 
          begin match payload with 
            | PStr [{pstr_desc = Pstr_eval (e,_)}]
              -> 
              Ext_ref.non_exn_protect record_as_js_object true
                (fun () -> self.expr self e ) 
            | _ -> Location.raise_errorf ~loc "Expect an expression here"
          end
        | Pexp_extension({txt ; loc} as lid, PTyp typ) 
          when Ext_string.starts_with txt Literals.bs_deriving_dot -> 
          self.expr self @@ 
          Ast_derive.gen_expression lid typ

        (** End rewriting *)
        | Pexp_function cases -> 
          begin match Ast_attributes.process_pexp_fun_attributes_rev e.pexp_attributes with 
            | `Nothing, _ -> 
              Bs_ast_mapper.default_mapper.expr self  e 
            | `Exn, pexp_attributes -> 
              Ast_util.convertBsErrorFunction loc self  pexp_attributes cases
          end
        | Pexp_fun ("", None, pat , body)
          ->
          begin match Ast_attributes.process_attributes_rev e.pexp_attributes with 
            | `Nothing, _ 
              -> Bs_ast_mapper.default_mapper.expr self e 
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
                      ); pexp_attributes = attrs }
                   (* we should warn when we discard attributes *)
                   )
                  ] -> (* f##(paint 1 2 ) *)
                  (* gpr#1063 foo##(bar##baz) we should rewrite (bar##baz) 
                     first  before pattern match. 
                     currently the pattern match is written in a top down style.
                     Another corner case: f##(g a b [@bs])
                  *)
                  Bs_ast_invariant.warn_unused_attributes attrs ;  
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
               since we allow user to create Js objects in OCaml, it can be of
               ref type
               {[
                 let u = object (self)
                   val x = ref 3 
                   method setX x = self##x := 32
                   method getX () = !self##x
                 end
               ]}
            *)
            | {pexp_desc = 
                 Pexp_ident {txt = Lident  ("#=" )}
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
                  Exp.constraint_ ~loc
                    { e with
                      pexp_desc =
                        Ast_util.method_apply loc self obj 
                          (name ^ Literals.setter_suffix) ["", arg ]  }
                    (Ast_literal.type_unit ~loc ())
                | _ -> Bs_ast_mapper.default_mapper.expr self e 
              end
            | _ -> 
              begin match 
                  Ext_list.exclude_with_val
                    Ast_attributes.is_bs e.pexp_attributes with 
              | false, _ -> Bs_ast_mapper.default_mapper.expr self e 
              | true, pexp_attributes -> 
                {e with pexp_desc = Ast_util.uncurry_fn_apply loc self fn args ;
                        pexp_attributes }
              end
          end
        | Pexp_record (label_exprs, opt_exp)  -> 
          if !record_as_js_object then
            (match opt_exp with
             | None ->              
               { e with
                 pexp_desc =  
                   Ast_util.record_as_js_object loc self label_exprs;
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
                   loc self pcstr_self pcstr_fields;
               pexp_attributes               
              }                          
            | `Nothing , _ ->
              Bs_ast_mapper.default_mapper.expr  self e              
          end            
        | _ ->  Bs_ast_mapper.default_mapper.expr self e
      );
    typ = (fun self typ -> 
      Ast_core_type_class_type.handle_core_type self typ record_as_js_object);
    class_type = 
      (fun self ({pcty_attributes; pcty_loc} as ctd) -> 
         match Ast_attributes.process_bs pcty_attributes with 
         | `Nothing,  _ -> 
           Bs_ast_mapper.default_mapper.class_type
             self ctd 
         | `Has, pcty_attributes ->
           begin match ctd.pcty_desc with
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

             | Pcty_constr _
             | Pcty_extension _ 
             | Pcty_arrow _ ->
               Location.raise_errorf ~loc:pcty_loc "invalid or unused attribute `bs`"
               (* {[class x : int -> object 
                    end [@bs]
                  ]}
                  Actually this is not going to happpen as below is an invalid syntax
                  {[class type x = int -> object
                      end[@bs]]}
               *)
           end             
      );
    signature_item =  begin fun (self : Bs_ast_mapper.mapper) (sigi : Parsetree.signature_item) -> 
      match sigi.psig_desc with 
      | Psig_type (_ :: _ as tdcls) -> 
        begin match Ast_attributes.process_derive_type 
                      (Ext_list.last tdcls).ptype_attributes  with 
        | {bs_deriving = Some actions; explict_nonrec}, newAttrs
          -> 
          let loc = sigi.psig_loc in 
          let newTdcls = newTdcls tdcls newAttrs in             
          let newSigi = 
            self.signature_item self {sigi with psig_desc = Psig_type newTdcls} in 
          if Ast_payload.isAbstract actions then 
            let  codes = Ast_derive_abstract.handleTdclsInSig newTdcls in 
            Ast_signature.fuseAll ~loc             
            (
              Sig.include_ ~loc 
              (Incl.mk ~loc 
                (Mty.typeof_ ~loc (Mod.constraint_ ~loc 
                  (Mod.structure ~loc [
                     { pstr_loc = loc; pstr_desc = Pstr_type (match newSigi.psig_desc with Psig_type x -> x | _ -> assert false)
                   }] ) 
                (Mty.signature ~loc [])) ) )
              ::
              self.signature self 
              codes
            )
          else 

            Ast_signature.fuseAll ~loc 
              (newSigi::
                self.signature 
                 self 
                 ( 
                   Ast_derive.gen_signature tdcls actions explict_nonrec))
        | {bs_deriving = None }, _  -> 
          Bs_ast_mapper.default_mapper.signature_item self sigi 

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
        let pval_attributes = self.attributes self pval_attributes in         
        let pval_type, pval_prim, pval_attributes = 
          match pval_prim with 
          | [ v ] -> 
            External_process.handle_attributes_as_string
              pval_loc 
              pval_name.txt 
              pval_type 
              pval_attributes v
          | _ -> 
            Location.raise_errorf 
              ~loc:pval_loc
              "only a single string is allowed in bs external" in
        {sigi with 
         psig_desc = 
           Psig_value
             {prim with
              pval_type ; 
              pval_prim ;
              pval_attributes 
             }}

      | _ -> Bs_ast_mapper.default_mapper.signature_item self sigi
    end;
    pat = begin fun self (pat : Parsetree.pattern) -> 
      match pat with 
      | { ppat_desc = Ppat_constant(Const_string (_, Some "j")); ppat_loc = loc} -> 
        Location.raise_errorf ~loc 
          "Unicode string is not allowed in pattern match"
      | _  -> Bs_ast_mapper.default_mapper.pat self pat

    end;
    value_bindings = Ast_tuple_pattern_flatten.handle_value_bindings;
    structure_item = begin fun self (str : Parsetree.structure_item) -> 
      begin match str.pstr_desc with 
        | Pstr_extension ( ({txt = ("bs.raw"| "raw") ; loc}, payload), _attrs) 
          -> 
          Ast_util.handle_raw_structure loc payload
        | Pstr_type (_ :: _ as tdcls ) (* [ {ptype_attributes} as tdcl ] *)-> 
          begin match Ast_attributes.process_derive_type 
                        ((Ext_list.last tdcls).ptype_attributes) with 
          | {bs_deriving = Some actions;
             explict_nonrec 
            }, newAttrs ->                         
            let loc = str.pstr_loc in      
            let tdcls2 = newTdcls tdcls newAttrs in 
            let newStr = 
              self.structure_item self 
                {str with pstr_desc = Pstr_type tdcls2} in 
            if Ast_payload.isAbstract actions then 
              let codes = Ast_derive_abstract.handleTdclsInStr tdcls2 in 
              (* use [tdcls2] avoid nonterminating *)
              Ast_structure.fuseAll ~loc 
                ( 
                  Ast_structure.constraint_ ~loc
                    [newStr] []::
                  self.structure self 
                    codes)
            else
              Ast_structure.fuseAll ~loc                                
                (newStr :: 
                 self.structure self 
                   (
                     List.map 
                       (fun action -> 
                          Ast_derive.gen_structure_signature 
                            loc
                            tdcls action explict_nonrec
                       )    actions
                   ))
          | {bs_deriving = None }, _  -> 
            Bs_ast_mapper.default_mapper.structure_item self str
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
          let pval_attributes = self.attributes self pval_attributes in         
          let pval_type, pval_prim, pval_attributes = 
            match pval_prim with 
            | [ v] -> 
              External_process.handle_attributes_as_string
                pval_loc
                pval_name.txt
                pval_type pval_attributes v

            | _ -> Location.raise_errorf 
                     ~loc:pval_loc "only a single string is allowed in bs external" in
          {str with 
           pstr_desc = 
             Pstr_primitive
               {prim with
                pval_type ; 
                pval_prim;
                pval_attributes 
               }}
        | _ -> Bs_ast_mapper.default_mapper.structure_item self str 
      end
    end
  }




(** global configurations below *)
let common_actions_table : 
  (string *  (Parsetree.expression option -> unit)) list = 
  [ 
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

let dummy_unused_attribute : Warnings.t = (Bs_unused_attribute "")

let rewrite_signature : 
  (Parsetree.signature  -> Parsetree.signature) ref = 
  ref (fun  x -> 
      let result = 
        match (x : Parsetree.signature) with 
        | {psig_desc = Psig_attribute ({txt = "bs.config"; loc}, payload); _} :: rest 
          -> 
          begin 
            Ast_payload.ident_or_record_as_config loc payload 
            |> List.iter (Ast_payload.table_dispatch signature_config_table) ; 
            unsafe_mapper.signature unsafe_mapper rest
          end
        | _ -> 
          unsafe_mapper.signature  unsafe_mapper x in 
      reset ();
      (* Keep this check, since the check is not inexpensive*)
      if Warnings.is_active dummy_unused_attribute then 
        Bs_ast_invariant.emit_external_warnings.signature Bs_ast_invariant.emit_external_warnings result ;
      result 
    )

let rewrite_implementation : (Parsetree.structure -> Parsetree.structure) ref = 
  ref (fun (x : Parsetree.structure) -> 
      let result = 
        match x with 
        | {pstr_desc = Pstr_attribute ({txt = "bs.config"; loc}, payload); _} :: rest 
          -> 
          begin 
            Ast_payload.ident_or_record_as_config loc payload 
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
      reset (); 
      (* Keep this check since it is not inexpensive*)    
      (if Warnings.is_active dummy_unused_attribute then 
         Bs_ast_invariant.emit_external_warnings.structure Bs_ast_invariant.emit_external_warnings result);
      result 
    )

