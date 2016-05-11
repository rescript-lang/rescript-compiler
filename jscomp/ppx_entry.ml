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


let tmp_module_name = "J"
let tmp_fn = "unsafe_expr"
let predef_string_type = 
  Ast_helper.Typ.var "string" 
let predef_any_type = 
  Ast_helper.Typ.any ()
let predef_unit_type = 
  Ast_helper.Typ.var "unit"
let predef_val_unit  = 
  Ast_helper.Exp.construct {txt = Lident "()"; loc = Location.none }  None
let prim = "js_pure_expr"
let prim_stmt = "js_pure_stmt"
let prim_debugger = "js_debugger"


let handle_raw ?ty loc e attrs  = 
  let attrs = 
    match ty with 
    | Some ty -> 
      Parsetree_util.attr_attribute_from_type ty :: attrs  
    | None -> attrs in 
  Ast_helper.Exp.letmodule 
    {txt = tmp_module_name; loc }
    (Ast_helper.Mod.structure [ 
        Ast_helper.Str.primitive 
          (Ast_helper.Val.mk ~attrs {loc ; txt = tmp_fn} 
             ~prim:[prim]
             (Ast_helper.Typ.arrow "" predef_string_type predef_any_type))]
    )    
  @@ 
  let u = (Ast_helper.Exp.apply 
       (Ast_helper.Exp.ident {txt= Ldot(Lident tmp_module_name, tmp_fn) ; loc})
       [("",e)]) in 
  match ty with 
  | Some ty -> 
    Ast_helper.Exp.constraint_ ~loc u 
      ty
  | None -> u 
    

let rec unsafe_mapper : Ast_mapper.mapper =   
  { Ast_mapper.default_mapper with 
    expr = (fun mapper e -> 
        match e.pexp_desc with 
        | Pexp_extension (
            {txt = "bs.raw"; loc} ,
            PStr 
              ( [{ pstr_desc = Pstr_eval ({ 
                   pexp_desc = Pexp_constant (Const_string (_, _)) ;
                   pexp_attributes = attrs } as e ,
                                                _); pstr_loc = _ }]))
          -> 

              handle_raw loc e attrs
        | Pexp_extension( {txt = "bs.raw"; loc}, PStr 
                ( [{ pstr_desc = Parsetree.Pstr_eval ({ 
                      pexp_desc = 
                        Pexp_constraint (
                          {pexp_desc = Pexp_constant (Const_string (_, _)) ; _}
                          as e,
                             ty)
                      ; pexp_attributes = attrs} , _);  }]))
        | Pexp_constraint({pexp_desc = Pexp_extension( {txt = "bs.raw"; loc}, PStr 
                ( [{ pstr_desc = Pstr_eval ({ 
                      pexp_desc = 
                        Pexp_constant (Const_string (_, _)) 
                      ; pexp_attributes = attrs} as e , _);  }]))}, ty)            
              -> handle_raw ~ty loc e attrs
        | Pexp_extension({txt = "bs.raw"; loc}, (PTyp _ | PPat _ | PStr _))
              -> 
              Location.raise_errorf ~loc "bs.raw can only be applied to a string"

        | Pexp_extension ({txt = "bs.debugger"; loc} , payload)
          ->
          begin
            match payload with
            | Parsetree.PStr ( [])
              ->
              Ast_helper.Exp.letmodule
                {txt = tmp_module_name; loc }
                (Ast_helper.Mod.structure [
                    Ast_helper.Str.primitive
                      (Ast_helper.Val.mk {loc ; txt = tmp_fn}
                         ~prim:[prim_debugger]
                         (Ast_helper.Typ.arrow "" predef_unit_type predef_unit_type))]
                )
                (Ast_helper.Exp.apply
                   (Ast_helper.Exp.ident {txt= Ldot(Lident tmp_module_name, tmp_fn) ; loc})
                   [("",  predef_val_unit)])
            | Parsetree.PTyp _
            | Parsetree.PPat (_,_)
            | Parsetree.PStr _
              ->
              Location.raise_errorf ~loc "bs.raw can only be applied to a string"
          end
        | _ ->  Ast_mapper.default_mapper.expr  mapper e
      );
    structure_item = (fun mapper (str : Parsetree.structure_item) -> 
        begin match str.pstr_desc with 
        | Pstr_extension ( ({txt = "bs.raw"; loc}, payload), _attrs) 
          -> 
            begin match payload with 
              | Parsetree.PStr 
                  ( [{ pstr_desc = Parsetree.Pstr_eval ({ 
                        pexp_desc = Pexp_constant (Const_string (cont, opt_label)) ;
                        pexp_loc; pexp_attributes } as e ,_); pstr_loc }])
                -> 
                Ast_helper.Str.eval @@ Ast_helper.Exp.letmodule 
                  {txt = tmp_module_name; loc }
                  (Ast_helper.Mod.structure [ 
                      Ast_helper.Str.primitive 
                        (Ast_helper.Val.mk {loc ; txt = tmp_fn} 
                           ~prim:[prim_stmt]
                           (Ast_helper.Typ.arrow "" predef_string_type predef_any_type))]
                  )    
                  (Ast_helper.Exp.apply 
                     (Ast_helper.Exp.ident {txt= Ldot(Lident tmp_module_name, tmp_fn) ; loc})
                     [("",e)])
              | Parsetree.PTyp _ 
              | Parsetree.PPat (_,_) 
              | Parsetree.PStr _ 
                -> 
                Location.raise_errorf ~loc "bs.raw can only be applied to a string"
            end
        | _ -> Ast_mapper.default_mapper.structure_item mapper str 
        end
      )
  }
let rewrite_signature : (Parsetree.signature -> Parsetree.signature) ref = 
  ref (fun  x -> x )

let rewrite_implementation : (Parsetree.structure -> Parsetree.structure) ref = 
  ref (fun x -> unsafe_mapper.structure  unsafe_mapper x )

