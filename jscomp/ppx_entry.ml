(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)

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
let prim = "js_pure_expr"
let prim_stmt = "js_pure_stmt"
let rec unsafe_mapper : Ast_mapper.mapper =   
  { Ast_mapper.default_mapper with 
    expr = (fun mapper e -> 
        match e.pexp_desc with 
        | Pexp_extension ({txt = "bb.unsafe"; loc} , payload)
          -> 
          begin 
            match payload with
            | Parsetree.PStr 
                ( [{ pstr_desc = Parsetree.Pstr_eval ({ 
                      pexp_desc = Pexp_constant (Const_string (cont, opt_label)) ;
                      pexp_loc; pexp_attributes } as e ,_); pstr_loc }])
              -> 
              Ast_helper.Exp.letmodule 
                {txt = tmp_module_name; loc }
                (Ast_helper.Mod.structure [ 
                    Ast_helper.Str.primitive 
                      (Ast_helper.Val.mk {loc ; txt = tmp_fn} 
                         ~prim:[prim]
                         (Ast_helper.Typ.arrow "" predef_string_type predef_any_type))]
                )    
                (Ast_helper.Exp.apply 
                   (Ast_helper.Exp.ident {txt= Ldot(Lident tmp_module_name, tmp_fn) ; loc})
                   [("",e)])
            | Parsetree.PTyp _ 
            | Parsetree.PPat (_,_) 
            | Parsetree.PStr _ 
              -> 
              Location.raise_errorf ~loc "bb.unsafe can only be applied to a string"
          end
        | _ ->  Ast_mapper.default_mapper.expr  mapper e
      );
    structure_item = (fun mapper (str : Parsetree.structure_item) -> 
        begin match str.pstr_desc with 
        | Pstr_extension ( ({txt = "bb.unsafe"; loc}, payload), _attrs) 
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
                Location.raise_errorf ~loc "bb.unsafe can only be applied to a string"

            end
        | _ -> Ast_mapper.default_mapper.structure_item mapper str 
        end
      )
  }
let rewrite_signature : (Parsetree.signature -> Parsetree.signature) ref = 
  ref (fun  x -> x )

let rewrite_implementation : (Parsetree.structure -> Parsetree.structure) ref = 
  ref (fun x -> unsafe_mapper.structure  unsafe_mapper x )

