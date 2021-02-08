(* Copyright (C) 2020- Authors of ReScript
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

type exp = Parsetree.expression 


(* TODO: 
   have a final checking for property arities 
     [#=], 
*)
let jsInternal = 
  Ast_literal.Lid.js_internal

(* we use the trick 
  [( opaque e : _) ] to avoid it being inspected, 
  the type constraint is avoid some syntactic transformation, e.g ` e |. (f g [@bs])`
  `opaque` is to avoid it being inspected in the type level
*)
let opaque_full_apply ~loc (e : exp) : Parsetree.expression_desc =   
  Pexp_constraint
    (Exp.apply ~loc (Exp.ident {txt = Ast_literal.Lid.js_internal_full_apply; loc})
      [Nolabel,e],
      Typ.any ~loc ()
    )  
let generic_apply loc 
    (self : Bs_ast_mapper.mapper) 
    (obj : Parsetree.expression) 
    (args : Ast_compatible.args) (cb : loc -> exp-> exp)   =
  let obj = self.expr self obj in
  let args =
    Ext_list.map args (fun (lbl,e) -> 
         Bs_syntaxerr.optional_err loc lbl; 
        (lbl,self.expr self e)) in
  let fn = cb loc obj in   
  let args  = 
    match args with 
    | [ Nolabel, {pexp_desc =
           Pexp_construct ({txt = Lident "()"}, None)}]
      -> []
    | _ -> args in
  let arity = List.length args in       
  if arity = 0 then 
    Parsetree.Pexp_apply 
      (Exp.ident {txt = Ldot (jsInternal, "run");loc}, [Nolabel,fn])
  else 
    let arity_s = string_of_int arity in 
    opaque_full_apply ~loc (
      Exp.apply ~loc
        (Exp.apply ~loc
           (Exp.ident ~loc {txt = Ast_literal.Lid.opaque; loc}) 
           [(Nolabel, Exp.field ~loc 
               (Exp.constraint_ ~loc fn 
                  (Typ.constr ~loc {txt = Ldot (Ast_literal.Lid.js_fn, "arity"^arity_s);loc} 
                     [Typ.any ~loc ()])) {txt = Ast_literal.Lid.hidden_field arity_s; loc})]) 
        args)

let method_apply  loc 
    (self : Bs_ast_mapper.mapper) 
    (obj : Parsetree.expression) name
    (args : Ast_compatible.args)   =
    let obj = self.expr self obj in
    let args =
      Ext_list.map args (fun (lbl,e) -> 
           Bs_syntaxerr.optional_err loc lbl; 
          (lbl,self.expr self e)) in
    let fn = Exp.mk ~loc (Ast_util.js_property loc obj name) in   
    let args  = 
      match args with 
      | [ Nolabel, {pexp_desc =
             Pexp_construct ({txt = Lident "()"}, None)}]
        -> []
      | _ -> args in
    let arity = List.length args in       
    if arity = 0 then 
      Parsetree.Pexp_apply 
        (Exp.ident {txt = Ldot ((Ldot (Ast_literal.Lid.js_oo,"Internal")), "run");loc}, [Nolabel,fn])
    else 
      let arity_s = string_of_int arity in 
      opaque_full_apply ~loc (
        Exp.apply ~loc (
          Exp.apply ~loc (Exp.ident ~loc {txt = Ast_literal.Lid.opaque; loc}) 
            [(Nolabel,
              Exp.field ~loc
                (Exp.constraint_ ~loc 
                   fn (Typ.constr ~loc {txt = Ldot (Ast_literal.Lid.js_meth,"arity"^arity_s);loc} [Typ.any ~loc ()]))
                {loc; txt = Ast_literal.Lid.hidden_field arity_s})]) 
          args)
  

let uncurry_fn_apply loc self fn args = 
  generic_apply  loc self fn args (fun _ obj -> obj )

let property_apply loc self obj name args  
  =  generic_apply loc self obj args 
    (fun loc obj -> Exp.mk ~loc (Ast_util.js_property loc obj name))
