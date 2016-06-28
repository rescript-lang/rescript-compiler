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

let create_local_external loc 
     ?(pval_attributes=[])
     ~pval_prim
     ~pval_type 
     ?(local_module_name = "J")
     ?(local_fun_name = "unsafe_expr")
     args
  : Parsetree.expression_desc = 
  Pexp_letmodule
    ({txt = local_module_name; loc},
     {pmod_desc =
        Pmod_structure
          [{pstr_desc =
              Pstr_primitive
                {pval_name = {txt = local_fun_name; loc};
                 pval_type ;
                 pval_loc = loc;
                 pval_prim ;
                 pval_attributes };
            pstr_loc = loc;
           }];
      pmod_loc = loc;
      pmod_attributes = []},
     {
       pexp_desc =
         Pexp_apply
           (({pexp_desc = Pexp_ident {txt = Ldot (Lident local_module_name, local_fun_name); 
                                      loc};
              pexp_attributes = [] ;
              pexp_loc = loc} : Parsetree.expression),
            args);
       pexp_attributes = [];
       pexp_loc = loc
     })

let local_extern_cont loc 
     ?(pval_attributes=[])
     ~pval_prim
     ~pval_type 
     ?(local_module_name = "J")
     ?(local_fun_name = "unsafe_expr")
     (cb : Parsetree.expression -> 'a) 
  : Parsetree.expression_desc = 
  Pexp_letmodule
    ({txt = local_module_name; loc},
     {pmod_desc =
        Pmod_structure
          [{pstr_desc =
              Pstr_primitive
                {pval_name = {txt = local_fun_name; loc};
                 pval_type ;
                 pval_loc = loc;
                 pval_prim ;
                 pval_attributes };
            pstr_loc = loc;
           }];
      pmod_loc = loc;
      pmod_attributes = []},
     cb {pexp_desc = Pexp_ident {txt = Ldot (Lident local_module_name, local_fun_name); 
                                 loc};
         pexp_attributes = [] ;
         pexp_loc = loc}
)

open Ast_helper 

let exp_apply_no_label ?loc ?attrs a b = 
  Exp.apply ?loc ?attrs a (List.map (fun x -> "", x) b)

let fun_no_label ?loc ?attrs  pat body = 
  Exp.fun_ ?loc ?attrs "" None pat body

let arrow_no_label ?loc ?attrs b c = 
  Typ.arrow ?loc ?attrs "" b c 

let discard_exp_as_unit loc e = 
  exp_apply_no_label ~loc     
    (Exp.ident ~loc {txt = Ast_literal.Lid.ignore_id; loc})
    [Exp.constraint_ ~loc e 
       (Ast_literal.type_unit ~loc ())]


let tuple_type_pair ?loc kind arity = 
  let prefix  = "a" in
  if arity = 0 then 
    let ty = Typ.var ?loc ( prefix ^ "0") in 
    match kind with 
    | `Run -> ty, ty 
    | `Make -> 
      (Typ.arrow "" ?loc
         (Ast_literal.type_unit ?loc ())
         ty ,
       ty)
  else
    let tys = Ext_list.init (arity + 1) (fun i -> 
        Typ.var ?loc (prefix ^ string_of_int i)
      )  in
    (Ext_list.reduce_from_right (fun x y -> Typ.arrow "" ?loc x y) tys,
     Typ.tuple ?loc  tys)
    


let obj_type_pair ?loc arity = 
  let obj = Typ.var ?loc "obj" in 
  let prefix  = "a" in
  if arity = 0 then 
    let ty = Typ.var ?loc ( prefix ^ "0") in 
    (Typ.arrow "" ?loc
       obj
       ty ,
     (obj, ty))
  else
    let tys = Ext_list.init (arity + 1) (fun i -> 
        Typ.var ?loc (prefix ^ string_of_int i)
      )  in
    (Typ.arrow "" ?loc obj 
       (Ext_list.reduce_from_right (fun x y -> Typ.arrow "" ?loc x y) tys),
     (obj, Typ.tuple ?loc  tys))
    


