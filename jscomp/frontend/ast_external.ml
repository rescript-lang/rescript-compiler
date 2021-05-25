(* Copyright (C) 2018 Hongbo Zhang, Authors of ReScript
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


let handleExternalInSig
    (self : Bs_ast_mapper.mapper)
    (prim : Parsetree.value_description)
    (sigi : Parsetree.signature_item)
  : Parsetree.signature_item
  =
  let loc = prim.pval_loc in  
  let pval_type = self.typ self prim.pval_type in
  let pval_attributes = self.attributes self prim.pval_attributes in
  match Ast_attributes.process_send_pipe pval_attributes with 
  | Some (obj , _) -> 
    (*has bs.send.pipe: best effort *)
    begin {
      sigi with 
      psig_desc = Psig_value {
          prim with 
          pval_type = Ast_helper.Typ.arrow ~loc Nolabel obj pval_type ; 
          pval_prim = []; 
          pval_attributes = []
        }
    } 
    end   
  | None  -> 
    match prim.pval_prim with
    | [] -> 
      Location.raise_errorf
        ~loc
        "empty primitive string" 
    | a :: b :: _ ->
      Location.raise_errorf
        ~loc
        "only a single string is allowed in bs external %S %S" a b
    | [ v ] ->
      match Ast_external_process.encode_attributes_as_string
              loc
              pval_type
              pval_attributes
              v 
      with
      | {pval_type; pval_prim; pval_attributes; no_inline_cross_module} ->        
        {sigi with
         psig_desc =
           Psig_value
             {prim with
              pval_type ;
              pval_prim = if no_inline_cross_module then [] else pval_prim ;
              pval_attributes
             }}

let handleExternalInStru
    (self : Bs_ast_mapper.mapper)
    (prim : Parsetree.value_description)
    (str : Parsetree.structure_item)
  : Parsetree.structure_item =
  let loc = prim.pval_loc in 
  let pval_type = self.typ self prim.pval_type in
  let pval_attributes = self.attributes self prim.pval_attributes in
  let send_pipe = ref false in 
  let pval_type , pval_attributes = 
    match Ast_attributes.process_send_pipe pval_attributes with 
    | Some (obj, attrs) -> 
      send_pipe := true ; 
      Ast_helper.Typ.arrow ~loc Nolabel obj pval_type, attrs
    | None  -> pval_type, pval_attributes in 
  match prim.pval_prim with
  | [] 
    -> 
    Location.raise_errorf
      ~loc 
      "empty primitive string" 
  | a :: b :: _
    -> 
    Location.raise_errorf
      ~loc 
      "only a single string is allowed in bs external %S : %S"  a b 
  | [ v] ->
    match Ast_external_process.encode_attributes_as_string
            loc
            pval_type 
            pval_attributes 
            v with 
    | { pval_type; pval_prim; pval_attributes; no_inline_cross_module} ->
      let external_result = 
        {str with
         pstr_desc =
           Pstr_primitive
             {prim with
              pval_type ;
              pval_prim;
              pval_attributes
             }} in 
      let normal () =
        if not no_inline_cross_module then   
          external_result
        else
          let open Ast_helper in 
          Str.include_ ~loc 
            (Incl.mk ~loc 
               (Mod.constraint_ ~loc
                  (Mod.structure ~loc 
                     [external_result])
                  (Mty.signature ~loc [
                      {
                        psig_desc = Psig_value {
                            prim with 
                            pval_type ; 
                            pval_prim = [];
                            pval_attributes ;
                          };
                        psig_loc = loc
                      }]))) in        
      if !send_pipe then        
        let [@warning "-8"] ((_::params) as args) = Ast_core_type.get_curry_labels pval_type in 
        let arity = List.length args in 
        if arity = 1 then normal () else 
          let open Ast_helper in 
          Str.include_ ~loc 
            (Incl.mk ~loc 
               (Mod.structure ~loc 
                  [external_result;
                   Str.value 
                     ~loc Nonrecursive [
                     Vb.mk ~loc 
                       (Pat.var ~loc prim.pval_name)
                       (let body = 
                          Exp.apply ~loc 
                            (Exp.ident ~loc {txt = Lident prim.pval_name.txt; loc }) 
                            (
                              (Asttypes.Nolabel, Exp.ident ~loc {txt = Lident "obj"; loc}) ::
                              Ext_list.mapi params (fun i x ->
                                  (x,                                  
                                   match x with 
                                   | Asttypes.Nolabel -> 
                                     Exp.ident {txt = Lident ("arg"^string_of_int (i + 1)); loc}
                                   | Labelled s
                                   | Optional s 
                                     -> 
                                     Exp.ident {txt = Lident s ; loc})))
                        in 
                        snd @@ 
                        Ext_list.fold_right
                          params (0, Exp.fun_  Nolabel None (Pat.var ~loc { txt = "obj"; loc} ) body) (
                          fun arg (i, obj) -> 
                            i + 1, Exp.fun_ arg None 
                              (Pat.var ~loc {txt = (match arg with 
                                   | Labelled s | Optional s -> s 
                                   | Nolabel -> "arg" ^ string_of_int (arity - i - 1)); loc}) obj
                        )
                       )
                   ]
                  ])
            )
      else 
        normal()

