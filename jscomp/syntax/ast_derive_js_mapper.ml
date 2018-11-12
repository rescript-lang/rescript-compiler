(* Copyright (C) 2017 Authors of BuckleScript
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
module U = Ast_derive_util
type tdcls = Parsetree.type_declaration list 

let js_field (o : Parsetree.expression) m = 
  Ast_compatible.app2
    (Exp.ident {txt = Lident "##"; loc = o.pexp_loc})
    o
    (Exp.ident m)


 

let handle_config (config : Parsetree.expression option) = 
  match config with 
  | Some config -> 
    (match config.pexp_desc with 
     | Pexp_record (
         [ 
           {txt = Lident "newType"}, 
           {pexp_desc = 
              (Pexp_construct 
                 (
                   {txt =
                      Lident ("true" 
                             | "false" 
                               as x)}, None)
              | Pexp_ident {txt = Lident ("newType" as x)}
              )
           }
         ],None)
       ->  not (x = "false")
     | Pexp_ident {txt = Lident ("newType")} 
       -> true
     | _ -> U.invalid_config config)
  | None -> false
let noloc = Location.none
(* [eraseType] will be instrumented, be careful about the name conflict*)  
let eraseTypeLit = "jsMapperEraseType"
let eraseTypeExp = Exp.ident {loc = noloc; txt = Lident eraseTypeLit}
let eraseType x = 
  Ast_compatible.app1 eraseTypeExp  x
let eraseTypeStr = 
  let any = Typ.any () in 
  Str.primitive 
    (Val.mk ~prim:["%identity"] {loc = noloc; txt = eraseTypeLit}
       (Ast_compatible.arrow any any)
    )

let app2 = Ast_compatible.app2
let app3 = Ast_compatible.app3

let (<=~) a b =   
  app2 (Exp.ident {loc = noloc; txt = Lident "<="}) a b 
let (-~) a b =   
  app2 (Exp.ident {loc = noloc; txt = Ldot(Lident "Pervasives","-")})
    a b 
let (+~) a b =   
  app2 (Exp.ident {loc = noloc; txt = Ldot(Lident "Pervasives","+")})
    a b 
let (&&~) a b =   
  app2 (Exp.ident {loc = noloc; txt = Ldot(Lident "Pervasives","&&")})
    a b 
let (->~) a b = Ast_compatible.arrow a b 
let jsMapperRt =     
  Longident.Ldot (Lident "Js", "MapperRt")

let search upper polyvar array = 
  app3
    (Exp.ident ({loc = noloc; 
                 txt = Longident.Ldot (jsMapperRt,"binarySearch") })
    )                                 
    upper
    (eraseType polyvar)
    array

let revSearch len constantArray exp =   
  app3 
    (Exp.ident 
       {loc= noloc; 
        txt = Longident.Ldot (jsMapperRt, "revSearch")})
    len
    constantArray
    exp

let revSearchAssert  len constantArray exp =   
  app3 
    (Exp.ident 
       {loc= noloc; 
        txt = Longident.Ldot (jsMapperRt, "revSearchAssert")})
    len
    constantArray
    exp

let toInt exp array =     
  app2
    (Exp.ident 
       { loc=noloc; 
         txt = Longident.Ldot (jsMapperRt, "toInt")})
    (eraseType exp)
    array
let fromInt len array exp = 
  app3
    (Exp.ident 
       {loc = noloc; 
        txt = Longident.Ldot (jsMapperRt,"fromInt")})
    len
    array
    exp

let fromIntAssert len array exp = 
  app3
    (Exp.ident 
       {loc = noloc; 
        txt = Longident.Ldot (jsMapperRt,"fromIntAssert")})
    len
    array
    exp


let assertExp e = 
  Exp.extension 
    ({Asttypes.loc = noloc; txt = "assert"},
     (PStr 
        [Str.eval e ]
     )
    )
let derivingName = "jsConverter"

(* let notApplicable loc = 
  Location.prerr_warning 
    loc
    (Warnings.Bs_derive_warning ( derivingName ^ " not applicable to this type")) *)

let init () =      
  Ast_derive.register
    derivingName
    (fun ( x : Parsetree.expression option) -> 
       let createType = handle_config x in 

       {
         structure_gen = (fun (tdcls : tdcls) _ -> 
             let handle_tdcl (tdcl: Parsetree.type_declaration) =
               let core_type = U.core_type_of_type_declaration tdcl
               in 
               let name = tdcl.ptype_name.txt in 
               let toJs = name ^ "ToJs" in 
               let fromJs = name ^ "FromJs" in 
               let constantArray = "jsMapperConstantArray" in 
               let loc = tdcl.ptype_loc in 
               let patToJs = {Asttypes.loc; txt = toJs} in 
               let patFromJs = {Asttypes.loc; txt = fromJs} in 
               let param = "param" in 

               let ident_param = {Asttypes.txt = Longident.Lident param; loc} in 
               let pat_param = {Asttypes.loc; txt = param} in 
               let exp_param = Exp.ident ident_param in 
               let newType,newTdcl =
                 U.new_type_of_type_declaration tdcl ("abs_" ^ name) in 
               let newTypeStr = Ast_compatible.rec_type_str [newTdcl] in   
               let toJsBody body = 
                 Ast_comb.single_non_rec_value patToJs
                   (Ast_compatible.fun_ (Pat.constraint_ (Pat.var pat_param) core_type) 
                      body )
               in 
               let (+>) a ty = 
                 Exp.constraint_ (eraseType a) ty in 
               let (+:) a ty =                  
                 eraseType (Exp.constraint_ a ty) in 
               let coerceResultToNewType e =
                 if createType then 
                   e +> newType
                 else e    
               in                  
               match tdcl.ptype_kind with  
               | Ptype_record label_declarations -> 
                 let exp = 
                   coerceResultToNewType
                     (Exp.extension 
                        (
                          {Asttypes.loc; txt = "bs.obj"},
                          (PStr
                             [Str.eval  
                                (Exp.record
                                   (Ext_list.map label_declarations
                                      (fun {pld_name = {loc; txt } }  -> 
                                         let label = 
                                           {Asttypes.loc; txt = Longident.Lident txt } in 
                                         label,Exp.field exp_param label) ) None)]))) in 
                 let toJs = 
                   toJsBody exp
                 in 
                 let obj_exp = 
                   Exp.record
                     (Ext_list.map label_declarations
                        (fun {pld_name = {loc; txt } } -> 
                           let label = 
                             {Asttypes.loc; txt = Longident.Lident txt } in 
                           label,
                           js_field exp_param  label) ) None in 
                 let fromJs = 
                   Ast_comb.single_non_rec_value patFromJs
                     (Ast_compatible.fun_ (Pat.var pat_param)
                        (if createType then                                             
                           (Exp.let_ Nonrecursive
                              [Vb.mk 
                                 (Pat.var pat_param) 
                                 (exp_param +: newType)]
                              (Exp.constraint_ obj_exp core_type) )
                         else 
                           (Exp.constraint_ obj_exp core_type) ))
                 in
                 let rest = 
                   [
                     toJs;
                     fromJs
                   ] in 
                 if createType then eraseTypeStr:: newTypeStr :: rest else rest 
               | Ptype_abstract -> 
                 (match Ast_polyvar.is_enum_polyvar tdcl with 
                  | Some row_fields ->               
                    let expConstantArray =   
                      Exp.ident {loc; txt = Longident.Lident constantArray} in 
                    let result :  _ list = 
                      Ext_list.map row_fields (fun tag -> 
                          match tag with 
                          | Rtag (label, attrs, _, []) -> 
                            (Ast_compatible.hash_label label,
                             match Ast_attributes.iter_process_bs_string_as_ast attrs with 
                             | Some name -> 
                               name
                             | None -> 
                                Ast_compatible.const_exp_string(Ast_compatible.label_of_name label)
                            )
                          | _ -> assert false (* checked by [is_enum_polyvar] *)
                        ) in 
                    let result_len = List.length result in 
                    let exp_len = Ast_compatible.const_exp_int result_len in 
                    let v = [
                      eraseTypeStr;
                      Ast_comb.single_non_rec_value 
                        {loc; txt = constantArray}
                        (Exp.array
                           (Ext_list.map (List.sort (fun (a,_) (b,_) -> compare (a:int) b) result)
                              (fun (i,str) -> 
                                 Exp.tuple 
                                   [
                                     Ast_compatible.const_exp_int i;
                                      str
                                   ]
                              ) ));
                      (
                        toJsBody
                          (coerceResultToNewType 
                             (search
                                exp_len
                                exp_param
                                expConstantArray 
                             ))
                      );
                      Ast_comb.single_non_rec_value
                        patFromJs
                        (Ast_compatible.fun_
                           (Pat.var pat_param)
                           (if createType then 
                              revSearchAssert
                                exp_len
                                expConstantArray
                                (exp_param +: newType)
                              +>
                              core_type
                            else 
                              revSearch                                      
                                exp_len
                                expConstantArray
                                exp_param                                      
                              +>
                              Ast_core_type.lift_option_type core_type
                           )
                        )
                    ] in 
                    if createType then 
                      newTypeStr :: v 
                    else v 
                  | None -> 
                    U.notApplicable 
                      tdcl.Parsetree.ptype_loc 
                      derivingName;
                    []
                 )

               | Ptype_variant ctors -> 
                 if Ast_polyvar.is_enum_constructors ctors then 
                   let xs = Ast_polyvar.map_constructor_declarations_into_ints ctors in 
                   match xs with 
                   | `New xs ->
                     let constantArrayExp = Exp.ident {loc; txt = Lident constantArray} in
                     let exp_len = Ast_compatible.const_exp_int (List.length ctors) in
                     let v = [
                       eraseTypeStr;
                       Ast_comb.single_non_rec_value 
                         {loc; txt = constantArray}
                         (Ast_compatible.const_exp_int_list_as_array xs)
                       ;
                       toJsBody                        
                         (
                           coerceResultToNewType @@
                           toInt
                             exp_param
                             constantArrayExp
                         )                       
                       ;
                       Ast_comb.single_non_rec_value
                         patFromJs
                         (Ast_compatible.fun_
                            (Pat.var pat_param)
                            (
                              if createType then 
                                fromIntAssert
                                  exp_len
                                  constantArrayExp
                                  (exp_param +: newType)
                                +>
                                core_type
                              else 
                                fromInt                                 
                                  exp_len
                                  constantArrayExp
                                  exp_param
                                +>
                                Ast_core_type.lift_option_type core_type

                            )
                         )
                     ] in 
                     if createType then newTypeStr :: v else v 
                   | `Offset offset  ->                      
                     let v = 
                       [  eraseTypeStr;
                          toJsBody (
                            coerceResultToNewType
                              (eraseType exp_param +~ Ast_compatible.const_exp_int offset)
                          )
                          ;
                          let len = List.length ctors in 
                          let range_low = Ast_compatible.const_exp_int (offset + 0) in 
                          let range_upper = Ast_compatible.const_exp_int (offset + len - 1) in 

                          Ast_comb.single_non_rec_value
                            {loc ; txt = fromJs}
                            (Ast_compatible.fun_
                               (Pat.var pat_param)
                               (if createType then 
                                  (Exp.let_ Nonrecursive
                                     [Vb.mk
                                        (Pat.var pat_param)
                                        (exp_param +: newType)
                                     ]
                                     (
                                       Exp.sequence
                                         (assertExp 
                                            ((exp_param <=~ range_upper) &&~ (range_low <=~ exp_param))
                                         )
                                         (exp_param  -~ Ast_compatible.const_exp_int offset))
                                  )
                                  +>
                                  core_type
                                else
                                  (Exp.ifthenelse
                                     ( (exp_param <=~ range_upper) &&~ (range_low <=~ exp_param))
                                     (Exp.construct {loc; txt = Lident "Some"} 
                                        ( Some (exp_param -~ Ast_compatible.const_exp_int offset)))
                                     (Some (Exp.construct {loc; txt = Lident "None"} None)))
                                  +>
                                  Ast_core_type.lift_option_type core_type
                               )
                            )
                       ] in 
                     if createType then newTypeStr :: v else v 
                 else 
                   begin 
                     U.notApplicable 
                     tdcl.Parsetree.ptype_loc 
                     derivingName;
                     []  
                   end
               | Ptype_open -> 
                 U.notApplicable tdcl.Parsetree.ptype_loc 
                 derivingName;
                 [] in 
             Ext_list.flat_map tdcls handle_tdcl
           );
         signature_gen = 
           (fun (tdcls : tdcls) _ -> 
              let handle_tdcl tdcl =
                let core_type = U.core_type_of_type_declaration tdcl 
                in 
                let name = tdcl.ptype_name.txt in 
                let toJs = name ^ "ToJs" in 
                let fromJs = name ^ "FromJs" in 
                let loc = tdcl.ptype_loc in 
                let patToJs = {Asttypes.loc; txt = toJs} in 
                let patFromJs = {Asttypes.loc; txt = fromJs} in 
                let toJsType result = 
                  Ast_comb.single_non_rec_val patToJs (Ast_compatible.arrow core_type result) in
                let newType,newTdcl =
                  U.new_type_of_type_declaration tdcl ("abs_" ^ name) in 
                let newTypeStr = Ast_compatible.rec_type_sig [newTdcl] in                     
                let (+?) v rest = if createType then v :: rest else rest in 
                match tdcl.ptype_kind with  
                | Ptype_record label_declarations ->            

                  let objType flag =                     
                    Ast_comb.to_js_type loc @@  
                    Ast_compatible.object_
                      (Ext_list.map label_declarations
                         (fun {pld_name = {loc; txt }; pld_type } -> 
                            txt, [], pld_type)) 
                      flag in                   
                  newTypeStr +? 
                  [
                    toJsType (if createType then newType else  objType Closed);
                    Ast_comb.single_non_rec_val patFromJs 
                      ( (if createType then  newType else objType Open)->~ core_type)
                  ] 

                | Ptype_abstract ->   
                  (match Ast_polyvar.is_enum_polyvar tdcl with 
                   | Some _ ->                     
                     let ty1 =  
                       if createType then newType else 
                         (Ast_literal.type_string ()) in 
                     let ty2 = 
                       if createType then core_type
                       else Ast_core_type.lift_option_type core_type in 
                     newTypeStr +? 
                     [
                       toJsType ty1;
                       Ast_comb.single_non_rec_val     
                         patFromJs
                         (ty1 ->~ ty2)
                     ] 

                   | None -> 
                     U.notApplicable tdcl.Parsetree.ptype_loc 
                     derivingName;
                     [])

                | Ptype_variant ctors 
                  -> 

                  if Ast_polyvar.is_enum_constructors ctors then 
                    let ty1 = 
                      if createType then newType 
                      else Ast_literal.type_int() in 
                    let ty2 = 
                      if createType then core_type
                      else Ast_core_type.lift_option_type core_type (*-FIXME**) in 
                    newTypeStr +? 
                    [
                      toJsType ty1;
                      Ast_comb.single_non_rec_val
                        patFromJs
                        (ty1 ->~ ty2)
                    ] 

                  else 
                  begin
                    U.notApplicable tdcl.Parsetree.ptype_loc 
                    derivingName;
                    []
                  end
                | Ptype_open -> 
                  U.notApplicable tdcl.Parsetree.ptype_loc 
                  derivingName;
                  [] in 
              Ext_list.flat_map tdcls handle_tdcl

           );
         expression_gen = None 
       } 
    )
;
