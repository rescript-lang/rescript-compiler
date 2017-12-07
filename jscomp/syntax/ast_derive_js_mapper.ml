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

type tdcls = Parsetree.type_declaration list 

let js_field (o : Parsetree.expression) m = 
  Exp.apply 
    (Exp.ident {txt = Lident "##"; loc = o.pexp_loc})
    [ 
      "",o; 
      "", Exp.ident m
    ]
let const_int i = Exp.constant (Const_int i)
let const_string s = Exp.constant (Const_string (s,None))

let invalid_config (config : Parsetree.expression) = 
  Location.raise_errorf ~loc:config.pexp_loc "such configuration is not supported"

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
     | _ -> invalid_config config)
  | None -> false
let noloc = Location.none
(* [eraseType] will be instrumented, be careful about the name conflict*)  
let eraseTypeLit = "jsMapperEraseType"
let eraseTypeExp = Exp.ident {loc = noloc; txt = Lident eraseTypeLit}
let eraseType x = 
  Exp.apply eraseTypeExp ["", x]
let eraseTypeStr = 
  let any = Typ.any () in 
  Str.primitive 
    (Val.mk ~prim:["%identity"] {loc = noloc; txt = eraseTypeLit}
       (Typ.arrow "" any any)
    )

let app2 f arg1 arg2 = 
  Exp.apply f ["",arg1; "", arg2]
let app3 f arg1 arg2 arg3 = 
  Exp.apply f ["", arg1; "", arg2; "", arg3]
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
let (->~) a b = Typ.arrow "" a b 
let jsMapperRt =     
  Longident.Ldot (Lident "Js", "MapperRt")

let search upper polyvar array = 
  app3
    (Exp.ident ({loc = noloc; 
                 txt = Longident.Ldot (jsMapperRt,"binSearch") })
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

let notApplicable loc = 
  Location.prerr_warning 
    loc
    (Warnings.Bs_derive_warning ( derivingName ^ " not applicable to this type"))

let init () =      
  Ast_derive.register
    derivingName
    (fun ( x : Parsetree.expression option) -> 
       let createType = handle_config x in 

       {
         structure_gen = (fun (tdcls : tdcls) _ -> 
             let handle_tdcl (tdcl: Parsetree.type_declaration) =
               let core_type = Ast_derive_util.core_type_of_type_declaration tdcl
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
                 Ast_derive_util.new_type_of_type_declaration tdcl ("abs_" ^ name) in 
               let newTypeStr = Str.type_ [newTdcl] in   
               let toJsBody body = 
                 Ast_comb.single_non_rec_value patToJs
                   (Exp.fun_ "" None (Pat.constraint_ (Pat.var pat_param) core_type) 
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
                                   (List.map 
                                      (fun ({pld_name = {loc; txt } } : Parsetree.label_declaration) -> 
                                         let label = 
                                           {Asttypes.loc; txt = Longident.Lident txt } in 
                                         label,Exp.field exp_param label
                                      ) label_declarations) None)]))) in 
                 let toJs = 
                   toJsBody exp
                 in 
                 let obj_exp = 
                   Exp.record
                     (List.map 
                        (fun ({pld_name = {loc; txt } } : Parsetree.label_declaration) -> 
                           let label = 
                             {Asttypes.loc; txt = Longident.Lident txt } in 
                           label,
                           js_field exp_param  label
                        ) label_declarations) None in 
                 let fromJs = 
                   Ast_comb.single_non_rec_value patFromJs
                     (Exp.fun_ "" None (Pat.var pat_param)
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
                    let attr = 
                      Ast_polyvar.map_row_fields_into_strings loc row_fields 
                    in 
                    let expConstantArray =   
                      Exp.ident {loc; txt = Longident.Lident constantArray} in 
                    begin match attr with 
                      | NullString result -> 
                        let result_len = List.length result in 
                        let exp_len = const_int result_len in 
                        let v = [
                          eraseTypeStr;
                          Ast_comb.single_non_rec_value 
                            {loc; txt = constantArray}
                            (Exp.array
                               (List.map (fun (i,str) -> 
                                    Exp.tuple 
                                      [
                                        const_int i;
                                        const_string str
                                      ]
                                  ) (List.sort (fun (a,_) (b,_) -> compare (a:int) b) result)));
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
                            (Exp.fun_ "" None 
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
                      | _ -> assert false 
                    end 
                  | None -> 
                    notApplicable tdcl.Parsetree.ptype_loc ;
                    []
                 )

               | Ptype_variant ctors -> 
                 if Ast_polyvar.is_enum_constructors ctors then 
                   let xs = Ast_polyvar.map_constructor_declarations_into_ints ctors in 
                   match xs with 
                   | `New xs ->
                     let constantArrayExp = Exp.ident {loc; txt = Lident constantArray} in
                     let exp_len = const_int (List.length ctors) in
                     let v = [
                       eraseTypeStr;
                       Ast_comb.single_non_rec_value 
                         {loc; txt = constantArray}
                         (Exp.array (List.map (fun i -> const_int i) xs ))
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
                         (Exp.fun_ "" None 
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
                              (eraseType exp_param +~ const_int offset)
                          )
                          ;
                          let len = List.length ctors in 
                          let range_low = const_int (offset + 0) in 
                          let range_upper = const_int (offset + len - 1) in 

                          Ast_comb.single_non_rec_value
                            {loc ; txt = fromJs}
                            (Exp.fun_ "" None 
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
                                         (exp_param  -~ const_int offset))
                                  )
                                  +>
                                  core_type
                                else
                                  (Exp.ifthenelse
                                     ( (exp_param <=~ range_upper) &&~ (range_low <=~ exp_param))
                                     (Exp.construct {loc; txt = Lident "Some"} 
                                        ( Some (exp_param -~ const_int offset)))
                                     (Some (Exp.construct {loc; txt = Lident "None"} None)))
                                  +>
                                  Ast_core_type.lift_option_type core_type
                               )
                            )
                       ] in 
                     if createType then newTypeStr :: v else v 
                 else 
                   begin 
                     notApplicable tdcl.Parsetree.ptype_loc ;
                     []  
                   end
               | Ptype_open -> 
                 notApplicable tdcl.Parsetree.ptype_loc ;
                 [] in 
             Ext_list.flat_map handle_tdcl tdcls 
           );
         signature_gen = 
           (fun (tdcls : tdcls) _ -> 
              let handle_tdcl tdcl =
                let core_type = Ast_derive_util.core_type_of_type_declaration tdcl 
                in 
                let name = tdcl.ptype_name.txt in 
                let toJs = name ^ "ToJs" in 
                let fromJs = name ^ "FromJs" in 
                let loc = tdcl.ptype_loc in 
                let patToJs = {Asttypes.loc; txt = toJs} in 
                let patFromJs = {Asttypes.loc; txt = fromJs} in 
                let toJsType result = 
                  Ast_comb.single_non_rec_val patToJs (Typ.arrow "" core_type result) in
                let newType,newTdcl =
                  Ast_derive_util.new_type_of_type_declaration tdcl ("abs_" ^ name) in 
                let newTypeStr = Sig.type_ [newTdcl] in                     
                let (+?) v rest = if createType then v :: rest else rest in 
                match tdcl.ptype_kind with  
                | Ptype_record label_declarations ->            

                  let objType flag =                     
                    Ast_comb.to_js_type loc @@  
                    Typ.object_
                      (List.map 
                         (fun ({pld_name = {loc; txt }; pld_type } : Parsetree.label_declaration) -> 
                            txt, [], pld_type
                         ) label_declarations) 
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
                     notApplicable tdcl.Parsetree.ptype_loc ;
                     [])

                | Ptype_variant ctors 
                  -> 

                  if Ast_polyvar.is_enum_constructors ctors then 
                    let ty1 = 
                      if createType then newType 
                      else Ast_literal.type_int() in 
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

                  else 
                  begin
                    notApplicable tdcl.Parsetree.ptype_loc ;
                    []
                  end
                | Ptype_open -> 
                  notApplicable tdcl.Parsetree.ptype_loc ;
                  [] in 
              Ext_list.flat_map handle_tdcl tdcls 

           );
         expression_gen = None 
       } 
    )
;