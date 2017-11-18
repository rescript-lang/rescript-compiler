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

let invalid_config (config : Parsetree.expression) = 
  Location.raise_errorf ~loc:config.pexp_loc "such configuration is not supported"

let init () =      
  Ast_derive.register
    "jsMapper"
    (fun ( x : Parsetree.expression option) -> 
       (match x with 
        | Some config -> invalid_config config 
        | None -> ());
       {
         structure_gen = (fun (tdcls : tdcls) _ -> 
             let handle_tdcl tdcl =
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
               let toJsBody body = 
                 Ast_comb.single_non_rec_value patToJs
                   (Exp.fun_ "" None (Pat.constraint_ (Pat.var pat_param) core_type) 
                      body )
               in 
               match tdcl.ptype_kind with  
               | Ptype_record label_declarations -> 
                 let exp = 
                   Exp.record
                     (List.map 
                        (fun ({pld_name = {loc; txt } } : Parsetree.label_declaration) -> 
                           let label = 
                             {Asttypes.loc; txt = Longident.Lident txt } in 
                           label,Exp.field exp_param label
                        ) label_declarations) None in 
                 let toJs = 
                   toJsBody
                     (Exp.extension ({Asttypes.loc; txt = "bs.obj"}, (PStr [Str.eval exp  ])))
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
                        (Exp.constraint_ obj_exp core_type) )
                 in
                 [
                   toJs;
                   fromJs
                 ]
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
                        [
                          Ast_comb.single_non_rec_value 
                            {loc; txt = constantArray}
                            (Exp.array
                               (List.map (fun (i,str) -> 
                                    Exp.tuple 
                                      [
                                        Exp.constant (Const_int i);
                                        Exp.constant (Const_string (str, None))
                                      ]
                                  ) result));
                          (
                            toJsBody
                              (Exp.apply
                                 (Exp.ident ({loc; 
                                              txt = Longident.parse "Js.MapperRt.search" })
                                 )
                                 [
                                   "", exp_param;
                                   "", expConstantArray
                                 ]
                              )
                          );
                          Ast_comb.single_non_rec_value
                            patFromJs
                            (Exp.fun_ "" None 
                               (Pat.var pat_param)
                               (Exp.constraint_
                                  (
                                    Exp.apply
                                      (Exp.ident {loc; txt = Longident.parse "Js.MapperRt.revSearch"})
                                      [
                                        "", Exp.constant (Const_int (List.length result));
                                        "", expConstantArray;
                                        "", exp_param
                                      ]
                                  )
                                  (Ast_core_type.lift_option_type core_type)
                               )

                            )
                        ]
                      | _ -> assert false 
                    end 
                  | None -> []
                 )

               | Ptype_variant ctors -> 
                 if Ast_polyvar.is_enum_constructors ctors then 
                   let xs = Ast_polyvar.map_constructor_declarations_into_ints ctors in 
                   match xs with 
                   | `New xs ->
                     [
                       Ast_comb.single_non_rec_value 
                         {loc; txt = constantArray}
                         (Exp.array (List.map (fun i -> Exp.constant (Const_int i)) xs ))
                       ;
                       (toJsBody                        
                          (
                            Exp.apply
                              (Exp.ident {loc; txt = Longident.parse "Js.MapperRt.toInt"})
                              [
                                "", exp_param;
                                "", Exp.ident {loc; txt = Lident constantArray}
                              ]
                          )
                       )
                       ;

                       Ast_comb.single_non_rec_value
                         patFromJs
                         (Exp.fun_ "" None 
                            (Pat.constraint_ 
                               (Pat.var pat_param)
                               (Ast_literal.type_int ())
                            )
                            (Exp.constraint_
                               (Exp.apply
                                  (Exp.ident {loc; txt = Longident.parse "Js.MapperRt.fromInt"})
                                  [
                                    "", Exp.constant(Const_int (List.length ctors));
                                    "", Exp.ident {loc; txt = Lident constantArray};
                                    "", exp_param
                                  ]
                               )
                               (Ast_core_type.lift_option_type core_type)
                            )
                         )
                     ]
                   | `Offset offset  ->                      

                     [(toJsBody
                         (Exp.apply 
                            (Exp.ident {loc; txt = Ldot (Lident "Pervasives", "+")})
                            [
                              "",
                              (Exp.apply 
                                 (Exp.ident {loc; txt = Ldot (Lident "Obj", "magic")})
                                 ["",
                                  exp_param]);
                              "", Exp.constant (Const_int offset)
                            ]
                         )                         
                      );

                      Ast_comb.single_non_rec_value
                        {loc ; txt = fromJs}
                        (Exp.fun_ "" None 
                           (Pat.constraint_ 
                              (Pat.var pat_param)
                              (Ast_literal.type_int ())
                           )
                           (Exp.constraint_
                              (

                                let len = List.length ctors in 
                                let range_low = Exp.constant (Const_int (offset + 0)) in 
                                let range_upper = Exp.constant (Const_int (offset + len - 1)) in 
                                Exp.apply 
                                  (Exp.ident {loc; txt = Ldot (Lident "Obj", "magic")})  
                                  ["",
                                   (
                                     Exp.ifthenelse
                                       (Exp.apply
                                          (Exp.ident {loc ; txt = Lident "&&"})
                                          ["",
                                           (Exp.apply (Exp.ident {loc; txt = Lident "<="})
                                              ["", exp_param; "", range_upper] )
                                           ;
                                           "",
                                           (Exp.apply (Exp.ident {loc; txt = Lident "<="})
                                              ["", range_low; "",exp_param]
                                           )
                                          ]
                                       )
                                       (Exp.construct {loc; txt = Lident "Some"} 
                                          (Some 
                                             (Exp.apply
                                                (Exp.ident {loc; txt = Ldot(Lident "Pervasives","-")})
                                                ["",exp_param ; "", Exp.constant (Const_int offset)])
                                          ))
                                       (Some (Exp.construct {loc; txt = Lident "None"} None)))
                                  ]
                              )
                              (Ast_core_type.lift_option_type core_type)
                           )
                        )
                     ]
                 else []  
               | Ptype_open -> [] in 
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
                match tdcl.ptype_kind with  
                | Ptype_record label_declarations ->            

                  let ty1 = 
                    Ast_comb.to_js_type loc @@  
                    Typ.object_
                      (List.map 
                         (fun ({pld_name = {loc; txt }; pld_type } : Parsetree.label_declaration) -> 
                            txt, [], pld_type
                         ) label_declarations) 
                      Open in 
                  let ty2 = 
                    Ast_comb.to_js_type loc @@  
                    Typ.object_
                      (List.map 
                         (fun ({pld_name = {loc; txt }; pld_type } : Parsetree.label_declaration) -> 
                            txt, [], pld_type
                         ) label_declarations) 
                      Closed in                       
                  let fromJs =    
                    Ast_comb.single_non_rec_val patFromJs (Typ.arrow ""  ty1 core_type) in 
                  [
                    toJsType ty2;
                    fromJs
                  ]
                | Ptype_abstract ->   
                  (match Ast_polyvar.is_enum_polyvar tdcl with 
                   | Some _ ->                     
                     let ty1 = (Ast_literal.type_string ()) in 
                     let ty2 = Ast_core_type.lift_option_type core_type in 
                     [
                       toJsType ty1;
                       Ast_comb.single_non_rec_val     
                         {loc; txt = fromJs}
                         (Typ.arrow ""
                            ty1 ty2
                         )
                     ]
                   | None -> [])

                | Ptype_variant ctors 
                  -> 

                  if Ast_polyvar.is_enum_constructors ctors then 
                    let ty1 = Ast_literal.type_int() in 
                    let ty2 = Ast_core_type.lift_option_type core_type in 
                    [
                      toJsType ty1;
                      Ast_comb.single_non_rec_val
                        patFromJs
                        (Typ.arrow "" ty1 ty2)
                    ]
                  else []
                | Ptype_open -> [] in 
              Ext_list.flat_map handle_tdcl tdcls 

           );
         expression_gen = None 
       } 
    )
;