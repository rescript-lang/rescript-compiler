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
               match tdcl.ptype_kind with  
               | Ptype_record label_declarations -> 
                 let record_arg = "record" in        
                 let exp = 
                   Exp.record
                     (List.map 
                        (fun ({pld_name = {loc; txt } } : Parsetree.label_declaration) -> 
                           {Asttypes.loc; txt = Longident.Lident txt },
                           Exp.field (Exp.ident {txt = Lident record_arg ; loc })
                             {Asttypes.loc; txt = Longident.Lident txt }
                        ) label_declarations) None in 
                 let loc = tdcl.ptype_loc in        
                 let toJs = Ast_comb.single_non_rec_value {loc; txt = toJs}
                     (Exp.fun_ "" None (Pat.constraint_ (Pat.var {loc; txt = record_arg}) core_type) 
                        (Exp.extension ({Asttypes.loc; txt = "bs.obj"}, (PStr [Str.eval exp  ])))) 
                 in 
                 let obj_arg = "obj" in 
                 let obj_exp = 
                   Exp.record
                     (List.map 
                        (fun ({pld_name = {loc; txt } } : Parsetree.label_declaration) -> 
                           {Asttypes.loc; txt = Longident.Lident txt },
                           js_field (Exp.ident {txt = Lident obj_arg ; loc })
                             {Asttypes.loc; txt = Longident.Lident txt }
                        ) label_declarations) None in 
                 let fromJs = 
                   Ast_comb.single_non_rec_value {loc; txt = fromJs}
                     (Exp.fun_ "" None (Pat.var {loc; txt = obj_arg})
                        (Exp.constraint_ obj_exp core_type) )
                 in
                 [
                   toJs;
                   fromJs
                 ]
               | Ptype_abstract -> 

                  begin match tdcl.ptype_manifest with 
                   | Some {
                       ptyp_desc = 
                         Ptyp_variant(row_fields, Closed,None);
                       ptyp_loc
                     }
                     -> 
                     if Ast_polyvar.is_enum row_fields then 

                       let attr = 
                         Ast_polyvar.map_row_fields_into_strings ptyp_loc row_fields 
                       in (* how to mark attributes as used *)
                       let constantArray = "constantArray" in 
                       begin match attr with 
                         | NullString result -> 
                           [
                             Ast_comb.single_non_rec_value 
                               {loc = ptyp_loc; txt = constantArray}
                               (Exp.array
                                  (List.map (fun (i,str) -> 
                                       Exp.tuple 
                                         [
                                           Exp.constant (Const_int i);
                                           Exp.constant (Const_string (str, None))
                                         ]
                                     ) result));
                             let polyvar_arg = "polyvar"   in 
                             Ast_comb.single_non_rec_value        
                              {loc = ptyp_loc; txt = toJs}
                              (Exp.fun_ "" None 
                                (Pat.constraint_ 
                                  (Pat.var {loc = ptyp_loc; txt = polyvar_arg })
                                  core_type
                                )
                                (Exp.apply
                                  (Exp.ident ({loc = ptyp_loc; 
                                    txt = Longident.parse "Js.MapperRt.search" })
                                  )
                                [
                                  "", (Exp.ident {loc = ptyp_loc; txt = Longident.Lident polyvar_arg});
                                  "", Exp.ident {loc = ptyp_loc; txt = Longident.Lident constantArray}
                                ]
                              )
                              )

                           ]
                         | _ -> assert false 
                       end 
                     else []
                   | Some _ | None -> []
                 end 
               | Ptype_variant _
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
                match tdcl.ptype_kind with  
                | Ptype_record label_declarations ->            
                  let loc = tdcl.ptype_loc in 
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
                  let loc = tdcl.ptype_loc in        
                  let toJs = 
                    Ast_comb.single_non_rec_val {loc; txt = toJs}
                      (Typ.arrow "" core_type ty2) in 
                  let fromJs =    
                    Ast_comb.single_non_rec_val {loc; txt = fromJs}
                      (Typ.arrow ""  ty1 core_type) in 
                  [
                    toJs;
                    fromJs
                  ]
                | Ptype_abstract ->   
                  begin match tdcl.ptype_manifest with 
                  | Some {
                    ptyp_desc = Ptyp_variant(row_fields,Closed,None);
                    ptyp_loc = loc
                  } -> 
                    if Ast_polyvar.is_enum row_fields then 
                      [Ast_comb.single_non_rec_val
                      {loc; txt = toJs}
                      (Typ.arrow "" 
                        core_type
                        (Ast_literal.type_string ()))
                      ]
                    else []  
                  | Some _ | None -> []
                  end
                | Ptype_variant _
                | Ptype_open -> [] in 
              Ext_list.flat_map handle_tdcl tdcls 

           );
         expression_gen = None 
       } 
    )
;