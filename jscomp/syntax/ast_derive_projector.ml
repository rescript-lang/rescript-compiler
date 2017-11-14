open Ast_helper


let init () =
  Ast_derive.register
    "accessors" 
    begin fun (x : Parsetree.expression option) ->
       match x with 
       | Some {pexp_loc = loc} 
         -> Location.raise_errorf ~loc "such configuration is not supported"
       | None -> 
         {structure_gen = 
            begin fun (tdcls : Parsetree.type_declaration list) _explict_nonrec ->
              let handle_tdcl tdcl = 
                let core_type = Ast_derive_util.core_type_of_type_declaration tdcl in 
                match tdcl with 
                | {ptype_kind = 
                     Ptype_record label_declarations }
                  -> 
                  label_declarations 
                  |> 
                  Ext_list.map (fun ({pld_name = {loc; txt = pld_label} as pld_name} : Parsetree.label_declaration) -> 
                      let txt = "param" in
                      Str.value Nonrecursive

                        [Vb.mk (Pat.var pld_name) @@
                         Exp.fun_ "" None
                           (Pat.constraint_ (Pat.var {txt ; loc}) core_type )
                           (Exp.field (Exp.ident {txt = Lident txt ; loc}) 
                              {txt = Longident.Lident pld_label ; loc}) ]
                    )
                | {ptype_kind = 
                     Ptype_variant constructor_declarations 
                  } 
                  -> 
                  constructor_declarations
                  |> 
                  Ext_list.map 
                    (fun
                      ( {pcd_name = {loc ; txt = con_name} ; pcd_args ; pcd_loc }:
                          Parsetree.constructor_declaration)
                      -> (* TODO: add type annotations *)
                        let little_con_name = String.uncapitalize con_name  in
                        let arity = List.length pcd_args in 
                        if arity = 0 then 
                          Str.value Nonrecursive 
                            [Vb.mk  
                               (Pat.var  {loc ; txt = little_con_name})
                               (Exp.constraint_
                                  (Exp.construct {loc ; txt = Longident.Lident con_name } None)
                                  core_type
                               )
                            ]
                        else 
                          begin 
                            let vars = 
                              Ext_list.init  arity (fun x -> "param_" ^ string_of_int x ) in 
                            let exp = 
                              Exp.constraint_
                                ( 
                                  Exp.construct {loc ; txt = Longident.Lident con_name} @@ 
                                  Some
                                    ( 
                                      if  arity = 1 then 
                                        Exp.ident { loc ; txt = Longident.Lident (List.hd vars )}
                                      else 
                                        Exp.tuple (Ext_list.map 
                                                     (fun x -> Exp.ident {loc ; txt = Longident.Lident x})
                                                     vars 
                                                  ) )) core_type
                            in 
                            let fun_ = 
                              Ext_list.fold_right  (fun var b -> 
                                  Exp.fun_ "" None  (Pat.var {loc ; txt = var}) b 
                                ) vars exp  in 

                            Str.value Nonrecursive
                              [
                                Vb.mk 
                                  (Pat.var { loc ; txt  = little_con_name} )
                                  fun_
                              ]
                          end
                    )
                | _ -> []
                (* Location.raise_errorf "projector only works with record" *)
              in Ext_list.flat_map handle_tdcl tdcls


            end;
          signature_gen = 
            begin fun (tdcls : Parsetree.type_declaration list) _explict_nonrec -> 
              let handle_tdcl tdcl = 
                let core_type = Ast_derive_util.core_type_of_type_declaration tdcl in 
                match tdcl with 
                | {ptype_kind = 
                     Ptype_record label_declarations }
                  -> 
                  label_declarations 
                  |> 
                  Ext_list.map (fun 
                             ({pld_name = {loc; txt = pld_label} as pld_name;
                               pld_type
                              } : 
                                Parsetree.label_declaration) -> 
                             Sig.value 
                               (Val.mk pld_name 
                                  (Typ.arrow "" core_type pld_type )))
                | {ptype_kind = 
                     Ptype_variant constructor_declarations 
                  } -> 
                  constructor_declarations
                  |>
                  Ext_list.map
                    (fun  ({pcd_name = {loc ; txt = con_name} ; pcd_args ; pcd_loc }:
                             Parsetree.constructor_declaration)
                      -> 
                        Sig.value 
                          (Val.mk {loc ; txt = (String.uncapitalize con_name)}
                             
                           (Ext_list.fold_right 
                              (fun x acc -> Typ.arrow "" x acc) 
                              pcd_args
                              core_type)
                          )
                    )
                           
                  | _ -> [] 
              in 
              Ext_list.flat_map handle_tdcl tdcls
            end;
          expression_gen = None
         }
    end

