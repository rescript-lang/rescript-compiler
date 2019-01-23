open Ast_helper

(* @BenHack Copy pasted from Ast_comb to avoid dep on it. *)
let single_non_rec_value  name exp = 
  Str.value Nonrecursive 
    [Vb.mk (Pat.var name) exp]

let single_non_rec_val name ty = 
  Sig.value 
    (Val.mk name ty)



let invalid_config (config : Parsetree.expression) = 
  Location.raise_errorf ~loc:config.pexp_loc "such configuration is not supported"



type tdcls = Parsetree.type_declaration list 

let derivingName = "accessors" 
let init () =
  
  Ast_derive.register
    derivingName
    (fun (x : Parsetree.expression option) ->
       (match x with 
        | Some config -> invalid_config config
        | None -> ());
       {structure_gen = 
          begin fun (tdcls : tdcls) _explict_nonrec ->
            let handle_tdcl tdcl = 
              let core_type = Ast_derive_util.core_type_of_type_declaration tdcl in 
              match tdcl.ptype_kind with 
              | Ptype_record label_declarations 
                -> 
                label_declarations 
                |> Ext_list.map (
                  fun ({pld_name = {loc; txt = pld_label} as pld_name} : Parsetree.label_declaration) -> 
                    let txt = "param" in
                    single_non_rec_value pld_name
                      (Exp.fun_ "" None
                         (Pat.constraint_ (Pat.var {txt ; loc}) core_type )
                         (Exp.field (Exp.ident {txt = Lident txt ; loc}) 
                            {txt = Longident.Lident pld_label ; loc}) )
                )
              | Ptype_variant constructor_declarations 
                -> 
                constructor_declarations
                |> Ext_list.map 
                  (fun
                    ( {pcd_name = {loc ; txt = con_name} ; pcd_args ; pcd_loc }:
                        Parsetree.constructor_declaration)
                    -> (* TODO: add type annotations *)
                      let little_con_name = String.uncapitalize con_name  in
                      let arity = List.length pcd_args in 
                      single_non_rec_value {loc ; txt = little_con_name}
                        (
                          if arity = 0 then (*TODO: add a prefix, better inter-op with FFI *)
                            (Exp.constraint_
                               (Exp.construct {loc ; txt = Longident.Lident con_name } None)
                               core_type
                            )
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
                              Ext_list.fold_right  (fun var b -> 
                                  Exp.fun_ "" None  (Pat.var {loc ; txt = var}) b 
                                ) vars exp  

                            end)
                  )
              | Ptype_abstract | Ptype_open ->
                Ast_derive_util.notApplicable tdcl.ptype_loc derivingName ; 
               []
              (* Location.raise_errorf "projector only works with record" *)
            in Ext_list.flat_map handle_tdcl tdcls


          end;
        signature_gen = 
          begin fun (tdcls : Parsetree.type_declaration list) _explict_nonrec -> 
            let handle_tdcl tdcl = 
              let core_type = Ast_derive_util.core_type_of_type_declaration tdcl in 
              match tdcl.ptype_kind with 
              | Ptype_record label_declarations 
                -> 
                label_declarations 
                |> Ext_list.map 
                  (fun 
                    ({pld_name ;
                      pld_type
                     } : 
                       Parsetree.label_declaration) -> 
                    single_non_rec_val pld_name (Typ.arrow "" core_type pld_type )
                  )
              | Ptype_variant constructor_declarations 
                -> 
                constructor_declarations
                |>
                Ext_list.map
                  (fun  ({pcd_name = {loc ; txt = con_name} ; pcd_args ; pcd_loc }:
                           Parsetree.constructor_declaration)
                    -> 
                      single_non_rec_val {loc ; txt = (String.uncapitalize con_name)}
                        (Ext_list.fold_right 
                           (fun x acc -> Typ.arrow "" x acc) 
                           pcd_args
                           core_type))
              | Ptype_open | Ptype_abstract -> 
              Ast_derive_util.notApplicable tdcl.ptype_loc derivingName ; 
              [] 
            in 
            Ext_list.flat_map handle_tdcl tdcls
          end;
        expression_gen = None
       }
    )

