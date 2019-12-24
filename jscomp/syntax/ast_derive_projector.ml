open Ast_helper

let invalid_config (config : Parsetree.expression) = 
  Location.raise_errorf ~loc:config.pexp_loc "such configuration is not supported"



type tdcls = Parsetree.type_declaration list 

(* #if OCAML_VERSION =~ ">4.03.0" then 
let constructor_arguments_length (xs : Parsetree.constructor_arguments) = 
  match xs with 
  | Pcstr_tuple xs -> List.length xs 
  | Pcstr_record xs -> List.length xs (* inline record FIXME*) 
#else
let constructor_arguments_length = List.length 
#end *)
let derivingName = "accessors" 
let init () =
  
  Ast_derive.register
    derivingName
    (fun (x : Parsetree.expression option) ->
       Ext_option.iter x invalid_config;
       {structure_gen = 
          begin fun (tdcls : tdcls) _explict_nonrec ->
            let handle_tdcl tdcl = 
              let core_type = Ast_derive_util.core_type_of_type_declaration tdcl in 
              match tdcl.ptype_kind with 
              | Ptype_record label_declarations 
                ->                  
                Ext_list.map label_declarations (
                  fun ({pld_name = {loc; txt = pld_label} as pld_name} : Parsetree.label_declaration) -> 
                    let txt = "param" in
                    Ast_comb.single_non_rec_value pld_name
                      (Ast_compatible.fun_
                         (Pat.constraint_ (Pat.var {txt ; loc}) core_type )
                         (Exp.field (Exp.ident {txt = Lident txt ; loc}) 
                            {txt = Longident.Lident pld_label ; loc}) )
                )
              | Ptype_variant constructor_declarations 
                ->                 
                Ext_list.map constructor_declarations
                  (fun {pcd_name = {loc ; txt = con_name} ; pcd_args ; pcd_loc; pcd_res }
                    -> (* TODO: add type annotations *)
                      let pcd_args = 
                        match pcd_args with 
                        | Pcstr_tuple pcd_args -> pcd_args 
                        | Pcstr_record _ -> assert false in  
                      let little_con_name = Ext_string.uncapitalize_ascii con_name  in
                      let arity = List.length pcd_args in 
                      let annotate_type = 
                        match pcd_res with 
                        | None -> core_type
                        | Some x -> x in  
                      Ast_comb.single_non_rec_value {loc ; txt = little_con_name}
                        (
                          if arity = 0 then (*TODO: add a prefix, better inter-op with FFI *)
                            (Exp.constraint_
                               (Exp.construct {loc ; txt = Longident.Lident con_name } None)
                               annotate_type
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
                                      (if  arity = 1 then 
                                          Exp.ident { loc ; txt = Lident (List.hd vars )}
                                        else 
                                          Exp.tuple (Ext_list.map vars 
                                                       (fun x -> Exp.ident {loc ; txt = Lident x})))) annotate_type
                              in 
                              Ext_list.fold_right vars exp (fun var b -> 
                                  Ast_compatible.fun_  (Pat.var {loc ; txt = var}) b 
                                ) 

                            end)
                  )
              | Ptype_abstract | Ptype_open ->
                Ast_derive_util.notApplicable tdcl.ptype_loc derivingName ; 
               []
              (* Location.raise_errorf "projector only works with record" *)
            in Ext_list.flat_map tdcls handle_tdcl


          end;
        signature_gen = 
          begin fun (tdcls : Parsetree.type_declaration list) _explict_nonrec -> 
            let handle_tdcl tdcl = 
              let core_type = Ast_derive_util.core_type_of_type_declaration tdcl in 
              match tdcl.ptype_kind with 
              | Ptype_record label_declarations 
                ->                 
                Ext_list.map label_declarations (fun {pld_name; pld_type}  -> 
                    Ast_comb.single_non_rec_val pld_name (Ast_compatible.arrow core_type pld_type )
                  )
              | Ptype_variant constructor_declarations 
                ->                 
                Ext_list.map constructor_declarations
                  (fun  {pcd_name = {loc ; txt = con_name} ; pcd_args ; pcd_loc; pcd_res}
                    -> 
                      let pcd_args = 
                        match pcd_args with 
                        | Pcstr_tuple pcd_args -> pcd_args 
                        | Pcstr_record _ -> assert false in 
                      let annotate_type = 
                        match pcd_res with
                        | Some x -> x 
                        | None -> core_type in 
                      Ast_comb.single_non_rec_val {loc ; txt = (Ext_string.uncapitalize_ascii con_name)}
                        (Ext_list.fold_right pcd_args annotate_type (fun x acc -> Ast_compatible.arrow x acc)))
              | Ptype_open | Ptype_abstract -> 
              Ast_derive_util.notApplicable tdcl.ptype_loc derivingName ; 
              [] 
            in 
            Ext_list.flat_map tdcls handle_tdcl 
          end;
        expression_gen = None
       }
    )

