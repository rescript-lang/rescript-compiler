open Ast_helper


let () =
  Ast_derive.update 
    "projector" 
    begin fun (x : Parsetree.expression option) ->
       match x with 
       | Some {pexp_loc = loc} 
         -> Location.raise_errorf ~loc "such configuration is not supported"
       | None -> 
         {structure_gen = 
            begin fun (tdcl : Parsetree.type_declaration) _explict_nonrec -> 
              let core_type = Ast_derive_dyn.core_type_of_type_declaration tdcl in 
              match tdcl with 
              | {ptype_kind = 
                   Ptype_record label_declarations }
                -> 
                label_declarations 
                |> 
                List.map (fun ({pld_name = {loc; txt = pld_label} as pld_name} : Parsetree.label_declaration) -> 
                    let txt = "param" in
                    Str.value Nonrecursive

                      [Vb.mk (Pat.var pld_name) @@
                       Exp.fun_ "" None
                         (Pat.constraint_ (Pat.var {txt ; loc}) core_type )
                         (Exp.field (Exp.ident {txt = Lident txt ; loc}) 
                            {txt = Longident.Lident pld_label ; loc}) ]
                  )

              | _ -> Location.raise_errorf "projector only works with record"
            end;
          signature_gen = 
            begin fun (tdcl : Parsetree.type_declaration) _explict_nonrec -> 
              raise Not_found
            end;
          expression_gen = None
         }
    end

