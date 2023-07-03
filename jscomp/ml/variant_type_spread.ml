let mk_constructor_comes_from_spread_attr () : Parsetree.attribute =
  (Location.mknoloc "res.constructor_from_spread", PStr [])

type variant_type_spread_error = CouldNotFindType

exception VariantTypeSpreadError of Location.t * variant_type_spread_error

(* Spreads in variants are parsed as constructors named "...", with a single payload that's an identifier
   pointing to the type that's spread. We need to expand those constructors as soon as we can, before type
   checking. So, here we look for constructors named "...", look up their type, and add the constructors that
   type itself has.
*)
let expand_variant_spreads (env : Env.t)
    (sdecl_list : Parsetree.type_declaration list) =
  sdecl_list
  |> List.map (fun (sdecl : Parsetree.type_declaration) ->
         match sdecl with
         | {ptype_kind = Ptype_variant constructors} ->
           {
             sdecl with
             ptype_kind =
               Ptype_variant
                 (constructors
                 |> List.map (fun (c : Parsetree.constructor_declaration) ->
                        match c with
                        | {
                         pcd_name = {txt = "..."};
                         pcd_args =
                           Pcstr_tuple
                             [{ptyp_loc; ptyp_desc = Ptyp_constr (loc, [])}];
                        } -> (
                          (* This is a variant type spread constructor. Look up its type *)
                          try
                            let _path, type_decl =
                              Typetexp.find_type env ptyp_loc loc.txt
                            in
                            match type_decl with
                            | {type_kind = Type_variant cstrs; type_attributes}
                              ->
                              Variant_coercion
                              .variant_configuration_can_be_coerced_raises
                                ~is_spread_context:true ~left_loc:loc.loc
                                ~left_attributes:type_attributes
                                ~right_attributes:sdecl.ptype_attributes
                                ~right_loc:sdecl.ptype_loc;
                              (* We add back the spread constructor here so the type checker
                                 helps us resolve its type (we'll obviously filter this out
                                 at a later stage). We also append the type identifier so we
                                 can have multiple spreads, since each constructor name needs
                                 to be unique. *)
                              let spread_constructor_name =
                                "..."
                                ^ (Longident.flatten loc.txt
                                 |> String.concat ".")
                              in
                              {
                                c with
                                pcd_name =
                                  {
                                    c.pcd_name with
                                    txt = spread_constructor_name;
                                  };
                              }
                              :: (cstrs
                                 |> List.map
                                      (fun
                                        (cstr : Types.constructor_declaration)
                                        :
                                        Parsetree.constructor_declaration
                                      ->
                                        {
                                          (* This will mark this constructor as originating from a variant type spread.
                                             We use that hint to fill in the real, typed constructor arguments (if any)
                                             at a later stage when that information is available. *)
                                          pcd_attributes =
                                            mk_constructor_comes_from_spread_attr
                                              ()
                                            :: cstr.cd_attributes;
                                          pcd_loc = cstr.cd_loc;
                                          pcd_res = None;
                                          (* It's important that we _don't_ fill in pcd_args here, since we have no way to produce
                                             a valid set of args for the parsetree at this stage. Inserting dummies here instead
                                             of later means that our dummies would end up being typechecked, and we don't want that.

                                             We'll fill in the correct arg types in the type checked version of this constructor later. *)
                                          pcd_args = Pcstr_tuple [];
                                          pcd_name =
                                            Location.mkloc cstr.cd_id.name
                                              cstr.cd_loc;
                                        }))
                            | _ -> [c]
                          with
                          | Variant_coercion.VariantConfigurationError _ as err
                            ->
                            raise err
                          | _ ->
                            (* Did not find type. Can't spread here, report as error that types need to be known before hand. *)
                            raise(VariantTypeSpreadError(loc.loc, CouldNotFindType)))
                        | _ -> [c])
                 |> List.concat);
           }
         | _ -> sdecl)

let constructor_is_from_spread (attrs : Parsetree.attributes) =
  attrs
  |> List.exists (fun (a : Parsetree.attribute) ->
         match a with
         | {txt = "res.constructor_from_spread"}, PStr [] -> true
         | _ -> false)

let remove_is_spread_attribute (attr : Parsetree.attribute) =
  match attr with
  | {txt = "res.constructor_from_spread"}, PStr [] -> false
  | _ -> false

(* Add dummy arguments of the right length to constructors that comes
   from spreads, and that has arguments. *)
let expand_dummy_constructor_args (sdecl_list : Parsetree.type_declaration list)
    (decls : (Ident.t * Types.type_declaration) list) =
  List.map2
    (fun sdecl (_, decl) ->
      match (sdecl, decl) with
      | ( {Parsetree.ptype_kind = Ptype_variant c1},
          {Types.type_kind = Type_variant c2} ) ->
        {
          sdecl with
          ptype_kind =
            Ptype_variant
              (c1
              |> List.map (fun (c : Parsetree.constructor_declaration) ->
                     if constructor_is_from_spread c.pcd_attributes then
                       match
                         c2
                         |> List.find_opt
                              (fun (cc : Types.constructor_declaration) ->
                                Ident.name cc.cd_id = c.pcd_name.txt)
                       with
                       | None -> c
                       | Some constructor -> (
                         match constructor with
                         | {cd_args = Cstr_tuple args} ->
                           {
                             c with
                             pcd_attributes =
                               c.pcd_attributes
                               |> List.filter remove_is_spread_attribute;
                             pcd_args =
                               Pcstr_tuple
                                 (args
                                 |> List.map (fun _t ->
                                        {
                                          Parsetree.ptyp_loc = c.pcd_loc;
                                          ptyp_attributes = [];
                                          ptyp_desc = Ptyp_any;
                                        }));
                           }
                         | _ -> c)
                     else c));
        }
      | _ -> sdecl)
    sdecl_list decls
