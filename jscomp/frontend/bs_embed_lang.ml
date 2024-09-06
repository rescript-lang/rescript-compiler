let should_transform name = !Js_config.embeds |> List.mem name

let make_embed_target_module_name ~module_filename ~extension_name ~tag_count =
  Printf.sprintf "%s__%s_%i"
    (String.capitalize_ascii module_filename)
    (String.map (fun c -> if c = '.' then '_' else c) extension_name)
    tag_count

let transformed_count = Hashtbl.create 10

let escaped_name_for_ext ?fn_name (ext_name : string) =
  match fn_name with
  | Some fn_name -> ext_name ^ "_" ^ fn_name
  | None -> ext_name

let increment_transformed_count ?fn_name (ext_name : string) =
  let name = escaped_name_for_ext ?fn_name ext_name in
  match Hashtbl.find_opt transformed_count name with
  | None -> Hashtbl.add transformed_count name 1
  | Some count -> Hashtbl.replace transformed_count name (count + 1)

let get_transformed_count ext_name =
  match Hashtbl.find_opt transformed_count ext_name with
  | None -> 0
  | Some count -> count

type transformMode = LetBinding | ModuleBinding

let make_lident ~extension_name ~transform_mode filename =
  let module_name =
    if String.ends_with filename ~suffix:".res" then
      Filename.(chop_suffix (basename filename) ".res")
    else Filename.(chop_suffix (basename filename) ".resi")
  in
  Longident.parse
    (Printf.sprintf "%s%s"
       (make_embed_target_module_name ~module_filename:module_name
          ~extension_name
          ~tag_count:(get_transformed_count extension_name))
       (match transform_mode with
       | LetBinding -> ".default"
       | ModuleBinding -> ""))

let transform_expr expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_extension
      ( {txt = extension_name},
        PStr
          [
            {
              pstr_desc =
                Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (_, _))}, _);
            };
          ] )
    when should_transform extension_name ->
    increment_transformed_count extension_name;
    let loc = expr.pexp_loc in
    let filename = loc.loc_start.pos_fname in
    let lid = make_lident ~extension_name ~transform_mode:LetBinding filename in
    Ast_helper.Exp.ident ~loc {txt = lid; loc}
  | _ -> expr

let structure_item structure_item =
  match structure_item.Parsetree.pstr_desc with
  | Pstr_value
      ( recFlag,
        [
          ({
             pvb_expr =
               {pexp_desc = Pexp_extension ({txt = extension_name}, _)} as expr;
           } as valueBinding);
        ] )
    when should_transform extension_name ->
    {
      structure_item with
      pstr_desc =
        Pstr_value
          (recFlag, [{valueBinding with pvb_expr = transform_expr expr}]);
    }
  | Pstr_include
      ({
         pincl_mod =
           {pmod_desc = Pmod_extension ({txt = extension_name; loc}, _)} as pmod;
       } as pincl)
    when should_transform extension_name ->
    increment_transformed_count extension_name;
    {
      structure_item with
      pstr_desc =
        Pstr_include
          {
            pincl with
            pincl_mod =
              {
                pmod with
                pmod_desc =
                  Pmod_ident
                    {
                      txt =
                        make_lident loc.loc_start.pos_fname ~extension_name
                          ~transform_mode:ModuleBinding;
                      loc;
                    };
              };
          };
    }
  | Pstr_module
      ({
         pmb_expr =
           {pmod_desc = Pmod_extension ({txt = extension_name; loc}, _)} as pmod;
       } as pmb)
    when should_transform extension_name ->
    increment_transformed_count extension_name;
    {
      structure_item with
      pstr_desc =
        Pstr_module
          {
            pmb with
            pmb_expr =
              {
                pmod with
                pmod_desc =
                  Pmod_ident
                    {
                      txt =
                        make_lident loc.loc_start.pos_fname ~extension_name
                          ~transform_mode:ModuleBinding;
                      loc;
                    };
              };
          };
    }
  | _ -> structure_item
