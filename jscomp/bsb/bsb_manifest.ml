let ( // ) = Ext_path.combine

let ( |? ) m (key, cb) = m |> Ext_json.test key cb
let ( .?() ) = Map_string.find_opt

let load_json ~(per_proj_dir : string) ~(warn_legacy_manifest : bool) : string * string * Ext_json_types.t =
  let filename, absolute_path, in_chan =
    let filename = Literals.rescript_json in
    let absolute_path = (per_proj_dir // filename) in
    match open_in absolute_path
    with
    | in_chan -> (filename, absolute_path, in_chan)
    | exception e ->
      let filename = Literals.bsconfig_json in
      let absolute_path = (per_proj_dir // filename) in
      match open_in absolute_path
      with
      | in_chan -> (filename, absolute_path, in_chan)
      | exception _ -> raise e (* forward error from rescript.json *)
  in
  if warn_legacy_manifest && filename = Literals.bsconfig_json then
    print_endline "TODO: deprecation warning" ;
  match
    Ext_json_parse.parse_json_from_chan filename in_chan
  with
  | v -> close_in in_chan ; (filename, absolute_path, v)
  | exception e -> close_in in_chan ; raise e

let parse ~(filename : string) ~(json : Ext_json_types.t) : Bsb_manifest_types.t =
  match json with
  | Obj { map } -> (
    let open Bsb_manifest_fields in
    let package_name, namespace = extract_package_name_and_namespace map in
    let suffix = extract_suffix map in
    {
      package_name;
      namespace;
      warning = extract_warning map;
      external_includes = extract_string_list map Bsb_build_schemas.bs_external_includes;
      bsc_flags = extract_string_list map Bsb_build_schemas.bsc_flags;
      generators = extract_generators map;
      bs_dependencies = extract_string_list map Bsb_build_schemas.bs_dependencies |> Set_string.of_list;
      bs_dev_dependencies = extract_string_list map Bsb_build_schemas.bs_dev_dependencies |> Set_string.of_list;
      pinned_dependencies = extract_string_list map Bsb_build_schemas.pinned_dependencies |> Set_string.of_list;
      ppx_specs = extract_ppx_specs map;
      pp_file = extract_string map Bsb_build_schemas.pp_flags;
      js_post_build_cmd = extract_js_post_build map;
      ignored_dirs = extract_string_list map Bsb_build_schemas.ignored_dirs;
      package_specs = extract_package_specs map ~suffix;
      use_stdlib = extract_boolean map Bsb_build_schemas.use_stdlib true;
      external_stdlib = extract_string map Bsb_build_schemas.external_stdlib;
      suffix;
      reason_react = extract_reason_react map;
      jsx = extract_jsx map;
      cut_generators = extract_boolean map Bsb_build_schemas.cut_generators false;
      uncurried = extract_boolean map Bsb_build_schemas.uncurried true;
    }
  )
  | _ -> Bsb_exception.invalid_spec (filename ^ " expect a json object {}")
