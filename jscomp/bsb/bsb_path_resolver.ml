let ( // ) = Ext_path.combine

let extract_path_from_bsconfig (map : Ext_json_types.t Map_string.t) package =
  match Map_string.find_opt map Bsb_build_schemas.paths with
  | Some (Obj { map }) ->
    (match Map_string.find_opt map package with
    | Some (Str { str }) -> Some str
    | _ -> None)
  | _ -> None

let rec find_path_recursively cwd (map : Ext_json_types.t Map_string.t) package =
  match extract_path_from_bsconfig map package with
  | Some path -> Some (cwd // path)
  | None ->
    (try
       let dir = Ext_path.find_package_json_dir (Filename.dirname cwd) in
       let json = Ext_json_parse.parse_json_from_file (dir // Literals.bsconfig_json) in
       match json with
       | Obj { map } -> find_path_recursively dir map package
       | _ -> None
     with
    | _ -> None)
