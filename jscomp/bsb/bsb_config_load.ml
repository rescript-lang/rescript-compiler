let ( // ) = Ext_path.combine

let load_json ~(per_proj_dir : string)
    : Ext_json_types.t =
  Ext_json_parse.parse_json_from_file (per_proj_dir // Literals.rescript_json)
