let parse_json ~(per_proj_dir : string) ~(warn_legacy_config : bool)
    : string * string * Ext_json_types.t =
  let ( // ) = Ext_path.combine in
  let filename, abs, in_chan =
    let filename = Literals.rescript_json in
    let abs = (per_proj_dir // filename) in
    match open_in abs
    with
    | in_chan -> (filename, abs, in_chan)
    | exception e ->
      let filename = Literals.bsconfig_json in
      let abs = (per_proj_dir // filename) in
      match open_in abs
      with
      | in_chan -> (filename, abs, in_chan)
      | exception _ -> raise e (* forward error from rescript.json *)
  in
  if warn_legacy_config && filename = Literals.bsconfig_json then
    print_endline "TODO: deprecation warning" ;
  match
    Ext_json_parse.parse_json_from_chan filename in_chan
  with
  | v -> close_in in_chan ; (filename, abs, v)
  | exception e -> close_in in_chan ; raise e
