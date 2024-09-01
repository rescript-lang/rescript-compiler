let default_print_width = 100

(* Look at rescript.json (or bsconfig.json) to set Uncurried or Legacy mode if it contains "uncurried": false *)
let get_uncurried_from_config ~filename =
  let rec find_config ~dir =
    let config = Filename.concat dir "rescript.json" in
    if Sys.file_exists config then Some (Res_io.read_file ~filename:config)
    else
      let config = Filename.concat dir "bsconfig.json" in
      if Sys.file_exists config then Some (Res_io.read_file ~filename:config)
      else
        let parent = Filename.dirname dir in
        if parent = dir then None else find_config ~dir:parent
  in
  let rec find_from_node_modules ~dir =
    let parent = Filename.dirname dir in
    if Filename.basename dir = "node_modules" then
      let config = Filename.concat parent "rescript.json" in
      if Sys.file_exists config then Some (Res_io.read_file ~filename:config)
      else
        let config = Filename.concat parent "bsconfig.json" in
        if Sys.file_exists config then Some (Res_io.read_file ~filename:config)
        else None
    else if parent = dir then None
    else find_from_node_modules ~dir:parent
  in
  let dir =
    if Filename.is_relative filename then
      Filename.dirname (Filename.concat (Sys.getcwd ()) filename)
    else Filename.dirname filename
  in
  let config () =
    match find_config ~dir with
    | None ->
      (* The editor calls format on a temporary file. So bsconfig can't be found.
         This looks outside the node_modules containing the bsc binary *)
      let dir = (Filename.dirname Sys.argv.(0) [@doesNotRaise]) in
      find_from_node_modules ~dir
    | x -> x
  in
  match config () with
  | exception _ -> ()
  | None -> ()
  | Some config ->
    let lines = config |> String.split_on_char '\n' in
    let is_legacy_uncurried =
      lines
      |> List.exists (fun line ->
             let is_uncurried_option = ref false in
             let is_option_falsy = ref false in
             let words = line |> String.split_on_char ' ' in
             words
             |> List.iter (fun word ->
                    match word with
                    | "\"uncurried\"" | "\"uncurried\":" ->
                      is_uncurried_option := true
                    | "\"uncurried\":false" | "\"uncurried\":false," ->
                      is_uncurried_option := true;
                      is_option_falsy := true
                    | "false" | ":false" | "false," | ":false," ->
                      is_option_falsy := true
                    | _ -> ());
             !is_uncurried_option && !is_option_falsy)
    in
    if not is_legacy_uncurried then Config.uncurried := Uncurried

(* print res files to res syntax *)
let print_res ~ignore_parse_errors ~is_interface ~filename =
  get_uncurried_from_config ~filename;
  if is_interface then (
    let parse_result =
      Res_driver.parsing_engine.parse_interface ~for_printer:true ~filename
    in
    if parse_result.invalid then (
      Res_diagnostics.print_report parse_result.diagnostics parse_result.source;
      if not ignore_parse_errors then exit 1);
    Res_printer.print_interface ~width:default_print_width
      ~comments:parse_result.comments parse_result.parsetree)
  else
    let parse_result =
      Res_driver.parsing_engine.parse_implementation ~for_printer:true ~filename
    in
    if parse_result.invalid then (
      Res_diagnostics.print_report parse_result.diagnostics parse_result.source;
      if not ignore_parse_errors then exit 1);
    Res_printer.print_implementation ~width:default_print_width
      ~comments:parse_result.comments parse_result.parsetree
[@@raises exit]

(* print ocaml files to res syntax *)
let print_ml ~is_interface ~filename =
  if is_interface then
    let parse_result =
      Res_driver_ml_parser.parsing_engine.parse_interface ~for_printer:true
        ~filename
    in
    Res_printer.print_interface ~width:default_print_width
      ~comments:parse_result.comments parse_result.parsetree
  else
    let parse_result =
      Res_driver_ml_parser.parsing_engine.parse_implementation ~for_printer:true
        ~filename
    in
    Res_printer.print_implementation ~width:default_print_width
      ~comments:parse_result.comments parse_result.parsetree

(* print the given file named input to from "language" to res, general interface exposed by the compiler *)
let print ?(ignore_parse_errors = false) language ~input =
  let is_interface =
    let len = String.length input in
    len > 0 && String.unsafe_get input (len - 1) = 'i'
  in
  match language with
  | `res -> print_res ~ignore_parse_errors ~is_interface ~filename:input
  | `ml -> print_ml ~is_interface ~filename:input
[@@raises exit]

(* suppress unused optional arg *)
let _ = fun s -> print ~ignore_parse_errors:false s [@@raises exit]
