let defaultPrintWidth = 100

(* Look at rescript.json (or bsconfig.json) to set Uncurried or Legacy mode if it contains "uncurried": false *)
let getUncurriedFromConfig ~filename =
  let rec findConfig ~dir =
    let config = Filename.concat dir "rescript.json" in
    if Sys.file_exists config then Some (Res_io.readFile ~filename:config)
    else
      let config = Filename.concat dir "bsconfig.json" in
      if Sys.file_exists config then Some (Res_io.readFile ~filename:config)
      else
        let parent = Filename.dirname dir in
        if parent = dir then None else findConfig ~dir:parent
  in
  let rec findFromNodeModules ~dir =
    let parent = Filename.dirname dir in
    if Filename.basename dir = "node_modules" then
      let config = Filename.concat parent "rescript.json" in
      if Sys.file_exists config then Some (Res_io.readFile ~filename:config)
      else
        let config = Filename.concat parent "bsconfig.json" in
        if Sys.file_exists config then Some (Res_io.readFile ~filename:config)
        else None
    else if parent = dir then None
    else findFromNodeModules ~dir:parent
  in
  let dir =
    if Filename.is_relative filename then
      Filename.dirname (Filename.concat (Sys.getcwd ()) filename)
    else Filename.dirname filename
  in
  let config () =
    match findConfig ~dir with
    | None ->
      (* The editor calls format on a temporary file. So bsconfig can't be found.
         This looks outside the node_modules containing the bsc binary *)
      let dir = (Filename.dirname Sys.argv.(0) [@doesNotRaise]) in
      findFromNodeModules ~dir
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
let printRes ~ignoreParseErrors ~isInterface ~filename =
  getUncurriedFromConfig ~filename;
  if isInterface then (
    let parseResult =
      Res_driver.parsingEngine.parseInterface ~forPrinter:true ~filename
    in
    if parseResult.invalid then (
      Res_diagnostics.printReport parseResult.diagnostics parseResult.source;
      if not ignoreParseErrors then exit 1);
    Res_printer.printInterface ~width:defaultPrintWidth
      ~comments:parseResult.comments parseResult.parsetree)
  else
    let parseResult =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:true ~filename
    in
    if parseResult.invalid then (
      Res_diagnostics.printReport parseResult.diagnostics parseResult.source;
      if not ignoreParseErrors then exit 1);
    Res_printer.printImplementation ~width:defaultPrintWidth
      ~comments:parseResult.comments parseResult.parsetree
  [@@raises exit]

(* print ocaml files to res syntax *)
let printMl ~isInterface ~filename =
  if isInterface then
    let parseResult =
      Res_driver_ml_parser.parsingEngine.parseInterface ~forPrinter:true
        ~filename
    in
    Res_printer.printInterface ~width:defaultPrintWidth
      ~comments:parseResult.comments parseResult.parsetree
  else
    let parseResult =
      Res_driver_ml_parser.parsingEngine.parseImplementation ~forPrinter:true
        ~filename
    in
    Res_printer.printImplementation ~width:defaultPrintWidth
      ~comments:parseResult.comments parseResult.parsetree

(* print the given file named input to from "language" to res, general interface exposed by the compiler *)
let print ?(ignoreParseErrors = false) language ~input =
  let isInterface =
    let len = String.length input in
    len > 0 && String.unsafe_get input (len - 1) = 'i'
  in
  match language with
  | `res -> printRes ~ignoreParseErrors ~isInterface ~filename:input
  | `ml -> printMl ~isInterface ~filename:input
  [@@raises exit]

(* suppress unused optional arg *)
let _ = fun s -> print ~ignoreParseErrors:false s [@@raises exit]
