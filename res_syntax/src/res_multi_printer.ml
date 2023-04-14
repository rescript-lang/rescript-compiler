let defaultPrintWidth = 100

(* Determine if the file is in uncurried mode by looking for
   the fist ancestor .bsconfig and see if it contains "uncurried": true *)
let getUncurriedFromBsconfig ~filename =
  let rec findBsconfig ~dir =
    let bsconfig = Filename.concat dir "bsconfig.json" in
    if Sys.file_exists bsconfig then Some (Res_io.readFile ~filename:bsconfig)
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else findBsconfig ~dir:parent
  in
  let rec findFromNodeModules ~dir =
    let parent = Filename.dirname dir in
    if Filename.basename dir = "node_modules" then
      let bsconfig = Filename.concat parent "bsconfig.json" in
      if Sys.file_exists bsconfig then Some (Res_io.readFile ~filename:bsconfig)
      else None
    else if parent = dir then None
    else findFromNodeModules ~dir:parent
  in
  let dir =
    if Filename.is_relative filename then
      Filename.dirname (Filename.concat (Sys.getcwd ()) filename)
    else Filename.dirname filename
  in
  let bsconfig () =
    match findBsconfig ~dir with
    | None ->
      (* The editor calls format on a temporary file. So bsconfig can't be found.
         This looks outside the node_modules containing the bsc binary *)
      let dir = (Filename.dirname Sys.argv.(0) [@doesNotRaise]) in
      findFromNodeModules ~dir
    | x -> x
  in
  match bsconfig () with
  | exception _ -> ()
  | None -> ()
  | Some bsconfig ->
    let lines = bsconfig |> String.split_on_char '\n' in
    let uncurried =
      lines
      |> List.exists (fun line ->
             let uncurried = ref false in
             let true_ = ref false in
             let words = line |> String.split_on_char ' ' in
             words
             |> List.iter (fun word ->
                    match word with
                    | "\"uncurried\"" | "\"uncurried\":" -> uncurried := true
                    | "\"uncurried\":true" | "\"uncurried\":true," ->
                      uncurried := true;
                      true_ := true
                    | "true" | ":true" | "true," | ":true," -> true_ := true
                    | _ -> ());
             !uncurried && !true_)
    in
    if uncurried then Config.uncurried := Uncurried

(* print res files to res syntax *)
let printRes ~ignoreParseErrors ~isInterface ~filename =
  getUncurriedFromBsconfig ~filename;
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
