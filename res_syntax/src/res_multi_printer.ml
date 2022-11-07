let defaultPrintWidth = 100

(* print res files to res syntax *)
let printRes ~isInterface ~filename =
  if isInterface then
    let parseResult =
      Res_driver.parsingEngine.parseInterface ~forPrinter:true ~filename
    in
    if parseResult.invalid then (
      Res_diagnostics.printReport parseResult.diagnostics parseResult.source;
      exit 1)
    else
      Res_printer.printInterface ~width:defaultPrintWidth
        ~comments:parseResult.comments parseResult.parsetree
  else
    let parseResult =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:true ~filename
    in
    if parseResult.invalid then (
      Res_diagnostics.printReport parseResult.diagnostics parseResult.source;
      exit 1)
    else
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
let print language ~input =
  let isInterface =
    let len = String.length input in
    len > 0 && String.unsafe_get input (len - 1) = 'i'
  in
  match language with
  | `res -> printRes ~isInterface ~filename:input
  | `ml -> printMl ~isInterface ~filename:input
  [@@raises exit]
