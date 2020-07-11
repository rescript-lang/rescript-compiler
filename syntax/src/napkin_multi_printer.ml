module IO = Napkin_io

let defaultPrintWidth = 100

(* print res files to res syntax *)
let printRes ~isInterface ~filename =
  if isInterface then
    let parseResult =
      Napkin_driver.parsingEngine.parseInterface ~forPrinter:true ~filename
    in
    if not parseResult.invalid then
      Napkin_printer.printInterface
        ~width:defaultPrintWidth
        ~comments:parseResult.comments
        parseResult.parsetree
    else
      let msg =
        let style = Napkin_diagnostics.parseReportStyle "" in
        Napkin_diagnostics.stringOfReport ~style parseResult.diagnostics parseResult.source
      in
      raise (Location.Error (Location.error msg))
  else
    let parseResult =
      Napkin_driver.parsingEngine.parseImplementation ~forPrinter:true ~filename
    in
    if not parseResult.invalid then
      Napkin_printer.printImplementation
        ~width:defaultPrintWidth
        ~comments:parseResult.comments
        parseResult.parsetree
    else
      let msg =
        let style = Napkin_diagnostics.parseReportStyle "" in
        Napkin_diagnostics.stringOfReport ~style parseResult.diagnostics parseResult.source
      in
      raise (Location.Error (Location.error msg))
[@@raises Location.Error]

(* print ocaml files to res syntax *)
let printMl ~isInterface ~filename =
  if isInterface then
    let parseResult =
      Napkin_ml_parser_driver.parsingEngine.parseInterface ~forPrinter:true ~filename in
    Napkin_printer.printInterface
      ~width:defaultPrintWidth
      ~comments:parseResult.comments
      parseResult.parsetree
  else
    let parseResult =
      Napkin_ml_parser_driver.parsingEngine.parseImplementation ~forPrinter:true ~filename in
    Napkin_printer.printImplementation
      ~width:defaultPrintWidth
      ~comments:parseResult.comments
      parseResult.parsetree

(* How does printing Reason to Res work?
 * -> Run refmt in parallel with the program,
 *    the standard input and standard output of the refmt command are redirected
 *    to pipes connected to the two returned channels
 * -> Read the source code of "filename"
 * -> Write the source code to output channel (i.e. the input of refmt)
 * -> Read the marshalled ast from the input channel (i.e. the output of refmt)
 * -> re-read the original "filename" and extract string + comment data
 * -> put the comment- and string data back into the unmarshalled parsetree
 * -> normalize the ast to conform to the napkin printer
 * -> pretty print to res
 * -> take a deep breath and exhale slowly *)
let printReason ~refmtPath ~isInterface ~filename =
  (* Run refmt in parallel with the program *)
  let refmtCmd = Printf.sprintf "%s --print=binary --interface=%b" refmtPath isInterface in
  let (refmtOutput, refmtInput) = Unix.open_process refmtCmd in
  (* Read the source code of "filename" *)
  let source = IO.readFile ~filename in
  (* Write the source code to output channel (i.e. the input of refmt) *)
  output_string refmtInput source;
  close_out refmtInput;
  (* Read the marshalled ast from the input channel (i.e. the output of refmt) *)
  let magic = if isInterface then Config.ast_intf_magic_number else Config.ast_impl_magic_number in
  ignore ((really_input_string [@doesNotRaise]) refmtOutput (String.length magic));
  ignore (input_value refmtOutput);
  let ast = input_value refmtOutput in
  close_in refmtOutput;
  (* re-read the original "filename" and extract string + comment data *)
  let (comments, stringData) = Napkin_reason_binary_driver.extractConcreteSyntax filename in
  if isInterface then
    let ast = ast
      (* put the comment- and string data back into the unmarshalled parsetree *)
      |> Napkin_ast_conversion.replaceStringLiteralSignature stringData
      (* normalize the ast to conform to the napkin printer *)
      |> Napkin_ast_conversion.normalizeReasonAritySignature ~forPrinter:true
      |> Napkin_ast_conversion.signature
    in
    (* pretty print to res *)
    Napkin_printer.printInterface
      ~width:defaultPrintWidth
      ~comments:comments
      ast
  else
    let ast = ast
      (* put the comment- and string data back into the unmarshalled parsetree *)
      |> Napkin_ast_conversion.replaceStringLiteralStructure stringData
      (* normalize the ast to conform to the napkin printer *)
      |> Napkin_ast_conversion.normalizeReasonArityStructure ~forPrinter:true
      |> Napkin_ast_conversion.structure
    in
    (* pretty print to res *)
    Napkin_printer.printImplementation
      ~width:defaultPrintWidth
      ~comments:comments
      ast
[@@raises Sys_error]

(* print the given file named input to from "language" to res, general interface exposed by the compiler *)
let print language ~input =
  let isInterface =
    let len = String.length input in
    len > 0 && String.unsafe_get input (len - 1) = 'i'
  in
  match language with
  | `res -> printRes ~isInterface ~filename:input
  | `ml -> printMl ~isInterface ~filename:input
  | `refmt path -> printReason ~refmtPath:path ~isInterface ~filename:input
[@@raises Location.Error, Sys_error]
