module IO = Napkin_io

let defaultPrintWidth = 100

(* print res files to res syntax *)
let printRes ~isInterface ~filename =
  if isInterface then
    let parseResult =
      Napkin_driver.parsingEngine.parseInterface ~forPrinter:true ~filename
    in
    if parseResult.invalid then
      begin
        let style = Napkin_diagnostics.parseReportStyle "" in
        let msg = Napkin_diagnostics.stringOfReport ~style parseResult.diagnostics parseResult.source in
        print_endline msg;
        exit 1
      end
    else
      Napkin_printer.printInterface
        ~width:defaultPrintWidth
        ~comments:parseResult.comments
        parseResult.parsetree
  else
    let parseResult =
      Napkin_driver.parsingEngine.parseImplementation ~forPrinter:true ~filename
    in
    if parseResult.invalid then
      begin
        let style = Napkin_diagnostics.parseReportStyle "" in
        let msg = Napkin_diagnostics.stringOfReport ~style parseResult.diagnostics parseResult.source in
        print_endline msg;
        exit 1
      end
    else
      Napkin_printer.printImplementation
        ~width:defaultPrintWidth
        ~comments:parseResult.comments
        parseResult.parsetree
[@@raises exit]

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
 * -> open a tempfile
 * -> write the source code found in "filename" into the tempfile
 * -> run refmt in-place in binary mode on the tempfile,
 *    mutates contents tempfile with marshalled AST.j
 * -> read the marshalled ast (from the binary output in the tempfile)
 * -> re-read the original "filename" and extract string + comment data
 * -> put the comment- and string data back into the unmarshalled parsetree
 * -> pretty print to res
 * -> take a deep breath and exhale slowly *)
let printReason ~refmtPath ~isInterface ~filename =
  (* open a tempfile *)
  let (tempFilename, chan) =
    (* refmt is just a prefix, `open_temp_file` takes care of providing a random name
     * It tries 1000 times in the case of a name conflict.
     * In practise this means that we shouldn't worry too much about filesystem races *)
    Filename.open_temp_file "refmt" (if isInterface then ".rei" else ".re") in
  close_out chan;
  (* Write the source code found in "filename" into the tempfile *)
  IO.writeFile ~filename:tempFilename ~contents:(IO.readFile ~filename);
  let cmd = Printf.sprintf "%s --print=binary --in-place --interface=%b %s" refmtPath isInterface tempFilename in
  (* run refmt in-place in binary mode on the tempfile *)
  ignore (Sys.command cmd);
  let result =
    if isInterface then
      let parseResult =
        (* read the marshalled ast (from the binary output in the tempfile) *)
        Napkin_reason_binary_driver.parsingEngine.parseInterface ~forPrinter:true ~filename:tempFilename in
      (* re-read the original "filename" and extract string + comment data *)
      let (comments, stringData) = Napkin_reason_binary_driver.extractConcreteSyntax filename in
      (* put the comment- and string data back into the unmarshalled parsetree *)
      let parseResult = {
        parseResult with
        parsetree =
          parseResult.parsetree |> Napkin_ast_conversion.replaceStringLiteralSignature stringData;
        comments = comments;
      } in
      (* pretty print to res *)
      Napkin_printer.printInterface
        ~width:defaultPrintWidth
        ~comments:parseResult.comments
        parseResult.parsetree
    else
      let parseResult =
        (* read the marshalled ast (from the binary output in the tempfile) *)
        Napkin_reason_binary_driver.parsingEngine.parseImplementation ~forPrinter:true ~filename:tempFilename in
      let (comments, stringData) = Napkin_reason_binary_driver.extractConcreteSyntax filename in
      (* put the comment- and string data back into the unmarshalled parsetree *)
      let parseResult = {
        parseResult with
        parsetree =
          parseResult.parsetree |> Napkin_ast_conversion.replaceStringLiteralStructure stringData;
        comments = comments;
      } in
      (* pretty print to res *)
      Napkin_printer.printImplementation
        ~width:defaultPrintWidth
        ~comments:parseResult.comments
        parseResult.parsetree
  in
  Sys.remove tempFilename;
  result
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
[@@raises Sys_error, exit]
