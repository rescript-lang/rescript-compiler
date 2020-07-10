let defaultPrintWidth = 100

let print (flavor : [`res | `refmt of string]) ~input =
  let isInterface =
    let len = String.length input in
    len > 0 && String.unsafe_get input (len - 1) = 'i'
  in
  match flavor with
  | `res ->
    if isInterface then
      let parseResult =
        Napkin_driver.parsingEngine.parseInterface ~forPrinter:true ~filename:input
      in
      if not parseResult.invalid then
        Napkin_printer.printInterface
          ~width:defaultPrintWidth
          parseResult.parsetree
          parseResult.comments
      else
        let msg =
          let style = Napkin_diagnostics.parseReportStyle "" in
          Napkin_diagnostics.stringOfReport ~style parseResult.diagnostics parseResult.source
        in
        raise (Location.Error (Location.error msg))
    else
      let parseResult =
        Napkin_driver.parsingEngine.parseImplementation ~forPrinter:true ~filename:input
      in
      if not parseResult.invalid then
        Napkin_printer.printImplementation
          ~width:defaultPrintWidth
          parseResult.parsetree
          parseResult.comments
      else
        let msg =
          let style = Napkin_diagnostics.parseReportStyle "" in
          Napkin_diagnostics.stringOfReport ~style parseResult.diagnostics parseResult.source
        in
        raise (Location.Error (Location.error msg))
  | `refmt path ->
    let (filename, chan) = Filename.open_temp_file "refmt" (if isInterface then ".rei" else ".re") in
    output_string chan input;
    close_out(chan);
    let cmd = Printf.sprintf "%s --print=binary --interface=%b --in-place %s" path isInterface filename in
    print_string cmd;
    ignore (Sys.command cmd);
    let _ = assert false in
    let result =
      if isInterface then
        let parseResult =
          Napkin_reason_binary_driver.parsingEngine.parseInterface ~forPrinter:true ~filename in
        Napkin_printer.printInterface
          ~width:defaultPrintWidth
          parseResult.parsetree
          parseResult.comments
      else
        let parseResult =
          Napkin_reason_binary_driver.parsingEngine.parseImplementation ~forPrinter:true ~filename in
        Napkin_printer.printImplementation
          ~width:defaultPrintWidth
          parseResult.parsetree
          parseResult.comments
    in
    Sys.remove filename;
    result
