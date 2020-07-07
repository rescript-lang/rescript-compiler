let defaultPrintWidth = 100

let print flavor ~input =
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
      if parseResult.valid then
        Napkin_printer.printInterface
          ~width:defaultPrintWidth
          ~filename:parseResult.filename
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
        Napkin_driver.parsingEngine.parseImplementation ~forPrinter:true ~filename:input
      in
      if parseResult.valid then
        Napkin_printer.printImplementation
          ~width:defaultPrintWidth
          ~filename:parseResult.filename
          ~comments:parseResult.comments
          parseResult.parsetree
      else
        let msg =
          let style = Napkin_diagnostics.parseReportStyle "" in
          Napkin_diagnostics.stringOfReport ~style parseResult.diagnostics parseResult.source
        in
        raise (Location.Error (Location.error msg))
