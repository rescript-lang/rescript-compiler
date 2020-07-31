module IO = Napkin_io

type ('ast, 'diagnostics) parseResult = {
  filename: string; [@live]
  source: string;
  parsetree: 'ast;
  diagnostics: 'diagnostics;
  invalid: bool;
  comments: Napkin_comment.t list
}

type ('diagnostics) parsingEngine = {
  parseImplementation:
    forPrinter:bool -> filename:string
    -> (Parsetree.structure, 'diagnostics) parseResult;
  parseInterface:
    forPrinter:bool -> filename:string
    -> (Parsetree.signature, 'diagnostics) parseResult;
  stringOfDiagnostics: source:string -> filename:string -> 'diagnostics  -> unit
}

type printEngine = {
  printImplementation:
    width: int
    -> filename: string
    -> comments: Napkin_comment.t list
    -> Parsetree.structure
    -> unit;
  printInterface:
    width: int
    -> filename: string
    -> comments: Napkin_comment.t list
    -> Parsetree.signature
    -> unit;
}

let setup ~filename ~forPrinter () =
  let src = if filename = "" then IO.readStdin () else IO.readFile ~filename in
  let mode = if forPrinter then Napkin_parser.Default
    else ParseForTypeChecker
  in
  Napkin_parser.make ~mode src filename

let parsingEngine = {
  parseImplementation = begin fun ~forPrinter ~filename ->
    let engine = setup ~filename ~forPrinter () in
    let structure = Napkin_core.parseImplementation engine in
    let (invalid, diagnostics) = match engine.diagnostics with
    | [] as diagnostics -> (false, diagnostics)
    | _ as diagnostics -> (true, diagnostics)
    in {
      filename = engine.scanner.filename;
      source = Bytes.to_string engine.scanner.src;
      parsetree = structure;
      diagnostics;
      invalid;
      comments = List.rev engine.comments;
    }
  end;
  parseInterface = begin fun ~forPrinter ~filename ->
    let engine = setup ~filename ~forPrinter () in
    let signature = Napkin_core.parseSpecification engine in
    let (invalid, diagnostics) = match engine.diagnostics with
    | [] as diagnostics -> (false, diagnostics)
    | _ as diagnostics -> (true, diagnostics)
    in {
      filename = engine.scanner.filename;
      source = Bytes.to_string engine.scanner.src;
      parsetree = signature;
      diagnostics;
      invalid;
      comments = List.rev engine.comments;
    }
  end;
  stringOfDiagnostics = begin fun ~source ~filename:_ diagnostics ->
    Napkin_diagnostics.printReport diagnostics source
  end;
}

let printEngine = {
  printImplementation = begin fun ~width ~filename:_ ~comments structure ->
    print_string (Napkin_printer.printImplementation ~width structure ~comments)
  end;
  printInterface = begin fun ~width ~filename:_ ~comments signature ->
    print_string (Napkin_printer.printInterface ~width signature ~comments)
  end;
}

let parse_implementation sourcefile =
  Location.input_name := sourcefile;
  let parseResult =
    parsingEngine.parseImplementation ~forPrinter:false ~filename:sourcefile
  in
  if parseResult.invalid then begin
    Napkin_diagnostics.printReport parseResult.diagnostics parseResult.source;
    exit 1
  end;
  parseResult.parsetree
[@@raises exit]

let parse_interface sourcefile =
  Location.input_name := sourcefile;
  let parseResult = parsingEngine.parseInterface ~forPrinter:false ~filename:sourcefile in
  if parseResult.invalid then begin
    Napkin_diagnostics.printReport parseResult.diagnostics parseResult.source;
    exit 1
  end;
  parseResult.parsetree
[@@raises exit]
