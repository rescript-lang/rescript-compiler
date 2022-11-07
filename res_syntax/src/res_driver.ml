module IO = Res_io

type ('ast, 'diagnostics) parseResult = {
  filename: string; [@live]
  source: string;
  parsetree: 'ast;
  diagnostics: 'diagnostics;
  invalid: bool;
  comments: Res_comment.t list;
}

type 'diagnostics parsingEngine = {
  parseImplementation:
    forPrinter:bool ->
    filename:string ->
    (Parsetree.structure, 'diagnostics) parseResult;
  parseInterface:
    forPrinter:bool ->
    filename:string ->
    (Parsetree.signature, 'diagnostics) parseResult;
  stringOfDiagnostics: source:string -> filename:string -> 'diagnostics -> unit;
}

type printEngine = {
  printImplementation:
    width:int ->
    filename:string ->
    comments:Res_comment.t list ->
    Parsetree.structure ->
    unit;
  printInterface:
    width:int ->
    filename:string ->
    comments:Res_comment.t list ->
    Parsetree.signature ->
    unit;
}

let setup ~filename ~forPrinter () =
  let src = IO.readFile ~filename in
  let mode = if forPrinter then Res_parser.Default else ParseForTypeChecker in
  Res_parser.make ~mode src filename

let setupFromSource ~displayFilename ~source ~forPrinter () =
  let mode = if forPrinter then Res_parser.Default else ParseForTypeChecker in
  Res_parser.make ~mode source displayFilename

let parsingEngine =
  {
    parseImplementation =
      (fun ~forPrinter ~filename ->
        let engine = setup ~filename ~forPrinter () in
        let structure = Res_core.parseImplementation engine in
        let invalid, diagnostics =
          match engine.diagnostics with
          | [] as diagnostics -> (false, diagnostics)
          | _ as diagnostics -> (true, diagnostics)
        in
        {
          filename = engine.scanner.filename;
          source = engine.scanner.src;
          parsetree = structure;
          diagnostics;
          invalid;
          comments = List.rev engine.comments;
        });
    parseInterface =
      (fun ~forPrinter ~filename ->
        let engine = setup ~filename ~forPrinter () in
        let signature = Res_core.parseSpecification engine in
        let invalid, diagnostics =
          match engine.diagnostics with
          | [] as diagnostics -> (false, diagnostics)
          | _ as diagnostics -> (true, diagnostics)
        in
        {
          filename = engine.scanner.filename;
          source = engine.scanner.src;
          parsetree = signature;
          diagnostics;
          invalid;
          comments = List.rev engine.comments;
        });
    stringOfDiagnostics =
      (fun ~source ~filename:_ diagnostics ->
        Res_diagnostics.printReport diagnostics source);
  }

let parseImplementationFromSource ~forPrinter ~displayFilename ~source =
  let engine = setupFromSource ~displayFilename ~source ~forPrinter () in
  let structure = Res_core.parseImplementation engine in
  let invalid, diagnostics =
    match engine.diagnostics with
    | [] as diagnostics -> (false, diagnostics)
    | _ as diagnostics -> (true, diagnostics)
  in
  {
    filename = engine.scanner.filename;
    source = engine.scanner.src;
    parsetree = structure;
    diagnostics;
    invalid;
    comments = List.rev engine.comments;
  }

let parseInterfaceFromSource ~forPrinter ~displayFilename ~source =
  let engine = setupFromSource ~displayFilename ~source ~forPrinter () in
  let signature = Res_core.parseSpecification engine in
  let invalid, diagnostics =
    match engine.diagnostics with
    | [] as diagnostics -> (false, diagnostics)
    | _ as diagnostics -> (true, diagnostics)
  in
  {
    filename = engine.scanner.filename;
    source = engine.scanner.src;
    parsetree = signature;
    diagnostics;
    invalid;
    comments = List.rev engine.comments;
  }

let printEngine =
  {
    printImplementation =
      (fun ~width ~filename:_ ~comments structure ->
        print_string
          (Res_printer.printImplementation ~width structure ~comments));
    printInterface =
      (fun ~width ~filename:_ ~comments signature ->
        print_string (Res_printer.printInterface ~width signature ~comments));
  }

let parse_implementation sourcefile =
  Location.input_name := sourcefile;
  let parseResult =
    parsingEngine.parseImplementation ~forPrinter:false ~filename:sourcefile
  in
  if parseResult.invalid then (
    Res_diagnostics.printReport parseResult.diagnostics parseResult.source;
    exit 1);
  parseResult.parsetree
  [@@raises exit]

let parse_interface sourcefile =
  Location.input_name := sourcefile;
  let parseResult =
    parsingEngine.parseInterface ~forPrinter:false ~filename:sourcefile
  in
  if parseResult.invalid then (
    Res_diagnostics.printReport parseResult.diagnostics parseResult.source;
    exit 1);
  parseResult.parsetree
  [@@raises exit]
