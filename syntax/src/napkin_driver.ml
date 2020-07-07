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
  stringOfDiagnostics: source:string -> filename:string -> 'diagnostics  -> string
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

module IO: sig
  val readFile: string -> string
  val readStdin: unit -> string
end = struct
  (* random chunk size: 2^15, TODO: why do we guess randomly? *)
  let chunkSize = 32768

  let readFile filename =
    let chan = open_in filename in
    let buffer = Buffer.create chunkSize in
    let chunk = (Bytes.create [@doesNotRaise]) chunkSize in
    let rec loop () =
      let len = try input chan chunk 0 chunkSize with Invalid_argument _ -> 0 in
      if len == 0 then (
        close_in_noerr chan;
        Buffer.contents buffer
      ) else (
        Buffer.add_subbytes buffer chunk 0 len;
        loop ()
      )
    in
    loop ()

  let readStdin () =
    let buffer = Buffer.create chunkSize in
    let chunk = (Bytes.create [@doesNotRaise]) chunkSize in
    let rec loop () =
      let len = try input stdin chunk 0 chunkSize with Invalid_argument _ -> 0 in
      if len == 0 then (
        close_in_noerr stdin;
        Buffer.contents buffer
      ) else (
        Buffer.add_subbytes buffer chunk 0 len;
        loop ()
      )
    in
    loop ()
end

let setup ~filename ~forPrinter () =
  let src = if filename = "" then IO.readStdin () else IO.readFile filename in
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
    let style = Napkin_diagnostics.parseReportStyle "" in
    Napkin_diagnostics.stringOfReport ~style diagnostics source
  end;
}

let printEngine = {
  printImplementation = begin fun ~width ~filename:_ ~comments structure ->
    print_string (Napkin_printer.printImplementation ~width structure comments)
  end;
  printInterface = begin fun ~width ~filename:_ ~comments signature ->
    print_string (Napkin_printer.printInterface ~width signature comments)
  end;
}

let parse_implementation sourcefile =
  Location.input_name := sourcefile;
  let parseResult =
    parsingEngine.parseImplementation ~forPrinter:false ~filename:sourcefile
  in
  let () = if parseResult.invalid then
    let msg =
      let style = Napkin_diagnostics.parseReportStyle "" in
      Napkin_diagnostics.stringOfReport ~style parseResult.diagnostics parseResult.source
    in
    raise (Location.Error (Location.error msg))
  in
  parseResult.parsetree
  [@@raises Location.Error]

let parse_interface sourcefile =
  Location.input_name := sourcefile;
  let parseResult = parsingEngine.parseInterface ~forPrinter:false ~filename:sourcefile in
  let () = if parseResult.invalid then
    let msg =
      let style = Napkin_diagnostics.parseReportStyle "" in
      Napkin_diagnostics.stringOfReport ~style parseResult.diagnostics parseResult.source
    in
    raise (Location.Error (Location.error msg))
  in
  parseResult.parsetree
  [@@raises Location.Error]
