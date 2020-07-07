(* command line flags *)
module NapkinClflags: sig
  val recover: bool ref
  val print: string ref
  val width: int ref
  val origin: string ref
  val files: string list ref
  val interface: bool ref
  val report: string ref

  val parse: unit -> unit
end = struct
  let recover = ref false
  let width = ref 100

  let files = ref []
  let addFilename filename = files := filename::(!files)

  let print = ref ""
  let origin = ref ""
  let interface = ref false
  let report = ref "pretty"

  let usage = "Usage: napkinscript <options> <file>\nOptions are:"

  let spec = [
    ("-recover", Arg.Unit (fun () -> recover := true), "Emit partial ast");
    ("-print", Arg.String (fun txt -> print := txt), "Print either binary or napkinscript");
    ("-parse", Arg.String (fun txt -> origin := txt), "Parse reasonBinary or napkinscript");
    ("-width", Arg.Int (fun w -> width := w), "Specify the line length that the printer will wrap on" );
    ("-interface", Arg.Unit (fun () -> interface := true), "Parse as interface");
    (* ("-report", Arg.String (fun txt -> report := txt), "Stylize errors and messages using color and context. Accepts `Pretty` and `Plain`. Default `Plain`") *)
  ]

  let parse () = Arg.parse spec addFilename usage
end

module CliArgProcessor = struct
  type backend = Parser: ('diagnostics) Napkin_driver.parsingEngine -> backend [@@unboxed]

  let processFile ~isInterface ~width ~recover ~origin ~target ~report:_ filename =
    try
      let len = String.length filename in
      let processInterface =
        isInterface || len > 0 && (String.get [@doesNotRaise]) filename (len - 1) = 'i'
      in
      let parsingEngine =
          match origin with
          | "reasonBinary" -> Parser Napkin_reason_binary_driver.parsingEngine
          | "ml" | "ocaml" -> Parser Napkin_ml_parser_driver.parsingEngine
          | _ -> Parser Napkin_driver.parsingEngine
      in
      let printEngine =
        match target with
        | "ns" | "napkinscript" -> Napkin_driver.printEngine
        | "ml" | "ocaml" -> Napkin_ml_parser_driver.printEngine
        | "ast" -> Napkin_ast_debugger.printEngine
        | "sexp" -> Napkin_ast_debugger.sexpPrintEngine
        | _  -> Napkin_binary_driver.printEngine
      in

      let forPrinter = match target with
      | "ns" | "napkinscript" | "sexp" -> true
      | _ -> false
      in

      let Parser backend = parsingEngine in
      if processInterface then
        let parseResult = backend.parseInterface ~forPrinter ~filename in
        if parseResult.invalid then
          let () = prerr_string (
            backend.stringOfDiagnostics
              ~source:parseResult.source
              ~filename:parseResult.filename
              parseResult.diagnostics)
          in
          if recover || not parseResult.invalid then
            printEngine.printInterface
              ~width ~filename ~comments:parseResult.comments parseResult.parsetree
          else
            ()
        else
          printEngine.printInterface
            ~width ~filename ~comments:parseResult.comments parseResult.parsetree
      else
        let parseResult = backend.parseImplementation ~forPrinter ~filename in
        if parseResult.invalid then
          let () = prerr_string (
            backend.stringOfDiagnostics
              ~source:parseResult.source
              ~filename:parseResult.filename
              parseResult.diagnostics)
          in
          if recover || not parseResult.invalid then
            printEngine.printImplementation
              ~width ~filename ~comments:parseResult.comments parseResult.parsetree
          else
            ()
        else
          printEngine.printImplementation
            ~width ~filename ~comments:parseResult.comments parseResult.parsetree
    with
    | Failure txt ->
      prerr_string txt;
      prerr_newline();
      exit 1
    | _ -> exit 1
end

let () =
  if not !Sys.interactive then begin
    NapkinClflags.parse ();
    match !NapkinClflags.files with
    | [] -> (* stdin *)
      CliArgProcessor.processFile
        ~isInterface:!NapkinClflags.interface
        ~width:!NapkinClflags.width
        ~recover:!NapkinClflags.recover
        ~target:!NapkinClflags.print
        ~origin:!NapkinClflags.origin
        ~report:!NapkinClflags.report
        ""
    | files ->
      List.iter (fun filename ->
          CliArgProcessor.processFile
            ~isInterface:!NapkinClflags.interface
            ~width:!NapkinClflags.width
            ~recover:!NapkinClflags.recover
            ~target:!NapkinClflags.print
            ~origin:!NapkinClflags.origin
            ~report:!NapkinClflags.report
            filename
        ) files

  end
