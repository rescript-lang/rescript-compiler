module Driver: sig
  val processFile:
       isInterface: bool
    -> width: int
    -> recover: bool
    -> origin:string
    -> target:string
    -> string
    -> unit
end = struct
  type 'a file_kind =
    | Structure: Parsetree.structure file_kind
    | Signature: Parsetree.signature file_kind

  let parseNapkin (type a) (kind : a file_kind) p : a =
    match kind with
    | Structure -> NapkinScript.parseImplementation p
    | Signature -> NapkinScript.parseSpecification p

  let parseOcaml (type a) (kind : a file_kind) lexbuf : a =
    match kind with
    | Structure -> ParsetreeCompatibility.structure (Parse.implementation lexbuf)
    | Signature -> Parse.interface lexbuf

  let parseReason (type a) (kind : a file_kind) lexbuf : a =
    let open Refmt_main3.Migrate_parsetree in
    let module Convert = Convert(OCaml_404)(OCaml_406) in
    match kind with
    | Structure ->
      let (ast, _ ) =
        Refmt_main3.Reason_toolchain_reason.implementation lexbuf
      in
      ParsetreeCompatibility.structure (Convert.copy_structure ast)
    | Signature ->
      let (ast, _) =
        Refmt_main3.Reason_toolchain_reason.interface lexbuf
      in
      Convert.copy_signature ast

  let parseNapkinFile kind filename =
    let src = if String.length filename > 0 then
      IO.readFile filename
    else
      IO.readStdin ()
    in
    let p = Parser.make src filename in
    let ast = parseNapkin kind p in
    let report = match p.diagnostics with
    | [] -> None
    | diagnostics ->
      Some(
        Diagnostics.makeReport p.diagnostics (Bytes.to_string p.scanner.src)
      )
    in
    (ast, report, p)

  let parseOcamlFile kind filename =
    let lexbuf = if String.length filename > 0 then
      IO.readFile filename |> Lexing.from_string
    else
      Lexing.from_channel stdin
    in
    let ast = parseOcaml kind lexbuf in
    let lexbuf2 = if String.length filename > 0 then
      IO.readFile filename |> Lexing.from_string
    else
      Lexing.from_channel stdin
    in
    let comments =
      let rec next (prevTokEndPos : Lexing.position) comments lb =
        let token = Lexer.token_with_comments lb in
        match token with
        | OcamlParser.EOF -> comments
        | OcamlParser.COMMENT (txt, loc) ->
          let comment = Comment.fromOcamlComment
            ~loc
            ~prevTokEndPos
            ~txt
          in
          next loc.Location.loc_end (comment::comments) lb
        | _ ->
          next lb.Lexing.lex_curr_p comments lb
      in
      let cmts = next lexbuf2.Lexing.lex_start_p [] lexbuf2 in
      cmts
    in
    let p = Parser.make "" filename in
    p.comments <- comments;
    (ast, None, p)

  let parseReasonFile kind filename =
    let lexbuf = if String.length filename > 0 then
      IO.readFile filename |> Lexing.from_string
    else
      Lexing.from_channel stdin
    in
    let ast =
      let reasonLexer = Refmt_main3.Reason_lexer.init lexbuf in
      parseReason kind reasonLexer
    in
    let lexbuf2 = if String.length filename > 0 then
      IO.readFile filename |> Lexing.from_string
    else
      Lexing.from_channel stdin
    in
    let comments =
      let module RawLex = Refmt_main3.Reason_declarative_lexer in
      let state = RawLex.make () in
      let rec next (prevTokEndPos : Lexing.position) comments state lb =
        let token = RawLex.token state lexbuf2  in
        match token with
        | Refmt_main3.Reason_parser.EOF -> comments
        | Refmt_main3.Reason_parser.COMMENT (txt, loc) ->
          let comment = Comment.fromOcamlComment
            ~loc
            ~prevTokEndPos
            ~txt
          in
          next loc.Location.loc_end (comment::comments) state lb
        | _ ->
          next lb.Lexing.lex_curr_p comments state lb
      in
      let cmts = next lexbuf2.Lexing.lex_curr_p [] state lexbuf2 in
      cmts
    in
    let p = Parser.make "" filename in
    p.comments <- comments;
    (ast, None, p)

  let parseImplementation ~origin filename =
    match origin with
    | "ml" | "ocaml" ->
      parseOcamlFile Structure filename
    | "re" | "reason" ->
      parseReasonFile Structure filename
    | _ ->
      parseNapkinFile Structure filename

  let parseInterface ~origin filename =
    match origin with
    | "ml" | "ocaml" ->
      parseOcamlFile Signature filename
    | "re" | "reason" ->
      parseReasonFile Signature filename
    | _ ->
      parseNapkinFile Signature filename

  let process parseFn printFn recover filename =
    let (ast, report, parserState) =
      Profile.record ~name:"parser" (fun () -> parseFn filename)
    in
    match report with
    | Some report when recover = true ->
      printFn ast parserState;
      prerr_string report;
    | Some report ->
      prerr_string report;
      exit 1
    | None ->
      printFn ast parserState

  type action =
    | ProcessImplementation
    | ProcessInterface

  let printImplementation ~target ~width filename ast _parserState =
    match target with
    | "ml" | "ocaml" ->
      Pprintast.structure Format.std_formatter ast
    | "ns" | "napkinscript" ->
      Printer.printImplementation ~width ast (List.rev _parserState.Parser.comments)
    | "ast" ->
      Printast.implementation Format.std_formatter ast
    | "sexp" ->
      ast |> SexpAst.implementation |> Sexp.toString |> print_string
    | _ -> (* default binary *)
      output_string stdout Config.ast_impl_magic_number;
      output_value stdout filename;
      output_value stdout ast

  let printInterface ~target ~width filename ast _parserState =
    match target with
    | "ml" | "ocaml" -> Pprintast.signature Format.std_formatter ast
    | "ns" | "napkinscript" ->
      Printer.printInterface ~width ast (List.rev _parserState.Parser.comments)
    | "ast" -> Printast.interface Format.std_formatter ast
    | "sexp" ->
      ast |> SexpAst.interface |> Sexp.toString |> print_string
    | _ -> (* default binary *)
      output_string stdout Config.ast_intf_magic_number;
      output_value stdout filename;
      output_value stdout ast

  let processFile ~isInterface ~width ~recover ~origin ~target filename =
    try
      let len = String.length filename in
      let action =
        if isInterface || len > 0 && String.get filename (len - 1) = 'i' then
          ProcessInterface
        else ProcessImplementation
      in
      match action with
      | ProcessImplementation ->
        process
          (parseImplementation ~origin)
          (printImplementation ~target ~width filename) recover filename
      | ProcessInterface ->
        process
          (parseInterface ~origin)
          (printInterface ~target ~width filename) recover filename
    with
    | Failure txt ->
      prerr_string txt;
      prerr_newline();
      exit 1
    | _ -> exit 1
end
