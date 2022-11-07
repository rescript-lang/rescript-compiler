module IO = Res_io

let isReasonDocComment (comment : Res_comment.t) =
  let content = Res_comment.txt comment in
  let len = String.length content in
  if len = 0 then true
  else if
    len >= 2
    && String.unsafe_get content 0 = '*'
    && String.unsafe_get content 1 = '*'
  then false
  else if len >= 1 && String.unsafe_get content 0 = '*' then true
  else false

let extractConcreteSyntax filename =
  let commentData = ref [] in
  let stringData = ref [] in
  let src = IO.readFile ~filename in
  let scanner = Res_scanner.make src ~filename in

  let rec next prevEndPos scanner =
    let startPos, endPos, token = Res_scanner.scan scanner in
    match token with
    | Eof -> ()
    | Comment c ->
      Res_comment.setPrevTokEndPos c prevEndPos;
      commentData := c :: !commentData;
      next endPos scanner
    | String _ ->
      let loc =
        {Location.loc_start = startPos; loc_end = endPos; loc_ghost = false}
      in
      let len = endPos.pos_cnum - startPos.pos_cnum in
      let txt = (String.sub [@doesNotRaise]) src startPos.pos_cnum len in
      stringData := (txt, loc) :: !stringData;
      next endPos scanner
    | Lbrace ->
      (* handle {| |} or {sql||sql} quoted strings. We don't care about its contents.
         Why? // abcdef inside the quoted string would otherwise be picked up as an extra comment *)
      Res_scanner.tryAdvanceQuotedString scanner;
      next endPos scanner
    | _ -> next endPos scanner
  in
  next Lexing.dummy_pos scanner;
  let comments =
    !commentData
    |> List.filter (fun c -> not (isReasonDocComment c))
    |> List.rev
  in
  (comments, !stringData)

let parsingEngine =
  {
    Res_driver.parseImplementation =
      (fun ~forPrinter:_ ~filename ->
        let chan, close =
          if String.length filename == 0 then (stdin, fun _ -> ())
          else
            let file_chan = open_in_bin filename in
            let () = seek_in file_chan 0 in
            (file_chan, close_in_noerr)
        in
        let magic = Config.ast_impl_magic_number in
        ignore
          ((really_input_string [@doesNotRaise]) chan (String.length magic));
        let filename = input_value chan in
        let comments, stringData =
          if filename <> "" then extractConcreteSyntax filename else ([], [])
        in
        let ast = input_value chan in
        close chan;
        let structure =
          ast
          |> Res_ast_conversion.replaceStringLiteralStructure stringData
          |> Res_ast_conversion.normalizeReasonArityStructure ~forPrinter:true
          |> Res_ast_conversion.structure
        in
        {
          Res_driver.filename;
          source = "";
          parsetree = structure;
          diagnostics = ();
          invalid = false;
          comments;
        });
    parseInterface =
      (fun ~forPrinter:_ ~filename ->
        let chan, close =
          if String.length filename == 0 then (stdin, fun _ -> ())
          else
            let file_chan = open_in_bin filename in
            let () = seek_in file_chan 0 in
            (file_chan, close_in_noerr)
        in
        let magic = Config.ast_intf_magic_number in
        ignore
          ((really_input_string [@doesNotRaise]) chan (String.length magic));
        let filename = input_value chan in
        let comments, stringData =
          if filename <> "" then extractConcreteSyntax filename else ([], [])
        in
        let ast = input_value chan in
        close chan;
        let signature =
          ast
          |> Res_ast_conversion.replaceStringLiteralSignature stringData
          |> Res_ast_conversion.normalizeReasonAritySignature ~forPrinter:true
          |> Res_ast_conversion.signature
        in
        {
          Res_driver.filename;
          source = "";
          parsetree = signature;
          diagnostics = ();
          invalid = false;
          comments;
        });
    stringOfDiagnostics = (fun ~source:_ ~filename:_ _diagnostics -> ());
  }
