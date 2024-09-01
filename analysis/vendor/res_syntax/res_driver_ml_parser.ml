module OcamlParser = Parser
module IO = Res_io

let setup ~filename =
  if String.length filename > 0 then (
    Location.input_name := filename;
    IO.read_file ~filename |> Lexing.from_string)
  else Lexing.from_channel stdin

let extract_ocaml_concrete_syntax filename =
  let lexbuf =
    if String.length filename > 0 then
      IO.read_file ~filename |> Lexing.from_string
    else Lexing.from_channel stdin
  in
  let string_locs = ref [] in
  let comment_data = ref [] in
  let rec next (prev_tok_end_pos : Lexing.position) () =
    let token = Lexer.token_with_comments lexbuf in
    match token with
    | OcamlParser.COMMENT (txt, loc) ->
      let comment =
        Res_comment.from_ocaml_comment ~loc ~prev_tok_end_pos ~txt
      in
      comment_data := comment :: !comment_data;
      next loc.Location.loc_end ()
    | OcamlParser.STRING (_txt, None) ->
      let open Location in
      let loc =
        {
          loc_start = lexbuf.lex_start_p;
          loc_end = lexbuf.Lexing.lex_curr_p;
          loc_ghost = false;
        }
      in
      let len = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
      let txt =
        Bytes.to_string
          ((Bytes.sub [@doesNotRaise]) lexbuf.Lexing.lex_buffer
             loc.loc_start.pos_cnum len)
      in
      string_locs := (txt, loc) :: !string_locs;
      next lexbuf.Lexing.lex_curr_p ()
    | OcamlParser.EOF -> ()
    | _ -> next lexbuf.Lexing.lex_curr_p ()
  in
  next lexbuf.Lexing.lex_start_p ();
  (List.rev !string_locs, List.rev !comment_data)

let parsing_engine =
  {
    Res_driver.parse_implementation =
      (fun ~for_printer:_ ~filename ->
        let lexbuf = setup ~filename in
        let string_data, comments =
          extract_ocaml_concrete_syntax !Location.input_name
        in
        let structure =
          Parse.implementation lexbuf
          |> Res_ast_conversion.replace_string_literal_structure string_data
          |> Res_ast_conversion.structure
        in
        {
          filename = !Location.input_name;
          source = Bytes.to_string lexbuf.lex_buffer;
          parsetree = structure;
          diagnostics = ();
          invalid = false;
          comments;
        });
    parse_interface =
      (fun ~for_printer:_ ~filename ->
        let lexbuf = setup ~filename in
        let string_data, comments =
          extract_ocaml_concrete_syntax !Location.input_name
        in
        let signature =
          Parse.interface lexbuf
          |> Res_ast_conversion.replace_string_literal_signature string_data
          |> Res_ast_conversion.signature
        in
        {
          filename = !Location.input_name;
          source = Bytes.to_string lexbuf.lex_buffer;
          parsetree = signature;
          diagnostics = ();
          invalid = false;
          comments;
        });
    string_of_diagnostics = (fun ~source:_ ~filename:_ _diagnostics -> ());
  }

let print_engine =
  Res_driver.
    {
      print_implementation =
        (fun ~width:_ ~filename:_ ~comments:_ structure ->
          Pprintast.structure Format.std_formatter structure);
      print_interface =
        (fun ~width:_ ~filename:_ ~comments:_ signature ->
          Pprintast.signature Format.std_formatter signature);
    }
