module OcamlParser = Parser
module IO = Res_io

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
