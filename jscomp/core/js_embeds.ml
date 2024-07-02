let write_embeds ~extension_points ~output ast =
  let content = ref [] in
  let append item = content := item :: !content in
  let extension (iterator : Ast_iterator.iterator) (ext : Parsetree.extension) =
    (match ext with
    | ( {txt},
        PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ( {
                      pexp_loc;
                      pexp_desc = Pexp_constant (Pconst_string (contents, _));
                    },
                    _ );
            };
          ] )
      when extension_points |> List.mem txt ->
      append (pexp_loc, txt, contents)
    | _ -> ());
    Ast_iterator.default_iterator.extension iterator ext
  in
  let iterator = {Ast_iterator.default_iterator with extension} in
  iterator.structure iterator ast;
  match !content with
  | [] -> false
  | content ->
    let text =
      content
      |> List.map (fun (loc, extensionName, contents) ->
             Printf.sprintf "<<- item begin ->>\n%s\n%s\n%i:%i-%i:%i"
               extensionName contents loc.Location.loc_start.pos_lnum
               loc.loc_start.pos_cnum loc.loc_end.pos_lnum loc.loc_end.pos_cnum)
      |> List.rev |> String.concat "\n\n"
    in
    let oc = open_out_bin output in
    output_string oc text;
    close_out oc;
    true