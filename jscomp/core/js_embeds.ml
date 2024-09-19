let escape text =
  let ln = String.length text in
  let buf = Buffer.create ln in
  let rec loop i =
    if i < ln then (
      (match text.[i] with
      | '\012' -> Buffer.add_string buf "\\f"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c);
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

let write_text output text =
  let oc = open_out_bin output in
  output_string oc text;
  close_out oc

let write_embeds ~extension_points ~module_filename ~output ast =
  match extension_points with
  | [] -> write_text output "[]"
  | extension_points -> (
    let content = ref [] in
    let append item = content := item :: !content in
    let extension (iterator : Ast_iterator.iterator) (ext : Parsetree.extension)
        =
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
    | [] -> write_text output "[]"
    | content ->
      let counts = Hashtbl.create 10 in
      let text =
        "[\n"
        ^ (content |> List.rev
          |> List.map (fun (loc, extension_name, contents) ->
                 let current_tag_count =
                   match Hashtbl.find_opt counts extension_name with
                   | None -> 0
                   | Some count -> count
                 in
                 let tag_count = current_tag_count + 1 in
                 Hashtbl.replace counts extension_name tag_count;

                 let target_file_name =
                   Printf.sprintf "%s.res"
                     (Bs_embed_lang.make_embed_target_module_name
                        ~module_filename ~extension_name ~tag_count)
                 in
                 Printf.sprintf
                   "  {\n\
                   \    \"tag\": \"%s\",\n\
                   \    \"filename\": \"%s\",\n\
                   \    \"contents\": \"%s\",\n\
                   \    \"loc\": {\"start\": {\"line\": %s, \"col\": %s}, \
                    \"end\": {\"line\": %s, \"col\": %s}}\n\
                   \  }" (escape extension_name) target_file_name
                   (escape contents)
                   (loc.Location.loc_start.pos_lnum |> string_of_int)
                   ((loc.loc_start.pos_cnum - loc.loc_start.pos_bol) |> string_of_int)
                   (loc.loc_end.pos_lnum |> string_of_int)
                   ((loc.loc_end.pos_cnum - loc.loc_end.pos_bol) |> string_of_int))
          |> String.concat ",\n")
        ^ "\n]"
      in
      write_text output text)
