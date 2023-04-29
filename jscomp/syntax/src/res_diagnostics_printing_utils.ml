(*
  This file is taken from ReScript's super_code_frame.ml and super_location.ml
  We're copying the look of ReScript's terminal error reporting.
  See https://github.com/rescript-lang/syntax/pull/77 for the rationale.
  A few lines have been commented out and swapped for their tweaked version.
*)

(* ===== super_code_frame.ml *)

module Super_code_frame = struct
  let digits_count n =
    let rec loop n base count =
      if n >= base then loop n (base * 10) (count + 1) else count
    in
    loop (abs n) 1 0

  let seek_2_lines_before src pos =
    let open Lexing in
    let original_line = pos.pos_lnum in
    let rec loop current_line current_char =
      if current_line + 2 >= original_line then (current_char, current_line)
      else
        loop
          (if (src.[current_char] [@doesNotRaise]) = '\n' then current_line + 1
          else current_line)
          (current_char + 1)
    in
    loop 1 0

  let seek_2_lines_after src pos =
    let open Lexing in
    let original_line = pos.pos_lnum in
    let rec loop current_line current_char =
      if current_char = String.length src then (current_char, current_line)
      else
        match src.[current_char] [@doesNotRaise] with
        | '\n' when current_line = original_line + 2 ->
          (current_char, current_line)
        | '\n' -> loop (current_line + 1) (current_char + 1)
        | _ -> loop current_line (current_char + 1)
    in
    loop original_line pos.pos_cnum

  let leading_space_count str =
    let rec loop i count =
      if i = String.length str then count
      else if (str.[i] [@doesNotRaise]) != ' ' then count
      else loop (i + 1) (count + 1)
    in
    loop 0 0

  let break_long_line max_width line =
    let rec loop pos accum =
      if pos = String.length line then accum
      else
        let chunk_length = min max_width (String.length line - pos) in
        let chunk = (String.sub [@doesNotRaise]) line pos chunk_length in
        loop (pos + chunk_length) (chunk :: accum)
    in
    loop 0 [] |> List.rev

  let filter_mapi f l =
    let rec loop f l i accum =
      match l with
      | [] -> accum
      | head :: rest ->
        let accum =
          match f i head with
          | None -> accum
          | Some result -> result :: accum
        in
        loop f rest (i + 1) accum
    in
    loop f l 0 [] |> List.rev

  (* Spiritual equivalent of
     https://github.com/ocaml/ocaml/blob/414bdec9ae387129b8102cc6bf3c0b6ae173eeb9/utils/misc.ml#L601
  *)
  module Color = struct
    type color =
      | Dim
      (* | Filename *)
      | Err
      | Warn
      | NoColor

    let dim = "\x1b[2m"

    (* let filename = "\x1b[46m" *)
    let err = "\x1b[1;31m"
    let warn = "\x1b[1;33m"
    let reset = "\x1b[0m"

    external isatty : out_channel -> bool = "caml_sys_isatty"

    (* reasonable heuristic on whether colors should be enabled *)
    let should_enable_color () =
      let term = try Sys.getenv "TERM" with Not_found -> "" in
      term <> "dumb" && term <> "" && isatty stderr

    let color_enabled = ref true

    let setup =
      let first = ref true in
      (* initialize only once *)
      fun o ->
        if !first then (
          first := false;
          color_enabled :=
            match o with
            | Some Misc.Color.Always -> true
            | Some Auto -> should_enable_color ()
            | Some Never -> false
            | None -> should_enable_color ());
        ()
  end

  let setup = Color.setup

  type gutter = Number of int | Elided
  type highlighted_string = {s: string; start: int; end_: int}
  type line = {gutter: gutter; content: highlighted_string list}

  (*
  Features:
  - display a line gutter
  - break long line into multiple for terminal display
  - peek 2 lines before & after for context
  - center snippet when it's heavily indented
  - ellide intermediate lines when the reported range is huge
*)
  let print ~is_warning ~src ~startPos ~endPos =
    let open Lexing in
    let indent = 2 in
    let highlight_line_start_line = startPos.pos_lnum in
    let highlight_line_end_line = endPos.pos_lnum in
    let start_line_line_offset, first_shown_line =
      seek_2_lines_before src startPos
    in
    let end_line_line_end_offset, last_shown_line =
      seek_2_lines_after src endPos
    in

    let more_than_5_highlighted_lines =
      highlight_line_end_line - highlight_line_start_line + 1 > 5
    in
    let max_line_digits_count = digits_count last_shown_line in
    (* TODO: change this back to a fixed 100? *)
    (* 3 for separator + the 2 spaces around it *)
    let line_width = 78 - max_line_digits_count - indent - 3 in
    let lines =
      (String.sub [@doesNotRaise]) src start_line_line_offset
        (end_line_line_end_offset - start_line_line_offset)
      |> String.split_on_char '\n'
      |> filter_mapi (fun i line ->
             let line_number = i + first_shown_line in
             if more_than_5_highlighted_lines then
               if line_number = highlight_line_start_line + 2 then
                 Some (Elided, line)
               else if
                 line_number > highlight_line_start_line + 2
                 && line_number < highlight_line_end_line - 1
               then None
               else Some (Number line_number, line)
             else Some (Number line_number, line))
    in
    let leading_space_to_cut =
      lines
      |> List.fold_left
           (fun current_max (_, line) ->
             let leading_spaces = leading_space_count line in
             if String.length line = leading_spaces then
               (* the line's nothing but spaces. Doesn't count *)
               current_max
             else min leading_spaces current_max)
           99999
    in
    let separator = if leading_space_to_cut = 0 then "│" else "┆" in
    let stripped_lines =
      lines
      |> List.map (fun (gutter, line) ->
             let new_content =
               if String.length line <= leading_space_to_cut then
                 [{s = ""; start = 0; end_ = 0}]
               else
                 (String.sub [@doesNotRaise]) line leading_space_to_cut
                   (String.length line - leading_space_to_cut)
                 |> break_long_line line_width
                 |> List.mapi (fun i line ->
                        match gutter with
                        | Elided -> {s = line; start = 0; end_ = 0}
                        | Number line_number ->
                          let highlight_line_start_offset =
                            startPos.pos_cnum - startPos.pos_bol
                          in
                          let highlight_line_end_offset =
                            endPos.pos_cnum - endPos.pos_bol
                          in
                          let start =
                            if i = 0 && line_number = highlight_line_start_line
                            then
                              highlight_line_start_offset - leading_space_to_cut
                            else 0
                          in
                          let end_ =
                            if line_number < highlight_line_start_line then 0
                            else if
                              line_number = highlight_line_start_line
                              && line_number = highlight_line_end_line
                            then
                              highlight_line_end_offset - leading_space_to_cut
                            else if line_number = highlight_line_start_line then
                              String.length line
                            else if
                              line_number > highlight_line_start_line
                              && line_number < highlight_line_end_line
                            then String.length line
                            else if line_number = highlight_line_end_line then
                              highlight_line_end_offset - leading_space_to_cut
                            else 0
                          in
                          {s = line; start; end_})
             in
             {gutter; content = new_content})
    in
    let buf = Buffer.create 100 in
    let open Color in
    let add_ch =
      let last_color = ref NoColor in
      fun color ch ->
        if (not !Color.color_enabled) || !last_color = color then
          Buffer.add_char buf ch
        else
          let ansi =
            match (!last_color, color) with
            | NoColor, Dim -> dim
            (* | NoColor, Filename -> filename *)
            | NoColor, Err -> err
            | NoColor, Warn -> warn
            | _, NoColor -> reset
            | _, Dim -> reset ^ dim
            (* | _, Filename -> reset ^ filename *)
            | _, Err -> reset ^ err
            | _, Warn -> reset ^ warn
          in
          Buffer.add_string buf ansi;
          Buffer.add_char buf ch;
          last_color := color
    in
    let draw_gutter color s =
      for _i = 1 to max_line_digits_count + indent - String.length s do
        add_ch NoColor ' '
      done;
      s |> String.iter (add_ch color);
      add_ch NoColor ' ';
      separator |> String.iter (add_ch Dim);
      add_ch NoColor ' '
    in
    stripped_lines
    |> List.iter (fun {gutter; content} ->
           match gutter with
           | Elided ->
             draw_gutter Dim ".";
             add_ch Dim '.';
             add_ch Dim '.';
             add_ch Dim '.';
             add_ch NoColor '\n'
           | Number line_number ->
             content
             |> List.iteri (fun i line ->
                    let gutter_content =
                      if i = 0 then string_of_int line_number else ""
                    in
                    let gutter_color =
                      if
                        i = 0
                        && line_number >= highlight_line_start_line
                        && line_number <= highlight_line_end_line
                      then if is_warning then Warn else Err
                      else NoColor
                    in
                    draw_gutter gutter_color gutter_content;

                    line.s
                    |> String.iteri (fun ii ch ->
                           let c =
                             if ii >= line.start && ii < line.end_ then
                               if is_warning then Warn else Err
                             else NoColor
                           in
                           add_ch c ch);
                    add_ch NoColor '\n'));
    Buffer.contents buf
end

(* ===== super_location.ml *)
module Super_location = struct
  let fprintf = Format.fprintf

  let setup_colors () =
    Misc.Color.setup !Clflags.color;
    Super_code_frame.setup !Clflags.color

  let print_filename = Location.print_filename

  let print_loc ~normalizedRange ppf (loc : Location.t) =
    setup_colors ();
    let dim_loc ppf = function
      | None -> ()
      | Some ((start_line, start_line_start_char), (end_line, end_line_end_char))
        ->
        if start_line = end_line then
          if start_line_start_char = end_line_end_char then
            fprintf ppf ":@{<dim>%i:%i@}" start_line start_line_start_char
          else
            fprintf ppf ":@{<dim>%i:%i-%i@}" start_line start_line_start_char
              end_line_end_char
        else
          fprintf ppf ":@{<dim>%i:%i-%i:%i@}" start_line start_line_start_char
            end_line end_line_end_char
    in
    fprintf ppf "@{<filename>%a@}%a" print_filename loc.loc_start.pos_fname
      dim_loc normalizedRange

  (* let print ~message_kind intro ppf (loc : Location.t) = *)
  let print ~message_kind intro src ppf (loc : Location.t) =
    (match message_kind with
    | `warning -> fprintf ppf "@[@{<info>%s@}@]@," intro
    | `warning_as_error ->
      fprintf ppf "@[@{<error>%s@} (configured as error) @]@," intro
    | `error -> fprintf ppf "@[@{<error>%s@}@]@," intro);
    (* ocaml's reported line/col numbering is horrible and super error-prone
       when being handled programmatically (or humanly for that matter. If you're
       an ocaml contributor reading this: who the heck reads the character count
       starting from the first erroring character?) *)
    (* let (file, start_line, start_char) = Location.get_pos_info loc.loc_start in *)
    let _file, start_line, start_char = Location.get_pos_info loc.loc_start in
    let _, end_line, end_char = Location.get_pos_info loc.loc_end in
    (* line is 1-indexed, column is 0-indexed. We convert all of them to 1-indexed to avoid confusion *)
    (* start_char is inclusive, end_char is exclusive *)
    let normalizedRange =
      (* TODO: lots of the handlings here aren't needed anymore because the new
         rescript syntax has much stronger invariants regarding positions, e.g.
         no -1 *)
      if start_char == -1 || end_char == -1 then
        (* happens sometimes. Syntax error for example *)
        None
      else if start_line = end_line && start_char >= end_char then
        (* in some errors, starting char and ending char can be the same. But
           since ending char was supposed to be exclusive, here it might end up
           smaller than the starting char if we naively did start_char + 1 to
           just the starting char and forget ending char *)
        let same_char = start_char + 1 in
        Some ((start_line, same_char), (end_line, same_char))
      else
        (* again: end_char is exclusive, so +1-1=0 *)
        Some ((start_line, start_char + 1), (end_line, end_char))
    in
    fprintf ppf "  @[%a@]@," (print_loc ~normalizedRange) loc;
    match normalizedRange with
    | None -> ()
    | Some _ -> (
      try
        (* let src = Ext_io.load_file file in *)
        (* we're putting the line break `@,` here rather than above, because this
           branch might not be reached (aka no inline file content display) so
           we don't wanna end up with two line breaks in the the consequent *)
        fprintf ppf "@,%s"
          (Super_code_frame.print ~is_warning:(message_kind = `warning) ~src
             ~startPos:loc.loc_start ~endPos:loc.loc_end)
      with
      (* this might happen if the file is e.g. "", "_none_" or any of the fake file name placeholders.
         we've already printed the location above, so nothing more to do here. *)
      | Sys_error _ ->
        ())

  (* taken from https://github.com/rescript-lang/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/parsing/location.ml#L380 *)
  (* This is the error report entry point. We'll replace the default reporter with this one. *)
  (* let rec super_error_reporter ppf ({loc; msg; sub} : Location.error) = *)
  let super_error_reporter ppf src ({loc; msg} : Location.error) =
    setup_colors ();
    (* open a vertical box. Everything in our message is indented 2 spaces *)
    (* Format.fprintf ppf "@[<v>@,  %a@,  %s@,@]" (print ~message_kind:`error "We've found a bug for you!") src loc msg; *)
    Format.fprintf ppf "@[<v>@,  %a@,  %s@,@]"
      (print ~message_kind:`error "Syntax error!" src)
      loc msg
  (* List.iter (Format.fprintf ppf "@,@[%a@]" super_error_reporter) sub *)
  (* no need to flush here; location's report_exception (which uses this ultimately) flushes *)
end
