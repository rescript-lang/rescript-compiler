(*
  This file is taken from BuckleScript's super_misc.ml and super_location.ml
  We're copying the look of BuckleScript's terminal error reporting.
  See https://github.com/BuckleScript/syntax/pull/77 for the rationale.
*)

(* ===== super_misc.ml *)

module Super_misc = struct

let fprintf = Format.fprintf

let string_slice ~start str =
  let last = String.length str in
  if last <= start then "" else (String.sub str start (last - start)) [@doesNotRaise]

let pad content n =
  let n = min n Sys.max_string_length in
  (String.make (n - (String.length content)) ' ') [@doesNotRaise] ^ content

let leading_space_count str =
  let rec _leading_space_count str str_length current_count =
    if current_count == str_length || str.[current_count] [@doesNotRaise] <> ' ' then
      current_count
    else
      _leading_space_count str str_length (current_count + 1)
  in
  _leading_space_count str (String.length str) 0

type current_printed_line_status =
  | Is_error_start_line
  | Is_error_end_line
  | Strictly_between_start_and_end
  | Only_error_line
  | Not_error_line

(* Range coordinates all 1-indexed, like for editors. Otherwise this code
  would have way too many off-by-one errors *)
let print_file
(* start_line_start_char inclusive, end_line_end_char exclusive *)
~range:((start_line, start_line_start_char), (end_line, end_line_end_char))
~lines
ppf
() =
  (* show 2 lines before & after the erroring lines. if there are too many lines, trim the middle *)
  let first_shown_line = max 1 (start_line - 2) in
  let last_shown_line = min (Array.length lines) (end_line + 2) in
  (* sometimes the code's very indented, and we'd end up displaying quite a
    few columns of leading whitespace; left-trim these. The general spirit is
    to center the erroring code. *)
  (* to achieve this, go through the shown lines and check the minimum number of leading whitespaces *)
  let columns_to_cut = ref 99999999 in
  (* first_shown_line <= last_shown_line aka this loop will always trigger at least once *)
  for i = first_shown_line to last_shown_line do
    let current_line = lines.(i - 1) [@doesNotRaise] in
    let current_line_leading_space_count = leading_space_count current_line in
    if String.length current_line == current_line_leading_space_count then
      (* disregard lines that are empty or are nothing but whitespace *)
      ()
    else if current_line_leading_space_count < !columns_to_cut then
      columns_to_cut := current_line_leading_space_count
  done;
  let columns_to_cut = !columns_to_cut in

  let print_char_maybe_highlight ~begin_highlight_line ~end_highlight_line ch =
    if begin_highlight_line then fprintf ppf "@{<error>";
    fprintf ppf "%c@," ch;
    if end_highlight_line then fprintf ppf "@}"
  in

  let print_separator ppf () =
    (* these are unicode chars. They're not of length 1. Careful; we need to
      explicitly tell Format to treat them as length 1 *)
    if columns_to_cut = 0 then fprintf ppf " @{<dim>@<1>│@} "
    else fprintf ppf " @{<dim>@<1>┆@} "
  in

  let max_line_number_number_of_digits = String.length (string_of_int last_shown_line) in

  fprintf ppf "@[<v 0>";
  (* inclusive *)
  for i = first_shown_line to last_shown_line do
    (* should some lines be ellipsed from the output? If we're showing more than 5 lines, then yes *)
    if end_line - start_line >= 5 && i >= start_line + 2 && i <= end_line - 2 then begin
      if i = start_line + 2 then
        (* Insert one line that's just a dimmed "..." *)
        let padded_line_number = pad "." max_line_number_number_of_digits in
        fprintf ppf "@{<dim>%s@}%a@{<dim>...@}@," padded_line_number print_separator ()
      end
    else
      let current_line = lines.(i - 1) [@doesNotRaise] in
      let padded_line_number = pad (string_of_int i) max_line_number_number_of_digits in

      fprintf ppf "@[<h 0>";

      fprintf ppf "@[<h 0>";

      if i < start_line || i > end_line then begin
        (* normal, non-highlighted line *)
        fprintf ppf "%s%a" padded_line_number print_separator ()
      end else begin
        (* highlighted *)
        fprintf ppf "@{<error>%s@}%a" padded_line_number print_separator ()
      end;

      fprintf ppf "@]"; (* h *)

      fprintf ppf "@[<hov 0>";

      let current_line_status =
        if i > start_line && i < end_line then Strictly_between_start_and_end
        else if i = start_line && i = end_line then Only_error_line
        else if i = start_line then Is_error_start_line
        else if i = end_line then Is_error_end_line
        else Not_error_line
      in
      let offset_current_line = current_line |> string_slice ~start:columns_to_cut in
      let offset_current_line_length = String.length offset_current_line in
      let offset_start_line_start_char = start_line_start_char - columns_to_cut in
      (* end_line_end_char is exclusive *)
      let offset_end_line_end_char = end_line_end_char - columns_to_cut in
      (* inclusive. To be consistent with using 1-indexed indices and count and i, j will be 1-indexed too *)
      for j = 1 to offset_current_line_length do
        let current_char = offset_current_line.[j - 1] [@doesNotRaise] in
        match current_line_status with
        | Strictly_between_start_and_end ->
          print_char_maybe_highlight
            ~begin_highlight_line:(j = 1)
            ~end_highlight_line:(j = offset_current_line_length)
            current_char
        | Only_error_line ->
          print_char_maybe_highlight
            ~begin_highlight_line:(j = offset_start_line_start_char)
            ~end_highlight_line:(j = offset_end_line_end_char)
            current_char
        | Is_error_start_line ->
          print_char_maybe_highlight
            ~begin_highlight_line:(j = offset_start_line_start_char)
            ~end_highlight_line:(j = offset_current_line_length)
            current_char
        | Is_error_end_line ->
          print_char_maybe_highlight
            ~begin_highlight_line:(j = 1)
            ~end_highlight_line:(j = offset_end_line_end_char)
            current_char
        | Not_error_line ->
          print_char_maybe_highlight
            ~begin_highlight_line:false
            ~end_highlight_line:false
            current_char
    done;

    fprintf ppf "@]"; (* hov *)

    fprintf ppf "@]@," (* h *)

  done;
  fprintf ppf "@]" (* v *)


end


(* ===== super_location.ml *)
module Super_location = struct

let fprintf = Format.fprintf

let setup_colors () =
  Misc.Color.setup !Clflags.color

let print_loc ~normalizedRange ppf (startPos: Lexing.position) =
  setup_colors ();
  let dim_loc ppf = function
    | None -> ()
    | Some ((start_line, start_line_start_char), (end_line, end_line_end_char)) ->
      if start_line = end_line then
        if start_line_start_char = end_line_end_char then
          fprintf ppf " @{<dim>%i:%i@}" start_line start_line_start_char
        else
          fprintf ppf " @{<dim>%i:%i-%i@}" start_line start_line_start_char end_line_end_char
      else
        fprintf ppf " @{<dim>%i:%i-%i:%i@}" start_line start_line_start_char end_line end_line_end_char
  in
  fprintf ppf "@{<filename>%a@}%a" Location.print_filename startPos.pos_fname dim_loc normalizedRange
;;

let print src startPos endPos ppf () =
  fprintf ppf "@[@{<error>Syntax error!@}@]@,";
  (* ocaml's reported line/col numbering is horrible and super error-prone
    when being handled programmatically (or humanly for that matter. If you're
    an ocaml contributor reading this: who the heck reads the character count
    starting from the first erroring character?) *)
  let (_file, start_line, start_char) = Location.get_pos_info startPos in
  let (_, end_line, end_char) = Location.get_pos_info endPos in
  (* line is 1-indexed, column is 0-indexed. We convert all of them to 1-indexed to avoid confusion *)
  (* start_char is inclusive, end_char is exclusive *)
  let normalizedRange =
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
  fprintf ppf "@[%a@]@," (print_loc ~normalizedRange) startPos;
  match normalizedRange with
  | None -> ()
  | Some range -> begin
    let lines = String.split_on_char '\n' src |> Array.of_list in
    (* we're putting the line break `@,` here rather than above, because this
       branch might not be reached (aka no inline file content display) so
       we don't wanna end up with two line breaks in the the consequent *)
    fprintf ppf "@,%a"
      (Super_misc.print_file ~lines ~range)
      ()
  end
;;

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/parsing/location.ml#L380 *)
(* This is the error report entry point. We'll replace the default reporter with this one. *)
let super_error_reporter ppf ~src ~startPos ~endPos ~msg =
  setup_colors ();
  (* open a vertical box. Everything in our message is indented 2 spaces *)
  Format.fprintf ppf "@[<v 2>@,%a@,%s@]@," (print src startPos endPos) () msg;

end
