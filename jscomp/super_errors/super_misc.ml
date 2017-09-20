(* This file has nothing to do with misc.ml in ocaml's source. Just thought it'd be an appropriate parallel to call it so *)

let fprintf = Format.fprintf

let string_slice ~start str =
  let last = String.length str in
  if last <= start then "" else String.sub str start (last - start)

let sp = Printf.sprintf

let number_of_digits n =
  let digits = ref 1 in
  let nn = ref n in
  while ((!nn) / 10) > 0 do (nn := ((!nn) / 10); digits := ((!digits) + 1))
    done;
  !digits

let pad ?(ch=' ') content n =
  (String.make (n - (String.length content)) ch) ^ content

let leading_space_count str =
  let rec _leading_space_count str str_length current_index =
    if current_index == str_length then current_index
    else if (str.[current_index]) <> ' ' then current_index 
    else _leading_space_count str str_length (current_index + 1)
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
~is_warning 
(* start_line_start_char inclusive, end_line_end_char exclusive *)
~range:((start_line, start_line_start_char), (end_line, end_line_end_char)) 
~lines
ppf 
() =
  (* show 2 lines before & after the erroring lines. if there are too many lines, trim the middle *)
  let first_shown_line = max 1 (start_line - 2) in
  let last_shown_line = min (Array.length lines) (end_line + 2) in
  let max_line_number_number_of_digits = number_of_digits last_shown_line in
  (* sometimes the code's very indented, and we'd end up displaying quite a
    few columsn of leading whitespace; left-trim these. The general spirit is
    to center the erroring spot. In this case, almost literally *)
  (* to achieve this, go through the shown lines and check the minimum number of leading whitespaces *)
  let columns_to_cut = ref None in
  for i = first_shown_line to last_shown_line do
    let current_line = lines.(i - 1) in
    (* disregard lines that are empty or are nothing but whitespace *)
    if String.length (String.trim current_line) == 0 then ()
    else
      let current_line_leading_space_count = leading_space_count current_line in
      match !columns_to_cut with
      | None ->
        columns_to_cut := Some current_line_leading_space_count
      | Some n when n > current_line_leading_space_count ->
        columns_to_cut := Some current_line_leading_space_count
      | Some n -> ()
  done;
  let columns_to_cut = match !columns_to_cut with
  | None -> 0
  | Some n -> n
  in
  (* coloring *)
  let highlighted_line_number : _ format = if is_warning then "@{<info>%s@}%a" else "@{<error>%s@}%a" in

  let print_char_maybe_highlight ~begin_highlight_line ~end_highlight_line ch =
    let highlighted_open_tag: _ format = if is_warning then "@{<info>" else "@{<error>" in
    if begin_highlight_line then fprintf ppf highlighted_open_tag;
    fprintf ppf "%c@," ch;
    if end_highlight_line then fprintf ppf "@}"
  in

  let print_separator ppf () = 
    (* these are unicode chars. They're not of length 1. Careful; we need to
      explicitly tell Format to treat them as length 1 *)
    if columns_to_cut = 0 then fprintf ppf " @{<dim>@<1>│@} "
    else fprintf ppf " @{<dim>@<1>┆@} "
  in

  fprintf ppf "@[<v 0>";
  (* inclusive *)
  for i = first_shown_line to last_shown_line do
    (* should some lines be ellipsed from the output? If we're showing more than 5 lines, then yes *)
    if last_shown_line - first_shown_line >= 5 && i >= start_line + 2 && i <= end_line - 2 then begin
      if i = start_line + 2 then
        (* Insert one line that's just a dimmed "..." *)
        let padded_line_number = pad "." max_line_number_number_of_digits in
        fprintf ppf "@{<dim>%s@}%a@{<dim>...@}@," padded_line_number print_separator ()
      end
    else
      let current_line = lines.(i - 1) in
      let padded_line_number = pad (string_of_int i) max_line_number_number_of_digits in

      fprintf ppf "@[<h 0>";

      fprintf ppf "@[<h 0>";

      if i < start_line || i > end_line then begin
        (* normal, non-highlighted line *)
        fprintf ppf "%s%a" padded_line_number print_separator ()
      end else begin
        (* highlighted *)
        fprintf ppf highlighted_line_number padded_line_number print_separator ()
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
        let current_char = offset_current_line.[j - 1] in
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
