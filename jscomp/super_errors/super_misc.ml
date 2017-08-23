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

(* ocaml's reported line/col numbering is horrible and super error-prone when
  being handled programmatically (or humanly for that matter. If you're an
  ocaml contributor reading this: who the heck reads the character count
  starting from the first erroring character?) *)
(* Range coordinates all 1-indexed, like for editors. Otherwise this code
  would have way too many off-by-one errors *)
let print_file 
~is_warning 
~range:((start_line, start_line_start_char), (end_line, end_line_end_char)) 
~lines
ppf 
() =
  (* show 2 lines before & after the erroring lines *)
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
  (* btw, these are unicode chars. They're not of length 1. Careful; we need to
    explicitly tell Format to treat them as length 1 below *)
  let separator = if columns_to_cut = 0 then "│" else "┆" in
  (* coloring *)
  let (highlighted_line_number, highlighted_content): (string -> string -> unit, Format.formatter, unit) format * (unit, Format.formatter, unit) format = 
    if is_warning then ("@{<info>%s@}@{<dim> @<1>%s @}", "@{<info>")
    else ("@{<error>%s@}@{<dim> @<1>%s @}", "@{<error>")
  in

  fprintf ppf "@[<v 0>";
  (* inclusive *)
  for i = first_shown_line to last_shown_line do
    let current_line = lines.(i - 1) in
    let current_line_cut = current_line |> string_slice ~start:columns_to_cut in
    let padded_line_number = pad (string_of_int i) max_line_number_number_of_digits in

    fprintf ppf "@[<h 0>";

    fprintf ppf "@[<h 0>";

    (* this is where you insrt the vertical separator. Mark them as legnth 1 as explained above *)
    if i < start_line || i > end_line then begin
      (* normal, non-highlighted line *)
      fprintf ppf "%s@{<dim> @<1>%s @}" padded_line_number separator
    end else begin
      (* highlighted  *)
      fprintf ppf highlighted_line_number padded_line_number separator
    end;

    fprintf ppf "@]"; (* h *)

    fprintf ppf "@[<hov 0>";

    let current_line_strictly_between_start_and_end_line = i > start_line && i < end_line in

    if current_line_strictly_between_start_and_end_line then fprintf ppf highlighted_content;

    let current_line_cut_length = String.length current_line_cut in
    (* inclusive. To be consistent with using 1-indexed indices and count and i, j will be 1-indexed too *)
    for j = 1 to current_line_cut_length do begin
      let current_char = current_line_cut.[j - 1] in
      if current_line_strictly_between_start_and_end_line then
        fprintf ppf "%c@," current_char
      else if i = start_line then begin
        if j == (start_line_start_char - columns_to_cut) then fprintf ppf highlighted_content;
        fprintf ppf "%c@," current_char;
        if j == current_line_cut_length then fprintf ppf "@}"
      end else if i = end_line then begin
        if j == 1 then fprintf ppf highlighted_content;
        fprintf ppf "%c@," current_char;
        if j == (end_line_end_char - columns_to_cut) then fprintf ppf "@}"
      end else
        (* normal, non-highlighted line *)
        fprintf ppf "%c@," current_char
    end
    done;

    if current_line_strictly_between_start_and_end_line then fprintf ppf "@}";

    fprintf ppf "@]"; (* hov *)

    fprintf ppf "@]@," (* h *)

  done;
  fprintf ppf "@]" (* v *)
