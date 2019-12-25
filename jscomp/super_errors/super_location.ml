open Misc
open Asttypes
open Parsetree
open Types
open Typedtree
open Btype
open Ctype

open Format
open Printtyp

open Location

let file_lines filePath =
  (* open_in_bin works on windows, as opposed to open_in, afaik? *)
  let chan = open_in_bin filePath in
  let lines = ref [] in
  try
    while true do
      lines := (input_line chan) :: !lines
     done;
     (* leave this here to make things type. The loop will definitly raise *)
     [||]
  with
  | End_of_file -> begin
      close_in chan;
      List.rev (!lines) |> Array.of_list
    end

let setup_colors () =
  Misc.Color.setup !Clflags.color

let print_filename ppf file =
  match file with
  (* modified *)
  | "_none_"
  | "" -> Format.fprintf ppf "(No file name)"
  | real_file -> Format.fprintf ppf "%s" (Location.show_filename real_file)

let print_loc ~normalizedRange ppf loc =
  setup_colors ();
  let (file, _, _) = Location.get_pos_info loc.loc_start in
  if file = "//toplevel//" then begin
    if highlight_locations ppf [loc] then () else
      fprintf ppf "Characters %i-%i"
              loc.loc_start.pos_cnum loc.loc_end.pos_cnum
  end else
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
    fprintf ppf "@{<filename>%a@}%a" print_filename file dim_loc normalizedRange
;;

let print ~message_kind intro ppf loc =
  if loc.loc_start.pos_fname = "//toplevel//"
  && highlight_locations ppf [loc] then ()
  else
    begin match message_kind with
    | `warning -> fprintf ppf "@[@{<info>%s@}@]@," intro
    | `warning_as_error -> fprintf ppf "@[@{<error>%s@} (configured as error) @]@," intro
    | `error -> fprintf ppf "@[@{<error>%s@}@]@," intro
    end;
    (* ocaml's reported line/col numbering is horrible and super error-prone
      when being handled programmatically (or humanly for that matter. If you're
      an ocaml contributor reading this: who the heck reads the character count
      starting from the first erroring character?) *)
    let (file, start_line, start_char) = Location.get_pos_info loc.loc_start in
    let (_, end_line, end_char) = Location.get_pos_info loc.loc_end in
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
    fprintf ppf "@[%a@]@," (print_loc ~normalizedRange) loc;
    match normalizedRange with
    | None -> ()
    | Some range -> begin
      try
        let lines = file_lines file in
        (* we're putting the line break `@,` here rather than above, because this
           branch might not be reached (aka no inline file content display) so
           we don't wanna end up with two line breaks in the the consequent *)
        fprintf ppf "@,%a"
          (Super_misc.print_file ~is_warning:(message_kind=`warning) ~lines ~range)
          ()
      with
      (* this might happen if the file is e.g. "", "_none_" or any of the fake file name placeholders.
        we've already printed the location above, so nothing more to do here. *)
      | Sys_error _ -> ()
    end
;;

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/parsing/location.ml#L380 *)
(* This is the error report entry point. We'll replace the default reporter with this one. *)
let rec super_error_reporter ppf ({Location.loc; msg; sub; if_highlight} as err) =
  let highlighted =
    if if_highlight <> "" then
      let rec collect_locs locs {Location.loc; sub; if_highlight; _} =
        List.fold_left collect_locs (loc :: locs) sub
      in
      let locs = collect_locs [] err in
      Location.highlight_locations ppf locs
    else
      false
  in
  if highlighted then
    Format.pp_print_string ppf if_highlight
  else begin
    setup_colors ();
    (* open a vertical box. Everything in our message is indented 2 spaces *)
    Format.fprintf ppf "@[<v 2>@,%a@,%s@,@]" (print ~message_kind:`error "We've found a bug for you!") loc msg;
    List.iter (Format.fprintf ppf "@,@[%a@]" super_error_reporter) sub;
    (* no need to flush here; location's report_exception (which uses this ultimately) flushes *)
  end

(* extracted from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/parsing/location.ml#L299 *)
(* This is the warning report entry point. We'll replace the default printer with this one *)
let super_warning_printer loc ppf w =
  match Warnings.report w with
  | `Inactive -> ()
  | `Active { Warnings. number; message; is_error; sub_locs } ->
    setup_colors ();
    let message_kind = if is_error then `warning_as_error else `warning in
    Format.fprintf ppf "@[<v 2>@,%a@,%s@,@]"
      (print ~message_kind ("Warning number " ^ (Warnings.number w |> string_of_int)))
      loc
      (Super_warnings.message w);
    (* at this point, you can display sub_locs too, from e.g. https://github.com/ocaml/ocaml/commit/f6d53cc38f87c67fbf49109f5fb79a0334bab17a
      but we won't bother for now *)
;;

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/parsing/location.ml#L354 *)
let print_phanton_error_prefix ppf =
  (* modified from the original. We use only 2 indentations for error report
    (see super_error_reporter above) *)
  Format.pp_print_as ppf 2 ""

let errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") fmt =
  Location.pp_ksprintf
    ~before:print_phanton_error_prefix
    (fun msg -> {loc; msg; sub; if_highlight})
    fmt

let error_of_printer loc print x =
  errorf ~loc "%a@?" print x

let error_of_printer_file print x =
  error_of_printer (in_file !input_name) print x

(* This will be called in super_main. This is how you override the default error and warning printers *)
let setup () =
  Location.error_reporter := super_error_reporter;
  Location.warning_printer := super_warning_printer;
