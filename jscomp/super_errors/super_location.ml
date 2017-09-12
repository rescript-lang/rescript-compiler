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
  Format.fprintf ppf "%s" (Location.show_filename file)

let print_loc ppf loc =
  setup_colors ();
  let (file, _, _) = Location.get_pos_info loc.loc_start in
  if file = "//toplevel//" then begin
    if highlight_locations ppf [loc] then () else
      fprintf ppf "Characters %i-%i"
              loc.loc_start.pos_cnum loc.loc_end.pos_cnum
  end else begin
    fprintf ppf "@{<filename>%a@}" print_filename file;
  end
;;

let print ~is_warning intro ppf loc =
  setup_colors ();
  (* TODO: handle locations such as _none_ and "" *)
  if loc.loc_start.pos_fname = "//toplevel//"
  && highlight_locations ppf [loc] then ()
  else
    if is_warning then
      fprintf ppf "@[@{<info>%s@}@]@," intro
    else begin
      fprintf ppf "@[@{<error>%s@}@]@," intro
    end;
    fprintf ppf "@[%a@]@," print_loc loc;
    let (file, start_line, start_char) = Location.get_pos_info loc.loc_start in
    let (_, end_line, end_char) = Location.get_pos_info loc.loc_end in
    (* things to special-case: startchar & endchar2 both -1  *)
    if start_char == -1 || end_char == -1 then
      (* happens sometimes. Syntax error for example. Just show the file and do nothing for now *)
      ()
    else begin
      try
        let lines = file_lines file in
        (* we're putting a line break here rather than above, because this
           branch might not be reached (aka no inline file content display) so 
           we don't wanna end up with two line breaks in the the consequent *)
        fprintf ppf "@,%a"
          (Super_misc.print_file
          ~is_warning
          ~lines
          ~range:(
            (* line is 1-indexed, column is 0-indexed. We convert all of them to 1-indexed to avoid confusion *)
            (* start_char is inclusive *)
            (start_line, start_char + 1),
            (* start_char is exclusive *)
            (end_line, end_char + 1)
          ))
          ()
      with
      (* this shouldn't happen, but gracefully fall back to the default reporter just in case *)
      | Sys_error _ -> Location.print ppf loc
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
    (* open a vertical box. Everything in our message is indented 2 spaces *)
    Format.fprintf ppf "@[<v 2>@,%a@,%s@,@]" (print ~is_warning:false "We've found a bug for you!") loc msg;
    List.iter (Format.fprintf ppf "@,@[%a@]" super_error_reporter) sub;
    (* no need to flush here; location's report_exception (which uses this ultimately) flushes *)
  end

(* extracted from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/parsing/location.ml#L299 *)
(* This is the warning report entry point. We'll replace the default printer with this one *)
let super_warning_printer loc ppf w =
  if Warnings.is_active w then begin
    setup_colors ();
    (* open a vertical box. Everything in our message is indented 2 spaces *)
    Format.fprintf ppf "@[<v 2>@,%a@,%a@,@]"
      (print ~is_warning:true ("Warning number " ^ (Super_warnings.number w |> string_of_int)))
      loc
      Super_warnings.print
      w
  end
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
