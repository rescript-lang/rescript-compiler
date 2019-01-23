module Super_misc : sig 
#1 "super_misc.mli"
(** Range coordinates all 1-indexed, like for editors. Otherwise this code
  would have way too many off-by-one errors *)
val print_file: is_warning:bool -> range:(int * int) * (int * int) -> lines:string array -> Format.formatter -> unit -> unit

end = struct
#1 "super_misc.ml"
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
    if end_line - start_line >= 5 && i >= start_line + 2 && i <= end_line - 2 then begin
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

end
module Super_warnings
= struct
#1 "super_warnings.ml"
let fprintf = Format.fprintf
(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/utils/warnings.ml#L251 *)
(* actual modified message branches are commented *)
let message (warning : Warnings.t)  =
  match warning with
  | Partial_match "" ->
      "You forgot to handle a possible value here, though we don't have more information on the value."
  | Partial_match s ->
      "You forgot to handle a possible value here, for example: \n" ^ s
  | Unerasable_optional_argument ->
      String.concat ""
        ["This optional parameter in final position will, in practice, not be optional.\n";
        "  Reorder the parameters so that at least one non-optional one is in final position or, if all parameters are optional, insert a final ().\n\n";
        "  Explanation: If the final parameter is optional, it'd be unclear whether a function application that omits it should be considered fully applied, or partially applied. Imagine writing `let title = display(\"hello!\")`, only to realize `title` isn't your desired result, but a curried call that takes a final optional argument, e.g. `~showDate`.\n\n";
        "  Formal rule: an optional argument is considered intentionally omitted when the 1st positional (i.e. neither labeled nor optional) argument defined after it is passed in."
        ]
  | Bad_module_name (modname) ->
      "This file's name is potentially invalid. The build systems conventionally turn a file name into a module name by upper-casing the first letter. " ^ modname ^ " isn't a valid module name.\n" ^
      "Note: some build systems might e.g. turn kebab-case into CamelCase module, which is why this isn't a hard error."
  | Statement_type -> "This expression returns a value, but you're not doing anything with it. If this is on purpose, put `|> ignore` at the end."
  | Useless_record_with ->
      "All the fields are already explicitly listed in this record. You can remove the `...` spread."
  | _ -> Warnings.message warning
;;

end
module Super_location
= struct
#1 "super_location.ml"
(* open Misc
open Asttypes
open Parsetree
open Types
open Typedtree
open Btype
open Ctype *)

open Format
(* open Printtyp *)

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

let print ~is_warning intro ppf loc =
  setup_colors ();
  if loc.loc_start.pos_fname = "//toplevel//"
  && highlight_locations ppf [loc] then ()
  else
    if is_warning then
      fprintf ppf "@[@{<info>%s@}@]@," intro
    else begin
      fprintf ppf "@[@{<error>%s@}@]@," intro
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
          (Super_misc.print_file ~is_warning ~lines ~range)
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
      (print ~is_warning:true ("Warning number " ^ (Warnings.number w |> string_of_int)))
      loc
      (Warnings.super_print Super_warnings.message)
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

end
module Super_env
= struct
#1 "super_env.ml"
let fprintf = Format.fprintf

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/env.ml#L1842 *)
(* modified branches are commented *)
let report_error ppf = function
  | Env.Illegal_renaming(name, modname, filename) -> 
      (* modified *)
      fprintf ppf
        "@[You referred to the module %s, but we've found one called %s instead.@ \
          Is the name's casing right?@]"
        name modname
  | Inconsistent_import(name, source1, source2) -> 
      (* modified *)
     fprintf ppf "@[<v>\
        @[@{<info>It's possible that your build is stale.@}@ Try to clean the artifacts and build again?@]@,@,\
        @[@{<info>Here's the original error message@}@]@,\
      @]";
      fprintf ppf
        "@[<hov>The files %a@ and %a@ \
              make inconsistent assumptions@ over interface %s@]"
      Location.print_filename source1 Location.print_filename source2 name
  | Need_recursive_types(import, export) ->
      fprintf ppf
        "@[<hov>Unit %s imports from %s, which uses recursive types.@ %s@]"
        export import "The compilation flag -rectypes is required"
  | Missing_module(_, path1, path2) ->
      fprintf ppf "@[@[<hov>";
      if Path.same path1 path2 then
        fprintf ppf "Internal path@ %s@ is dangling." (Path.name path1)
      else
        fprintf ppf "Internal path@ %s@ expands to@ %s@ which is dangling."
          (Path.name path1) (Path.name path2);
      fprintf ppf "@]@ @[%s@ %s@ %s.@]@]"
        "The compiled interface for module" (Ident.name (Path.head path2))
        "was not found"
  | Illegal_value_name(_loc, name) ->
      fprintf ppf "'%s' is not a valid value identifier."
        name

(* This will be called in super_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn
    (function
      | Env.Error (Missing_module (loc, _, _)
              | Illegal_value_name (loc, _)
               as err) when loc <> Location.none ->
          Some (Super_location.error_of_printer loc report_error err)
      | Env.Error err -> Some (Super_location.error_of_printer_file report_error err)
      | _ -> None
    )

end
module Ext_bytes : sig 
#1 "ext_bytes.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)







(** Port the {!Bytes.escaped} from trunk to make it not locale sensitive *)

val escaped : bytes -> bytes



end = struct
#1 "ext_bytes.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let escaped s =
  let n = Pervasives.ref 0 in
  for i = 0 to Bytes.length s - 1 do
    n := !n +
      (match Bytes.unsafe_get s i with
       | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
       | ' ' .. '~' -> 1
       | _ -> 4)
  done;
  if !n = Bytes.length s then Bytes.copy s else begin
    let s' = Bytes.create !n in
    n := 0;
    for i = 0 to Bytes.length s - 1 do
      begin match Bytes.unsafe_get s i with
      | ('"' | '\\') as c ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n c
      | '\n' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'n'
      | '\t' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 't'
      | '\r' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'r'
      | '\b' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'b'
      | (' ' .. '~') as c -> Bytes.unsafe_set s' !n c
      | c ->
          let a = char_code c in
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + a / 100));
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + (a / 10) mod 10));
          incr n;
          Bytes.unsafe_set s' !n (char_chr (48 + a mod 10));
      end;
      incr n
    done;
    s'
  end



end
module Ext_string : sig 
#1 "ext_string.mli"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)








(** Extension to the standard library [String] module, fixed some bugs like
    avoiding locale sensitivity *) 

(** default is false *)    
val split_by : ?keep_empty:bool -> (char -> bool) -> string -> string list


(** remove whitespace letters ('\t', '\n', ' ') on both side*)
val trim : string -> string 


(** default is false *)
val split : ?keep_empty:bool -> string -> char -> string list

(** split by space chars for quick scripting *)
val quick_split_by_ws : string -> string list 



val starts_with : string -> string -> bool

(**
   return [-1] when not found, the returned index is useful 
   see [ends_with_then_chop]
*)
val ends_with_index : string -> string -> int

val ends_with : string -> string -> bool

(**
  [ends_with_then_chop name ext]
  @example:
   {[
     ends_with_then_chop "a.cmj" ".cmj"
     "a"
   ]}
   This is useful in controlled or file case sensitve system
*)
val ends_with_then_chop : string -> string -> string option


val escaped : string -> string

(**
  [for_all_from  s start p]
  if [start] is negative, it raises,
  if [start] is too large, it returns true
*)
val for_all_from:
  string -> 
  int -> 
  (char -> bool) -> 
  bool 

val for_all : (char -> bool) -> string -> bool

val is_empty : string -> bool

val repeat : int -> string -> string 

val equal : string -> string -> bool

(**
  [find ~start ~sub s]
  returns [-1] if not found
*)
val find : ?start:int -> sub:string -> string -> int

val contain_substring : string -> string -> bool 

val non_overlap_count : sub:string -> string -> int 

val rfind : sub:string -> string -> int

(** [tail_from s 1]
  return a substring from offset 1 (inclusive)
*)
val tail_from : string -> int -> string


(** returns negative number if not found *)
val rindex_neg : string -> char -> int 

val rindex_opt : string -> char -> int option

type check_result = 
    | Good | Invalid_module_name | Suffix_mismatch

val is_valid_source_name :
   string -> check_result





val no_char : string -> char -> int -> int -> bool 


val no_slash : string -> bool 

(** return negative means no slash, otherwise [i] means the place for first slash *)
val no_slash_idx : string -> int 

(** if no conversion happens, reference equality holds *)
val replace_slash_backward : string -> string 

(** if no conversion happens, reference equality holds *)
val replace_backward_slash : string -> string 

val empty : string 


(* external compare : string -> string -> int = "caml_string_length_based_compare" "noalloc";; *)
  
val single_space : string

val concat3 : string -> string -> string -> string 
val concat4 : string -> string -> string -> string -> string 
val concat5 : string -> string -> string -> string -> string -> string  
val inter2 : string -> string -> string
val inter3 : string -> string -> string -> string 
val inter4 : string -> string -> string -> string -> string
val concat_array : string -> string array -> string 

val single_colon : string 

val parent_dir_lit : string
val current_dir_lit : string

val capitalize_ascii : string -> string

val uncapitalize_ascii : string -> string


end = struct
#1 "ext_string.ml"
(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)







(*
   {[ split " test_unsafe_obj_ffi_ppx.cmi" ~keep_empty:false ' ']}
*)
let split_by ?(keep_empty=false) is_delim str =
  let len = String.length str in
  let rec loop acc last_pos pos =
    if pos = -1 then
      if last_pos = 0 && not keep_empty then

        acc
      else 
        String.sub str 0 last_pos :: acc
    else
    if is_delim str.[pos] then
      let new_len = (last_pos - pos - 1) in
      if new_len <> 0 || keep_empty then 
        let v = String.sub str (pos + 1) new_len in
        loop ( v :: acc)
          pos (pos - 1)
      else loop acc pos (pos - 1)
    else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let trim s = 
  let i = ref 0  in
  let j = String.length s in 
  while !i < j &&  
        let u = String.unsafe_get s !i in 
        u = '\t' || u = '\n' || u = ' ' 
  do 
    incr i;
  done;
  let k = ref (j - 1)  in 
  while !k >= !i && 
        let u = String.unsafe_get s !k in 
        u = '\t' || u = '\n' || u = ' ' do 
    decr k ;
  done;
  String.sub s !i (!k - !i + 1)

let split ?keep_empty  str on = 
  if str = "" then [] else 
    split_by ?keep_empty (fun x -> (x : char) = on) str  ;;

let quick_split_by_ws str : string list = 
  split_by ~keep_empty:false (fun x -> x = '\t' || x = '\n' || x = ' ') str

let starts_with s beg = 
  let beg_len = String.length beg in
  let s_len = String.length s in
  beg_len <=  s_len &&
  (let i = ref 0 in
   while !i <  beg_len 
         && String.unsafe_get s !i =
            String.unsafe_get beg !i do 
     incr i 
   done;
   !i = beg_len
  )

let rec ends_aux s end_ j k = 
  if k < 0 then (j + 1)
  else if String.unsafe_get s j = String.unsafe_get end_ k then 
    ends_aux s end_ (j - 1) (k - 1)
  else  -1   

(** return an index which is minus when [s] does not 
    end with [beg]
*)
let ends_with_index s end_ = 
  let s_finish = String.length s - 1 in
  let s_beg = String.length end_ - 1 in
  if s_beg > s_finish then -1
  else
    ends_aux s end_ s_finish s_beg

let ends_with s end_ = ends_with_index s end_ >= 0 

let ends_with_then_chop s beg = 
  let i =  ends_with_index s beg in 
  if i >= 0 then Some (String.sub s 0 i) 
  else None

let check_suffix_case = ends_with 
let check_suffix_case_then_chop = ends_with_then_chop

let check_any_suffix_case s suffixes = 
  List.exists (fun x -> check_suffix_case s x) suffixes

let check_any_suffix_case_then_chop s suffixes = 
  let rec aux suffixes = 
    match suffixes with 
    | [] -> None 
    | x::xs -> 
      let id = ends_with_index s x in 
      if id >= 0 then Some (String.sub s 0 id)
      else aux xs in 
  aux suffixes    



(**  In OCaml 4.02.3, {!String.escaped} is locale senstive, 
     this version try to make it not locale senstive, this bug is fixed
     in the compiler trunk     
*)
let escaped s =
  let rec needs_escape i =
    if i >= String.length s then false else
      match String.unsafe_get s i with
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> true
      | ' ' .. '~' -> needs_escape (i+1)
      | _ -> true
  in
  if needs_escape 0 then
    Bytes.unsafe_to_string (Ext_bytes.escaped (Bytes.unsafe_of_string s))
  else
    s

(* it is unsafe to expose such API as unsafe since 
   user can provide bad input range 

*)
let rec unsafe_for_all_range s ~start ~finish p =     
  start > finish ||
  p (String.unsafe_get s start) && 
  unsafe_for_all_range s ~start:(start + 1) ~finish p

let for_all_from s start  p = 
  let len = String.length s in 
  if start < 0  then invalid_arg "Ext_string.for_all_from"
  else unsafe_for_all_range s ~start ~finish:(len - 1) p 


let for_all (p : char -> bool) s =   
  unsafe_for_all_range s ~start:0  ~finish:(String.length s - 1) p 

let is_empty s = String.length s = 0


let repeat n s  =
  let len = String.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    String.blit s 0 res (i * len) len
  done;
  Bytes.to_string res

let equal (x : string) y  = x = y



let unsafe_is_sub ~sub i s j ~len =
  let rec check k =
    if k = len
    then true
    else 
      String.unsafe_get sub (i+k) = 
      String.unsafe_get s (j+k) && check (k+1)
  in
  j+len <= String.length s && check 0


exception Local_exit 
let find ?(start=0) ~sub s =
  let n = String.length sub in
  let s_len = String.length s in 
  let i = ref start in  
  try
    while !i + n <= s_len do
      if unsafe_is_sub ~sub 0 s !i ~len:n then
        raise_notrace Local_exit;
      incr i
    done;
    -1
  with Local_exit ->
    !i

let contain_substring s sub = 
  find s ~sub >= 0 

(** TODO: optimize 
    avoid nonterminating when string is empty 
*)
let non_overlap_count ~sub s = 
  let sub_len = String.length sub in 
  let rec aux  acc off = 
    let i = find ~start:off ~sub s  in 
    if i < 0 then acc 
    else aux (acc + 1) (i + sub_len) in
  if String.length sub = 0 then invalid_arg "Ext_string.non_overlap_count"
  else aux 0 0  


let rfind ~sub s =
  let n = String.length sub in
  let i = ref (String.length s - n) in
  (* let module M = struct exception Exit end in  *)
  try
    while !i >= 0 do
      if unsafe_is_sub ~sub 0 s !i ~len:n then 
        raise_notrace Local_exit;
      decr i
    done;
    -1
  with Local_exit ->
    !i

let tail_from s x = 
  let len = String.length s  in 
  if  x > len then invalid_arg ("Ext_string.tail_from " ^s ^ " : "^ string_of_int x )
  else String.sub s x (len - x)

let equal (x : string) y  = x = y

let rec rindex_rec s i c =
  if i < 0 then i else
  if String.unsafe_get s i = c then i else rindex_rec s (i - 1) c;;

let rec rindex_rec_opt s i c =
  if i < 0 then None else
  if String.unsafe_get s i = c then Some i else rindex_rec_opt s (i - 1) c;;

let rindex_neg s c = 
  rindex_rec s (String.length s - 1) c;;

let rindex_opt s c = 
  rindex_rec_opt s (String.length s - 1) c;;

let is_valid_module_file (s : string) = 
  let len = String.length s in 
  len > 0 &&
  match String.unsafe_get s 0 with 
  | 'A' .. 'Z'
  | 'a' .. 'z' -> 
    unsafe_for_all_range s ~start:1 ~finish:(len - 1)
      (fun x -> 
         match x with 
         | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> true
         | _ -> false )
  | _ -> false 




type check_result = 
  | Good 
  | Invalid_module_name 
  | Suffix_mismatch
  (** 
     TODO: move to another module 
     Make {!Ext_filename} not stateful
  *)
let is_valid_source_name name : check_result =
  match check_any_suffix_case_then_chop name [
      ".ml"; 
      ".re";
      ".mli"; 
      ".rei"
    ] with 
  | None -> Suffix_mismatch
  | Some x -> 
    if is_valid_module_file  x then
      Good
    else Invalid_module_name  

(** TODO: can be improved to return a positive integer instead *)
let rec unsafe_no_char x ch i  last_idx = 
  i > last_idx  || 
  (String.unsafe_get x i <> ch && unsafe_no_char x ch (i + 1)  last_idx)

let rec unsafe_no_char_idx x ch i last_idx = 
  if i > last_idx  then -1 
  else 
  if String.unsafe_get x i <> ch then 
    unsafe_no_char_idx x ch (i + 1)  last_idx
  else i

let no_char x ch i len  : bool =
  let str_len = String.length x in 
  if i < 0 || i >= str_len || len >= str_len then invalid_arg "Ext_string.no_char"   
  else unsafe_no_char x ch i len 


let no_slash x = 
  unsafe_no_char x '/' 0 (String.length x - 1)

let no_slash_idx x = 
  unsafe_no_char_idx x '/' 0 (String.length x - 1)

let replace_slash_backward (x : string ) = 
  let len = String.length x in 
  if unsafe_no_char x '/' 0  (len - 1) then x 
  else 
    String.map (function 
        | '/' -> '\\'
        | x -> x ) x 

let replace_backward_slash (x : string)=
  let len = String.length x in
  if unsafe_no_char x '\\' 0  (len -1) then x 
  else  
    String.map (function 
        |'\\'-> '/'
        | x -> x) x

let empty = ""

    
(* external compare : string -> string -> int = "caml_string_length_based_compare" "noalloc";; *)

let single_space = " "
let single_colon = ":"

let concat_array sep (s : string array) =   
  let s_len = Array.length s in 
  match s_len with 
  | 0 -> empty 
  | 1 -> Array.unsafe_get s 0
  | _ ->     
    let sep_len = String.length sep in 
    let len = ref 0 in 
    for i = 0 to  s_len - 1 do 
      len := !len + String.length (Array.unsafe_get s i)
    done;
    let target = 
      Bytes.create 
        (!len + (s_len - 1) * sep_len ) in    
    let hd = (Array.unsafe_get s 0) in     
    let hd_len = String.length hd in 
    String.unsafe_blit hd  0  target 0 hd_len;   
    let current_offset = ref hd_len in     
    for i = 1 to s_len - 1 do 
      String.unsafe_blit sep 0 target  !current_offset sep_len;
      let cur = Array.unsafe_get s i in 
      let cur_len = String.length cur in     
      let new_off_set = (!current_offset + sep_len ) in
      String.unsafe_blit cur 0 target new_off_set cur_len; 
      current_offset := 
        new_off_set + cur_len ; 
    done;
    Bytes.unsafe_to_string target   

let concat3 a b c = 
  let a_len = String.length a in 
  let b_len = String.length b in 
  let c_len = String.length c in 
  let len = a_len + b_len + c_len in 
  let target = Bytes.create len in 
  String.unsafe_blit a 0 target 0 a_len ; 
  String.unsafe_blit b 0 target a_len b_len;
  String.unsafe_blit c 0 target (a_len + b_len) c_len;
  Bytes.unsafe_to_string target

let concat4 a b c d =
  let a_len = String.length a in 
  let b_len = String.length b in 
  let c_len = String.length c in 
  let d_len = String.length d in 
  let len = a_len + b_len + c_len + d_len in 

  let target = Bytes.create len in 
  String.unsafe_blit a 0 target 0 a_len ; 
  String.unsafe_blit b 0 target a_len b_len;
  String.unsafe_blit c 0 target (a_len + b_len) c_len;
  String.unsafe_blit d 0 target (a_len + b_len + c_len) d_len;
  Bytes.unsafe_to_string target


let concat5 a b c d e =
  let a_len = String.length a in 
  let b_len = String.length b in 
  let c_len = String.length c in 
  let d_len = String.length d in 
  let e_len = String.length e in 
  let len = a_len + b_len + c_len + d_len + e_len in 

  let target = Bytes.create len in 
  String.unsafe_blit a 0 target 0 a_len ; 
  String.unsafe_blit b 0 target a_len b_len;
  String.unsafe_blit c 0 target (a_len + b_len) c_len;
  String.unsafe_blit d 0 target (a_len + b_len + c_len) d_len;
  String.unsafe_blit e 0 target (a_len + b_len + c_len + d_len) e_len;
  Bytes.unsafe_to_string target



let inter2 a b = 
  concat3 a single_space b 


let inter3 a b c = 
  concat5 a  single_space  b  single_space  c 





let inter4 a b c d =
  concat_array single_space [| a; b ; c; d|]


let parent_dir_lit = ".."    
let current_dir_lit = "."


(* reference {!Bytes.unppercase} *)
let capitalize_ascii (s : string) : string = 
  if String.length s = 0 then s 
  else 
    begin
      let c = String.unsafe_get s 0 in 
      if (c >= 'a' && c <= 'z')
      || (c >= '\224' && c <= '\246')
      || (c >= '\248' && c <= '\254') then 
        let uc = Char.unsafe_chr (Char.code c - 32) in 
        let bytes = Bytes.of_string s in
        Bytes.unsafe_set bytes 0 uc;
        Bytes.unsafe_to_string bytes 
      else s 
    end

let uncapitalize_ascii =

    String.uncapitalize
      







end
module Pparse : sig 
#1 "pparse.mli"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format

type error =
  | CannotRun of string
  | WrongMagic of string

exception Error of error

val preprocess : string -> string
val remove_preprocessed : string -> unit
val file : formatter -> tool_name:string -> string -> (Lexing.lexbuf -> 'a) -> string -> 'a
val apply_rewriters: ?restore:bool -> tool_name:string -> string -> 'a -> 'a
  (** If [restore = true] (the default), cookies set by external rewriters will be
      kept for later calls. *)

val apply_rewriters_str: ?restore:bool -> tool_name:string -> Parsetree.structure -> Parsetree.structure
val apply_rewriters_sig: ?restore:bool -> tool_name:string -> Parsetree.signature -> Parsetree.signature


val report_error : formatter -> error -> unit


val parse_implementation: formatter -> tool_name:string -> string -> Parsetree.structure
val parse_interface: formatter -> tool_name:string -> string -> Parsetree.signature

(* [call_external_preprocessor sourcefile pp] *)
val call_external_preprocessor : string -> string -> string
val open_and_check_magic : string -> string -> in_channel * bool
val read_ast : string -> string -> 'a

end = struct
#1 "pparse.ml"
(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format

type error =
  | CannotRun of string
  | WrongMagic of string

exception Error of error

(* Optionally preprocess a source file *)

let call_external_preprocessor sourcefile pp =
      let tmpfile = Filename.temp_file "ocamlpp" "" in
      let comm = Printf.sprintf "%s %s > %s"
                                pp (Filename.quote sourcefile) tmpfile
      in
      if Ccomp.command comm <> 0 then begin
        Misc.remove_file tmpfile;
        raise (Error (CannotRun comm));
      end;
      tmpfile

let preprocess sourcefile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp -> call_external_preprocessor sourcefile pp


let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> Misc.remove_file inputfile


(* Note: some of the functions here should go to Ast_mapper instead,
   which would encapsulate the "binary AST" protocol. *)

let write_ast magic ast =
  let fn = Filename.temp_file "camlppx" "" in
  let oc = open_out_bin fn in
  output_string oc magic;
  output_value oc !Location.input_name;
  output_value oc ast;
  close_out oc;
  fn

let apply_rewriter magic fn_in ppx =
  let fn_out = Filename.temp_file "camlppx" "" in
  let comm =
    Printf.sprintf "%s %s %s" ppx (Filename.quote fn_in) (Filename.quote fn_out)
  in
  let ok = Ccomp.command comm = 0 in
  Misc.remove_file fn_in;
  if not ok then begin
    Misc.remove_file fn_out;
    raise (Error (CannotRun comm));
  end;
  if not (Sys.file_exists fn_out) then
    raise (Error (WrongMagic comm));
  (* check magic before passing to the next ppx *)
  let ic = open_in_bin fn_out in
  let buffer =
    try really_input_string ic (String.length magic) with End_of_file -> "" in
  close_in ic;
  if buffer <> magic then begin
    Misc.remove_file fn_out;
    raise (Error (WrongMagic comm));
  end;
  fn_out

let read_ast magic fn =
  let ic = open_in_bin fn in
  try
    let buffer = really_input_string ic (String.length magic) in
    assert(buffer = magic); (* already checked by apply_rewriter *)
    Location.input_name := input_value ic;
    let ast = input_value ic in
    close_in ic;
    Misc.remove_file fn;
    ast
  with exn ->
    close_in ic;
    Misc.remove_file fn;
    raise exn

let rewrite magic ast ppxs =
  read_ast magic
    (List.fold_left (apply_rewriter magic) (write_ast magic ast)
       (List.rev ppxs))

let apply_rewriters_str ?(restore = true) ~tool_name ast =
  match !Clflags.all_ppx with
  | [] -> ast
  | ppxs ->
      let ast = Ast_mapper.add_ppx_context_str ~tool_name ast in
      let ast = rewrite Config.ast_impl_magic_number ast ppxs in
      Ast_mapper.drop_ppx_context_str ~restore ast

let apply_rewriters_sig ?(restore = true) ~tool_name ast =
  match !Clflags.all_ppx with
  | [] -> ast
  | ppxs ->
      let ast = Ast_mapper.add_ppx_context_sig ~tool_name ast in
      let ast = rewrite Config.ast_intf_magic_number ast ppxs in
      Ast_mapper.drop_ppx_context_sig ~restore ast

let apply_rewriters ?restore ~tool_name magic ast =
  if magic = Config.ast_impl_magic_number then
    Obj.magic (apply_rewriters_str ?restore ~tool_name (Obj.magic ast))
  else if magic = Config.ast_intf_magic_number then
    Obj.magic (apply_rewriters_sig ?restore ~tool_name (Obj.magic ast))
  else
    assert false

(* Parse a file or get a dumped syntax tree from it *)

exception Outdated_version

let open_and_check_magic inputfile ast_magic =
  let ic = open_in_bin inputfile in
  let is_ast_file =
    try
      let buffer = really_input_string ic (String.length ast_magic) in
      if buffer = ast_magic then true
      else if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
        raise Outdated_version
      else false
    with
      Outdated_version ->
        Misc.fatal_error "OCaml and preprocessor have incompatible versions"
    | _ -> false
  in
  (ic, is_ast_file)

let file ppf ~tool_name inputfile parse_fun ast_magic =
  let (ic, is_ast_file) = open_and_check_magic inputfile ast_magic in
  let ast =
    try
      if is_ast_file then begin
        if !Clflags.fast then
          (* FIXME make this a proper warning *)
          fprintf ppf "@[Warning: %s@]@."
            "option -unsafe used with a preprocessor returning a syntax tree";
        Location.input_name := input_value ic;
        input_value ic
      end else begin
        seek_in ic 0;
        Location.input_name := inputfile;
        let lexbuf = Lexing.from_channel ic in
        Location.init lexbuf inputfile;
        parse_fun lexbuf
      end
    with x -> close_in ic; raise x
  in
  close_in ic;
  apply_rewriters ~restore:false ~tool_name ast_magic ast


let report_error ppf = function
  | CannotRun cmd ->
      fprintf ppf "Error while running external preprocessor@.\
                   Command line: %s@." cmd
  | WrongMagic cmd ->
      fprintf ppf "External preprocessor does not produce a valid file@.\
                   Command line: %s@." cmd

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let parse_all ~tool_name parse_fun magic ppf sourcefile =
  Location.input_name := sourcefile;
  let inputfile = preprocess sourcefile in
  let ast =
    try file ppf ~tool_name inputfile parse_fun magic
    with exn ->
      remove_preprocessed inputfile;
      raise exn
  in
  remove_preprocessed inputfile;
  ast

let parse_implementation ppf ~tool_name sourcefile =
  parse_all ~tool_name Parse.implementation
    Config.ast_impl_magic_number ppf sourcefile
let parse_interface ppf ~tool_name sourcefile =
  parse_all ~tool_name Parse.interface
    Config.ast_intf_magic_number ppf sourcefile

end
module Super_pparse
= struct
#1 "super_pparse.ml"
let fprintf = Format.fprintf

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/driver/pparse.ml#L170 *)
(* modified branches are commented *)
let report_error ppf = function
  | Pparse.CannotRun cmd ->
    (* modified *)
    if Ext_string.contain_substring cmd "refmt" then
      fprintf ppf 
        "@[<v>@{<info>There's been an error running Reason's refmt parser on a file.@}@,\
          This was the command:@,@,%s@,@,\
          @[Please file an issue on@ github.com/facebook/reason.@ Thanks!@]@]" cmd
    else 
      fprintf ppf "@[<v>@{<info>There's been an error running a preprocessor before the compilation of a file.@}@,\
                   This was the command:@,@,%s@]" cmd
  | WrongMagic cmd ->
      fprintf ppf "External preprocessor does not produce a valid file@.\
                   Command line: %s@." cmd

let setup () =
  Location.register_error_of_exn
    (function
      | Pparse.Error err -> Some (Super_location.error_of_printer_file report_error err)
      | _ -> None
    )

end
module Super_reason_react : sig 
#1 "super_reason_react.mli"
val component_spec_weak_type_variables: Types.type_expr -> bool * bool * bool
(** Used by super_typemod when we detect the message "... contains type variables that cannot be generalized" *)

val component_spec_weak_type_variables_in_module_type: Types.module_type -> (bool * bool * bool) list
(** Used by super_typemod when we detect the message "... contains type variables that cannot be generalized" *)

val state_escape_scope: (Types.type_expr * Types.type_expr) list -> bool
(** Used by super_typecore when we detect the message "The type constructor state would escape its scope" *)

val trace_both_component_spec: (Types.type_expr * Types.type_expr) list -> bool
(** Used by super_typecore when we detect the message "The type constructor state would escape its scope" *)

val is_component_spec_wanted_react_element: (Types.type_expr * Types.type_expr) list -> bool
(** Used by super_typecore when we detect the message "This has type componentSpec but expected reactElement" *)

end = struct
#1 "super_reason_react.ml"
(* This file detects common error from
  [ReasonReact](https://reasonml.github.io/reason-react/) and provide
  situation-specific hints. See the mli file to see which heurisics we detect
  and related comments *)
open Types

let rec drill_through_tlink_and_tsubst t =
  match t.desc with
  | Tlink t
  | Tsubst t -> drill_through_tlink_and_tsubst t
  | _ -> t

let is_weak_type_after_drilling t =
  match drill_through_tlink_and_tsubst t with
  | {desc = Tvar _} -> true
  | _ -> false

let component_spec_weak_type_variables t =
  match drill_through_tlink_and_tsubst t with
  (* ReasonReact <=0.3.4 *)
  | {desc = Tconstr (
      Pdot ((Pident {name = "ReasonReact"}), "componentSpec", _),
      [state; _initial_state; retained_props; _initial_retained_props; action],
      _
    )} ->
    (
      state |> is_weak_type_after_drilling,
      retained_props |> is_weak_type_after_drilling,
      action |> is_weak_type_after_drilling
    )
  (* future ReasonReact version with retainedProps removed *)
  | {desc = Tconstr (
      Pdot ((Pident {name = "ReasonReact"}), "componentSpec", _),
      [state; _initial_state; action],
      _
    )} ->
    (
      state |> is_weak_type_after_drilling,
      false,
      action |> is_weak_type_after_drilling
    )
  | _ -> (false, false, false)

let component_spec_weak_type_variables_in_module_type (mty : Types.module_type) =
  match mty with
  | Mty_signature signature_values ->
      signature_values
        |> List.map (function
          | Sig_value (_id, value_desc) ->
            let typ = value_desc.val_type in
            component_spec_weak_type_variables typ
          | _ -> (false, false, false)
        )
        |> List.filter (function
          | (false, false, false) -> false
          | _ -> true
        )
  | _ -> []

(* `trace` is a funny data structure. It's an always even list of tuples. This error:
  this is foo (aliased as array(int)), wanted bar (aliased as array(string))
  the incompatible part: int vs string
  gives the following `trace` data structure:
  [
    (foo, array(int)),
    (bar, array(string)),
    (_, int),
    (_, string)
  ]
 *)
(* recursively walk the trace from right to left, calling f and checking if f matches part of the trace *)
let check_each_trace_chunk_bottom_up f = fun t ->
  let t_flipped = List.rev t in
  let rec check f = function
  (* we flipped the trace, so instead of [t1, t2, t3, t4, ...] it's [t4, t3, ...] *)
  | (_alias2, type2) :: (_alias1, type1) :: rest ->
    if f (type1, type2) then true
    else check f rest
  | _ -> false
  in
  check f t_flipped


let state_escape_scope = check_each_trace_chunk_bottom_up (function
  (* https://github.com/BuckleScript/ocaml/blob/ddf5a739cc0978dab5e553443825791ba7b0cef9/typing/printtyp.ml?#L1348 *)
  (* so apparently that's the logic for detecting "the constructor out of scope" error *)
  | ({desc = Tconstr (p, _, _)}, {desc = Tvar _; level})
    when level < Path.binding_time p -> true
  | _ -> false
)

let trace_both_component_spec = check_each_trace_chunk_bottom_up (function
  | ({desc = Tconstr (
      (Pdot ((Pident {name = "ReasonReact"}), "componentSpec", _)),
      ([state1; _; _; _; action1] | [state1; _; action1]),
      _
    )},
    {desc = Tconstr (
      (Pdot ((Pident {name = "ReasonReact"}), "componentSpec", _)),
      ([state2; _; _; _; action2] | [state2; _; action2]),
      _
    )})
    -> true
  | _ -> false
)

let is_component_spec_wanted_react_element = check_each_trace_chunk_bottom_up (function
  | ({desc = Tconstr (
      (Pdot ((Pident {name = "ReasonReact"}), "componentSpec", _)),
      ([state1; _; _; _; action1] | [state1; _; action1]),
      _
    )},
    {desc = Tconstr (
      (Pdot ((Pident {name = "ReasonReact"}), "reactElement", _)),
      _,
      _
    )}) -> true
  | _ -> false
)

end
module Super_typecore
= struct
#1 "super_typecore.ml"
(* open Misc *)
(* open Asttypes *)
(* open Parsetree *)
open Types
(* open Typedtree *)
open Btype
open Ctype

let fprintf = Format.fprintf
let sprintf = Format.sprintf
let longident = Printtyp.longident
let super_report_unification_error = Printtyp.super_report_unification_error
let reset_and_mark_loops = Printtyp.reset_and_mark_loops
let type_expr = Printtyp.type_expr

let tagged tag fn ppf arg =
    Format.pp_open_tag ppf tag;
    fn ppf arg;
    Format.pp_close_tag ppf ()

let rec bottom_aliases = function
  | (_, one) :: (_, two) :: rest -> begin match bottom_aliases rest with
    | Some types -> Some types
    | None -> Some (one, two)
  end
  | _ -> None

let simple_conversions = [
  (("float", "int"), "int_of_float");
  (("int", "float"), "float_of_int");
  (("int", "string"), "string_of_int");
  (("float", "string"), "string_of_float");
]

let print_simple_conversion ppf (actual, expected) =
  try (
    let converter = List.assoc (actual, expected) simple_conversions in
    Format.pp_print_newline ppf ();
    Format.pp_print_newline ppf ();
    fprintf ppf "You can convert a @{<info>%s@} to a @{<info>%s@} with @{<info>%s@}." actual expected converter
  ) with | Not_found -> ()

let print_simple_message ppf = function
  | ("float", "int") -> fprintf ppf "If this is a literal, you want a number without a trailing dot (e.g. @{<info>20@}).@;"
  | ("int", "float") -> fprintf ppf "If this is a literal, you want a number with a trailing dot (e.g. @{<info>20.@}).@;"
  | _ -> ()

let show_extra_help ppf env trace = begin
  match bottom_aliases trace with
  | Some ({desc = Tconstr (actualPath, actualArgs, _)}, {desc = Tconstr (expectedPath, expextedArgs, _)}) -> begin
    match (actualPath, actualArgs, expectedPath, expextedArgs) with
    | (Pident {name = actualName}, [], Pident {name = expectedName}, []) -> begin
      print_simple_conversion ppf (actualName, expectedName);
      print_simple_message ppf (actualName, expectedName);
    end
    | _ -> ()
  end;
  | _ -> ();
end

(* given type1 is foo => bar => baz(qux) and type 2 is bar => baz(qux), return Some(foo) *)
let rec collect_missing_arguments env type1 type2 = match type1 with
  (* why do we use Ctype.matches here? Please see https://github.com/BuckleScript/bucklescript/pull/2554 *)
  | {desc=Tarrow (label, argtype, typ, _)} when Ctype.matches env typ type2 ->
    Some [(label, argtype)]
  | {desc=Tarrow (label, argtype, typ, _)} -> begin
    match collect_missing_arguments env typ type2 with
    | Some res -> Some ((label, argtype) :: res)
    | None -> None
    end
  | _ -> None

let check_bs_arity_mismatch ppf trace =
  let arity t = match t.desc with
    | Tvariant { row_fields = [(label,_)] } ->
        let label_len = String.length label in
        let arity_str = "Arity_" in
        let arity_len = String.length arity_str in
        if arity_len < label_len &&
          String.sub label 0 arity_len = arity_str
        then
          try
            Some (int_of_string (String.sub label arity_len (label_len-arity_len)))
          with _ -> None
        else None
    | _ ->
        None in
  let check_mismatch t1 t2 = match (arity t1, arity t2) with
    | Some n1, Some n2 ->
        fprintf ppf "@[@{<info>Found uncurried application [@bs] with arity %d, where arity %d was expected.@}@]" n1 n2;
        true
    | None, _
    | _, None ->
        false in
  let rec traverse = function
    | (_arity1, type1) :: (_arity2, type2) :: rest ->
        if traverse rest
        then true
        else check_mismatch type1 type2
    | _ ->
        false in
  ignore (traverse trace)

let print_expr_type_clash env trace ppf =
  (* this is the most frequent error. Do whatever we can to provide specific
    guidance to this generic error before giving up *)
  if Super_reason_react.state_escape_scope trace && Super_reason_react.trace_both_component_spec trace then
    fprintf ppf "@[<v>\
      @[@{<info>Is this a ReasonReact reducerComponent or component with retained props?@}@ \
      If so, is the type for state, retained props or action declared _after_@ the component declaration?@ \
      @{<info>Moving these types above the component declaration@} should resolve this!@]\
    @]"
    (* This one above shouldn't catch any false positives, so we can safely not display the original type clash error. *)
  else if Super_reason_react.is_component_spec_wanted_react_element trace then
    fprintf ppf "@[<v>\
      @[@{<info>Did you want to create a ReasonReact element without using JSX?@}@ If not, disregard this.@ \
      If so, don't forget to wrap this value in `ReasonReact.element` yourself:@ https://reasonml.github.io/reason-react/docs/en/jsx.html#capitalized@]@,@,\
      @[@{<info>Here's the original error message@}@]@,\
    @]";
    begin
    let bottom_aliases_result = bottom_aliases trace in
    let missing_arguments = match bottom_aliases_result with
    | Some (actual, expected) -> collect_missing_arguments env actual expected
    | None -> assert false
    in
    let print_arguments =
      Format.pp_print_list
        ~pp_sep:(fun ppf _ -> fprintf ppf ",@ ")
        (fun ppf (label, argtype) ->
          if label = "" then fprintf ppf "@[%a@]" type_expr argtype
          else fprintf ppf "@[(~%s: %a)@]" label type_expr argtype
        )
    in
    match missing_arguments with
    | Some [singleArgument] ->
      (* btw, you can't say "final arguments". Intermediate labeled
        arguments might be the ones missing *)
      fprintf ppf "@[@{<info>This call is missing an argument@} of type@ %a@]"
        print_arguments [singleArgument]
    | Some arguments ->
      fprintf ppf "@[<hv>@{<info>This call is missing arguments@} of type:@ %a@]"
        print_arguments arguments
    | None ->
      let missing_parameters = match bottom_aliases_result with
      | Some (actual, expected) -> collect_missing_arguments env expected actual
      | None -> assert false
      in
      begin match missing_parameters with
      | Some [singleParameter] ->
        fprintf ppf "@[This value might need to be @{<info>wrapped in a function@ that@ takes@ an@ extra@ parameter@}@ of@ type@ %a@]@,@,"
          print_arguments [singleParameter];
        fprintf ppf "@[@{<info>Here's the original error message@}@]@,"
      | Some arguments ->
        fprintf ppf "@[This value seems to @{<info>need to be wrapped in a function that takes extra@ arguments@}@ of@ type:@ @[<hv>%a@]@]@,@,"
          print_arguments arguments;
        fprintf ppf "@[@{<info>Here's the original error message@}@]@,"
      | None -> ()
      end;
      (* final fallback: show the generic type mismatch error *)
      check_bs_arity_mismatch ppf trace;
      super_report_unification_error ppf env trace
        (function ppf ->
            fprintf ppf "This has type:")
        (function ppf ->
            fprintf ppf "But somewhere wanted:");
      show_extra_help ppf env trace;
    end
(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typecore.ml#L3769 *)
(* modified branches are commented *)
let report_error env ppf = function
  | Typecore.Polymorphic_label lid ->
      fprintf ppf "@[The record field %a is polymorphic.@ %s@]"
        longident lid "You cannot instantiate it in a pattern."
  | Constructor_arity_mismatch(lid, expected, provided) ->
      (* modified *)
      fprintf ppf
       "@[This variant constructor, %a, expects %i %s; here, we've %sfound %i.@]"
       longident lid expected (if expected == 1 then "argument" else "arguments") (if provided < expected then "only " else "") provided
  | Label_mismatch(lid, trace) ->
      super_report_unification_error ppf env trace
        (function ppf ->
           fprintf ppf "The record field %a@ belongs to the type"
                   longident lid)
        (function ppf ->
           fprintf ppf "but is mixed here with fields of type")
  | Pattern_type_clash trace ->
      super_report_unification_error ppf env trace
        (function ppf ->
          fprintf ppf "This pattern matches values of type")
        (function ppf ->
          fprintf ppf "but a pattern was expected which matches values of type")
  | Or_pattern_type_clash (id, trace) ->
      super_report_unification_error ppf env trace
        (function ppf ->
          fprintf ppf "The variable %s on the left-hand side of this or-pattern has type" (Ident.name id))
        (function ppf ->
          fprintf ppf "but on the right-hand side it has type")
  | Multiply_bound_variable name ->
      fprintf ppf "Variable %s is bound several times in this matching" name
  | Orpat_vars id ->
      fprintf ppf "Variable %s must occur on both sides of this | pattern"
        (Ident.name id)
  | Expr_type_clash trace ->
      (* modified *)
      fprintf ppf "@[<v>";
      print_expr_type_clash env trace ppf;
      fprintf ppf "@]"
  | Apply_non_function typ ->
      (* modified *)
      reset_and_mark_loops typ;
      begin match (repr typ).desc with
        Tarrow (_, _inputType, returnType, _) ->
          let rec countNumberOfArgs count {desc} = match desc with
          | Tarrow (_, _inputType, returnType, _) -> countNumberOfArgs (count + 1) returnType
          | _ -> count
          in
          let countNumberOfArgs = countNumberOfArgs 1 in
          let acceptsCount = countNumberOfArgs returnType in
          fprintf ppf "@[<v>@[<2>This function has type@ @{<info>%a@}@]"
            type_expr typ;
          fprintf ppf "@ @[It only accepts %i %s; here, it's called with more.@]@]"
                      acceptsCount (if acceptsCount == 1 then "argument" else "arguments")
      | Tconstr (
          (Path.Pdot (((Pdot (Path.Pident {name="Js"}, "Internal", _)) | (Pident {name="Js_internal"})), ("fn" | "meth"), _)),
          _,
          _
        )
        ->
          fprintf
            ppf
            "@[<v>This is an uncurried BuckleScript function. @{<info>It must be applied with a dot@}.@,@,\
            Like this: @{<info>foo(. a, b)@}@,\
            Not like this: @{<dim>foo(a, b)@}@,@,\
            This guarantees that your function is fully applied. More info here:@,\
            https://bucklescript.github.io/docs/en/function.html#solution-guaranteed-uncurrying@]"
      | _ ->
          fprintf ppf "@[<v>@[<2>This expression has type@ %a@]@ %s@]"
            type_expr typ
            "It is not a function."
      end
  | Apply_wrong_label (l, ty) ->
      let print_label ppf = function
        | "" -> fprintf ppf "without label"
        | l ->
            fprintf ppf "with label %s" (prefixed_label_name l)
      in
      reset_and_mark_loops ty;
      fprintf ppf
        "@[<v>@[<2>The function applied to this argument has type@ %a@]@.\
          This argument cannot be applied %a@]"
        type_expr ty print_label l
  | Label_multiply_defined s ->
      fprintf ppf "The record field label %s is defined several times" s
  | Label_missing labels ->
      let print_labels ppf =
        List.iter (fun lbl -> fprintf ppf "@ %s" (Ident.name lbl)) in
      fprintf ppf "@[<hov>Some record fields are undefined:%a@]"
        print_labels labels
  | Label_not_mutable lid ->
      fprintf ppf "The record field %a is not mutable" longident lid
  | Wrong_name (eorp, ty, kind, p, lid) as foo ->
      (* forwarded *)
      Typecore.report_error env ppf foo
      (* reset_and_mark_loops ty;
      fprintf ppf "@[@[<2>%s type@ %a@]@ "
        eorp type_expr ty;
      fprintf ppf "The %s %a does not belong to type %a@]"
        (if kind = "record" then "field" else "constructor")
        longident lid (*kind*) path p;
      if kind = "record" then Label.spellcheck ppf env p lid
                         else Constructor.spellcheck ppf env p lid *)
  | Name_type_mismatch (kind, lid, tp, tpl) ->
      let name = if kind = "record" then "field" else "constructor" in
      Printtyp.report_ambiguous_type_error ppf env tp tpl
        (function ppf ->
           fprintf ppf "The %s %a@ belongs to the %s type"
             name longident lid kind)
        (function ppf ->
           fprintf ppf "The %s %a@ belongs to one of the following %s types:"
             name longident lid kind)
        (function ppf ->
           fprintf ppf "but a %s was expected belonging to the %s type"
             name kind)
  | Invalid_format msg ->
      fprintf ppf "%s" msg
  | Undefined_method (ty, me) ->
      reset_and_mark_loops ty;
      fprintf ppf
        "@[<v>@[This expression has type@;<1 2>%a@]@,\
         It has no method %s@]" type_expr ty me
  | Undefined_inherited_method me ->
      fprintf ppf "This expression has no method %s" me
  | Virtual_class cl ->
      fprintf ppf "Cannot instantiate the virtual class %a"
        longident cl
  | Unbound_instance_variable v ->
      fprintf ppf "Unbound instance variable %s" v
  | Instance_variable_not_mutable (b, v) ->
      if b then
        fprintf ppf "The instance variable %s is not mutable" v
      else
        fprintf ppf "The value %s is not an instance variable" v
  | Not_subtype(tr1, tr2) ->
      Printtyp.report_subtyping_error ppf env tr1 "is not a subtype of" tr2
  | Outside_class ->
      fprintf ppf "This object duplication occurs outside a method definition"
  | Value_multiply_overridden v ->
      fprintf ppf "The instance variable %s is overridden several times" v
  | Coercion_failure (ty, ty', trace, b) ->
      super_report_unification_error ppf env trace
        (function ppf ->
           let ty, ty' = Printtyp.prepare_expansion (ty, ty') in
           fprintf ppf
             "This expression cannot be coerced to type@;<1 2>%a;@ it has type"
           (Printtyp.type_expansion ty) ty')
        (function ppf ->
           fprintf ppf "but is here used with type");
      if b then
        fprintf ppf ".@.@[<hov>%s@ %s@]"
          "This simple coercion was not fully general."
          "Consider using a double coercion."
  | Too_many_arguments (in_function, ty) ->
      (* modified *)
      reset_and_mark_loops ty;
      if in_function then begin
        fprintf ppf "@[This function expects too many arguments,@ ";
        fprintf ppf "it should have type@ %a@]"
          type_expr ty
      end else begin
        fprintf ppf "@[This expression should not be a function,@ ";
        fprintf ppf "the expected type is@ %a@]"
          type_expr ty
      end
  | Abstract_wrong_label (l, ty) ->
      let label_mark = function
        | "" -> "but its first argument is not labelled"
        |  l -> sprintf "but its first argument is labelled %s"
          (prefixed_label_name l) in
      reset_and_mark_loops ty;
      fprintf ppf "@[<v>@[<2>This function should have type@ %a@]@,%s@]"
      type_expr ty (label_mark l)
  | Scoping_let_module(id, ty) ->
      reset_and_mark_loops ty;
      fprintf ppf
       "This `let module' expression has type@ %a@ " type_expr ty;
      fprintf ppf
       "In this type, the locally bound module name %s escapes its scope" id
  | Masked_instance_variable lid ->
      fprintf ppf
        "The instance variable %a@ \
         cannot be accessed from the definition of another instance variable"
        longident lid
  | Private_type ty ->
      fprintf ppf "Cannot create values of the private type %a" type_expr ty
  | Private_label (lid, ty) ->
      fprintf ppf "Cannot assign field %a of the private type %a"
        longident lid type_expr ty
  | Not_a_variant_type lid ->
      fprintf ppf "The type %a@ is not a variant type" longident lid
  | Incoherent_label_order ->
      fprintf ppf "This function is applied to arguments@ ";
      fprintf ppf "in an order different from other calls.@ ";
      fprintf ppf "This is only allowed when the real type is known."
  | Less_general (kind, trace) ->
      super_report_unification_error ppf env trace
        (fun ppf -> fprintf ppf "This %s has type" kind)
        (fun ppf -> fprintf ppf "which is less general than")
  | Modules_not_allowed ->
      fprintf ppf "Modules are not allowed in this pattern."
  | Cannot_infer_signature ->
      fprintf ppf
        "The signature for this packaged module couldn't be inferred."
  | Not_a_packed_module ty ->
      fprintf ppf
        "This expression is packed module, but the expected type is@ %a"
        type_expr ty
  | Recursive_local_constraint trace ->
      super_report_unification_error ppf env trace
        (function ppf ->
           fprintf ppf "Recursive local constraint when unifying")
        (function ppf ->
           fprintf ppf "with")
  | Unexpected_existential ->
      fprintf ppf
        "Unexpected existential"
  | Unqualified_gadt_pattern (tpath, name) ->
      fprintf ppf "@[The GADT constructor %s of type %a@ %s.@]"
        name Printtyp.path tpath
        "must be qualified in this pattern"
  | Invalid_interval ->
      fprintf ppf "@[Only character intervals are supported in patterns.@]"
  | Invalid_for_loop_index ->
      fprintf ppf
        "@[Invalid for-loop index: only variables and _ are allowed.@]"
  | No_value_clauses ->
      fprintf ppf
        "None of the patterns in this 'match' expression match values."
  | Exception_pattern_below_toplevel ->
      fprintf ppf
        "@[Exception patterns must be at the top level of a match case.@]"

let report_error env ppf err =
  Printtyp.wrap_printing_env env (fun () -> report_error env ppf err)

(* This will be called in super_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn
    (function
      | Typecore.Error (loc, env, err) ->
        Some (Super_location.error_of_printer loc (report_error env) err)
      | Typecore.Error_forward err ->
        Some err
      | _ ->
        None
    )

end
module Super_typemod
= struct
#1 "super_typemod.ml"
open Printtyp

let fprintf = Format.fprintf

let non_generalizable_msg ppf ~result ~is_module print_fallback_msg =
  let contain_vs_be =
    if is_module then "This module seems to contain" else "This seems to be" in
  match result with
  | (_, true, _) :: _ ->
    fprintf ppf "@[<v>\
      @{<info>%s a ReasonReact reducerComponentWithRetainedProps?@}@ \
      The retained props feature is deprecated.@ \
      Please use a regular @{<info>reducerComponent@} and keep the props you want to retain in state.\
    @]"
    contain_vs_be
  | (true, _, _) :: _->
    fprintf ppf "@[<v>\
      @[\
        @{<info>%s a ReasonReact reducerComponent?@}@ \
        We don't have@ all@ the@ type@ info@ for@ its@ @{<info>state@}.@ \
        Make sure you've done the following: @]@,@,\
        @[- Define the component `make` function@]@,\
        @[- Define `reducer` in that `make` body@]@,\
        @[- Annotate reducer's second parameter (state) with the desired state type\
      @]\
    @]"
    contain_vs_be
  | (_, _, true) :: _->
    fprintf ppf "@[<v>\
      @[\
        @{<info>%s a ReasonReact reducerComponent?@}@ \
        We don't have@ all@ the@ type@ info@ for@ its@ @{<info>action@}.@ \
        Make sure you've done the following: @]@,@,\
        @[- Define the component `make` function@]@,\
        @[- Define `reducer` in that `make` body@]@,\
        @[- Annotate reducer's first parameter (action) with the desired action type\
      @]\
    @]"
    contain_vs_be
  | _ ->
    fprintf ppf
      "%a@,@,\
      @[This happens when the type system senses there's a mutation/side-effect,@ in combination with a polymorphic value.@,\
      @{<info>Using or annotating that value usually solves it.@}@ \
      More info:@ https://realworldocaml.org/v1/en/html/imperative-programming-1.html#side-effects-and-weak-polymorphism@]"
    print_fallback_msg ()

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typemod.ml#L1754 *)
(* modified branches are commented *)
let report_error ppf = Typemod.(function
    Cannot_apply mty ->
      fprintf ppf
        "@[This module is not a functor; it has type@ %a@]" modtype mty
  | Not_included errs ->
      fprintf ppf
        "@[<v>Signature mismatch:@ %a@]" Includemod.report_error errs
  | Cannot_eliminate_dependency mty ->
      fprintf ppf
        "@[This functor has type@ %a@ \
           The parameter cannot be eliminated in the result type.@  \
           Please bind the argument to a module identifier.@]" modtype mty
  | Signature_expected -> fprintf ppf "This module type is not a signature"
  | Structure_expected mty ->
      fprintf ppf
        "@[This module is not a structure; it has type@ %a" modtype mty
  | With_no_component lid ->
      fprintf ppf
        "@[The signature constrained by `with' has no component named %a@]"
        longident lid
  | With_mismatch(lid, explanation) ->
      fprintf ppf
        "@[<v>\
           @[In this `with' constraint, the new definition of %a@ \
             does not match its original definition@ \
             in the constrained signature:@]@ \
           %a@]"
        longident lid Includemod.report_error explanation
  | Repeated_name(kind, name) ->
      fprintf ppf
        "@[Multiple definition of the %s name %s.@ \
           Names must be unique in a given structure or signature.@]" kind name
  | Non_generalizable typ ->
      (* modified *)
      fprintf ppf "@[<v>";
      non_generalizable_msg
        ppf
        ~result:([Super_reason_react.component_spec_weak_type_variables typ])
        ~is_module:false
        (fun ppf () ->
          fprintf ppf
          "@[This expression's type contains type variables that can't be generalized:@,@{<error>%a@}@]"
          type_scheme typ);
      fprintf ppf "@]"
  | Non_generalizable_class (id, desc) ->
      fprintf ppf
        "@[The type of this class,@ %a,@ \
           contains type variables that cannot be generalized@]"
        (class_declaration id) desc
  | Non_generalizable_module mty ->
      (* modified *)
      fprintf ppf "@[<v>";
      non_generalizable_msg
        ppf
        ~result:(Super_reason_react.component_spec_weak_type_variables_in_module_type mty)
        ~is_module:true
        (fun ppf () ->
          fprintf ppf
            "@[The type of this module contains type variables that cannot be generalized:@,@{<error>%a@}@]"
            modtype mty);
        fprintf ppf "@]"
  | Implementation_is_required intf_name ->
      fprintf ppf
        "@[The interface %a@ declares values, not just types.@ \
           An implementation must be provided.@]"
        Location.print_filename intf_name
  | Interface_not_compiled intf_name ->
      fprintf ppf
        "@[Could not find the .cmi file for interface@ %a.@]"
        Location.print_filename intf_name
  | Not_allowed_in_functor_body ->
      fprintf ppf
        "@[This expression creates fresh types.@ %s@]"
        "It is not allowed inside applicative functors."
  | With_need_typeconstr ->
      fprintf ppf
        "Only type constructors with identical parameters can be substituted."
  | Not_a_packed_module ty ->
      fprintf ppf
        "This expression is not a packed module. It has type@ %a"
        type_expr ty
  | Incomplete_packed_module ty ->
      fprintf ppf
        "The type of this packed module contains variables:@ %a"
        type_expr ty
  | Scoping_pack (lid, ty) ->
      fprintf ppf
        "The type %a in this module cannot be exported.@ " longident lid;
      fprintf ppf
        "Its type contains local dependencies:@ %a" type_expr ty
  | Recursive_module_require_explicit_type ->
      fprintf ppf "Recursive modules require an explicit module type."
  | Apply_generative ->
      fprintf ppf "This is a generative functor. It can only be applied to ()"
)

let report_error env ppf err =
  Printtyp.wrap_printing_env env (fun () -> report_error ppf err)

(* This will be called in super_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn
    (function
      | Typemod.Error (loc, env, err) ->
        Some (Super_location.error_of_printer loc (report_error env) err)
      | Typemod.Error_forward err ->
        Some err
      | _ ->
        None
    )

end
module Super_typetexp
= struct
#1 "super_typetexp.ml"
(* open Misc *)
(* open Asttypes *)
(* open Parsetree *)
open Types
(* open Typedtree *)
(* open Btype *)
(* open Ctype *)

open Format
open Printtyp

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typetexp.ml#L869 *)
let spellcheck ppf fold env lid =
  let cutoff =
    match String.length (Longident.last lid) with
      | 1 | 2 -> 0
      | 3 | 4 -> 1
      | 5 | 6 -> 2
      | _ -> 3
  in
  let compare target head acc =
    let (best_choice, best_dist) = acc in
    match Misc.edit_distance target head cutoff with
      | None -> (best_choice, best_dist)
      | Some dist ->
        let choice =
          if dist < best_dist then [head]
          else if dist = best_dist then head :: best_choice
          else best_choice in
        (choice, min dist best_dist)
  in
  let init = ([], max_int) in
  let handle (choice, _dist) =
    match List.rev choice with
      | [] -> ()
      | last :: rev_rest ->
        (* the modified part *)
        fprintf ppf "@[<v 2>@,@,@{<info>Hint: Did you mean %s%s%s?@}@]"
          (String.concat ", " (List.rev rev_rest))
          (if rev_rest = [] then "" else " or ")
          last
  in
  (* flush now to get the error report early, in the (unheard of) case
     where the linear search would take a bit of time; in the worst
     case, the user has seen the error, she can interrupt the process
     before the spell-checking terminates. *)
  fprintf ppf "@?";
  match lid with
    | Longident.Lapply _ -> ()
    | Longident.Lident s ->
      handle (fold (compare s) None env init)
    | Longident.Ldot (r, s) ->
      handle (fold (compare s) (Some r) env init)

let spellcheck ppf fold =
  spellcheck ppf (fun f -> fold (fun s _ _ x -> f s x))

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typetexp.ml#L918 *)
(* modified branches are commented *)
let report_error env ppf = function
  | Typetexp.Unbound_type_variable name ->
    fprintf ppf "Unbound type parameter %s@." name
  | Unbound_type_constructor lid ->
    (* modified *)
    fprintf ppf "This type constructor's parameter, `%a`, can't be found. Is it a typo?" longident lid;
    spellcheck ppf Env.fold_types env lid;
  | Unbound_type_constructor_2 p ->
    fprintf ppf "The type constructor@ %a@ is not yet completely defined"
      path p
  | Type_arity_mismatch(lid, expected, provided) ->
    fprintf ppf
      "@[The type constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
      longident lid expected provided
  | Bound_type_variable name ->
    fprintf ppf "Already bound type parameter '%s" name
  | Recursive_type ->
    fprintf ppf "This type is recursive"
  | Unbound_row_variable lid ->
      (* we don't use "spellcheck" here: this error is not raised
         anywhere so it's unclear how it should be handled *)
      fprintf ppf "Unbound row variable in #%a" longident lid
  | Type_mismatch trace ->
      Printtyp.super_report_unification_error ppf Env.empty trace
        (function ppf ->
           fprintf ppf "This type")
        (function ppf ->
           fprintf ppf "should be an instance of type")
  | Alias_type_mismatch trace ->
      Printtyp.super_report_unification_error ppf Env.empty trace
        (function ppf ->
           fprintf ppf "This alias is bound to type")
        (function ppf ->
           fprintf ppf "but is used as an instance of type")
  | Present_has_conjunction l ->
      fprintf ppf "The present constructor %s has a conjunctive type" l
  | Present_has_no_type l ->
      fprintf ppf "The present constructor %s has no type" l
  | Constructor_mismatch (ty, ty') ->
      wrap_printing_env env (fun ()  ->
        Printtyp.reset_and_mark_loops_list [ty; ty'];
        fprintf ppf "@[<hov>%s %a@ %s@ %a@]"
          "This variant type contains a constructor"
          Printtyp.type_expr ty
          "which should be"
          Printtyp.type_expr ty')
  | Not_a_variant ty ->
      Printtyp.reset_and_mark_loops ty;
      fprintf ppf "@[The type %a@ is not a polymorphic variant type@]"
        Printtyp.type_expr ty
  | Variant_tags (lab1, lab2) ->
      fprintf ppf
        "@[Variant tags `%s@ and `%s have the same hash value.@ %s@]"
        lab1 lab2 "Change one of them."
  | Invalid_variable_name name ->
      fprintf ppf "The type variable name %s is not allowed in programs" name
  | Cannot_quantify (name, v) ->
      fprintf ppf
        "@[<hov>The universal type variable '%s cannot be generalized:@ %s.@]"
        name
        (if Btype.is_Tvar v then "it escapes its scope" else
         if Btype.is_Tunivar v then "it is already bound to another variable"
         else "it is not a variable")
  | Multiple_constraints_on_type s ->
      fprintf ppf "Multiple constraints for type %a" longident s
  | Repeated_method_label s ->
      fprintf ppf "@[This is the second method `%s' of this object type.@ %s@]"
        s "Multiple occurences are not allowed."
  | Unbound_value lid ->
      (* modified *)
      begin
        match lid with
        | Ldot (outer, inner) ->
          fprintf ppf "The value %s can't be found in %a"
            inner
            Printtyp.longident outer;
        | other_ident -> fprintf ppf "The value %a can't be found" Printtyp.longident other_ident
      end;
      spellcheck ppf Env.fold_values env lid;
  | Unbound_module lid ->
      (* modified *)
      begin match lid with
      | Lident "Str" -> 
        begin
          fprintf ppf "@[\
              @{<info>The module or file %a can't be found.@}@,@,\
              Are you trying to use the standard library's Str?@ \
              If you're compiling to JavaScript,@ use @{<info>Js.Re@} instead.@ \
              Otherwise, add str.cma to your ocamlc/ocamlopt command.\
            @]"
            longident lid 
        end
      | lid -> 
        begin
          fprintf ppf "@[<v>\
              @{<info>The module or file %a can't be found.@}@,\
              @[<v 2>- If it's a third-party dependency:@,\
                - Did you list it in bsconfig.json?@,\
                - @[Did you run `bsb` instead of `bsb -make-world`@ (latter builds third-parties)@]?\
              @]@,\
              - Did you include the file's directory in bsconfig.json?@]\
            @]"
            longident lid
        end
      end;
      spellcheck ppf Env.fold_modules env lid
  | Unbound_constructor lid ->
      (* modified *)
      fprintf ppf "@[<v>\
      @{<info>The variant constructor %a can't be found.@}@,@,\
      @[<v 2>- If it's defined in another module or file, bring it into scope by:@,\
        @[- Annotating it with said module name:@ @{<info>let food = MyModule.Apple@}@]@,\
        @[- Or specifying its type:@ @{<info>let food: MyModule.fruit = Apple@}@]\
      @]@,\
      - @[Constructors and modules are both capitalized.@ Did you want the latter?@ Then instead of @{<dim>let foo = Bar@}, try @{<info>module Foo = Bar@}.@]\
      @]"
      longident lid;
      Typetexp.spellcheck_simple ppf Env.fold_constructors (fun d -> d.cstr_name)
        env lid;
  | Unbound_label lid ->
      (* modified *)
      fprintf ppf "@[<v>\
      @{<info>The record field %a can't be found.@}@,@,\
      If it's defined in another module or file, bring it into scope by:@,\
      @[- Annotating it with said module name:@ @{<info>let baby = {MyModule.age: 3}@}@]@,\
      @[- Or specifying its type:@ @{<info>let baby: MyModule.person = {age: 3}@}@]\
      @]"
      longident lid;
      Typetexp.spellcheck_simple ppf Env.fold_labels (fun d -> d.lbl_name) env lid;
  | Unbound_class lid ->
      fprintf ppf "Unbound class %a" longident lid;
      spellcheck ppf Env.fold_classs env lid;
  | Unbound_modtype lid ->
      fprintf ppf "Unbound module type %a" longident lid;
      spellcheck ppf Env.fold_modtypes env lid;
  | Unbound_cltype lid ->
      fprintf ppf "Unbound class type %a" longident lid;
      spellcheck ppf Env.fold_cltypes env lid;
  | Ill_typed_functor_application lid ->
      fprintf ppf "Ill-typed functor application %a" longident lid
  | Illegal_reference_to_recursive_module ->
      fprintf ppf "Illegal recursive module reference"
  | Access_functor_as_structure lid ->
      fprintf ppf "The module %a is a functor, not a structure" longident lid

(* This will be called in super_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn
    (function
      | Typetexp.Error (loc, env, err) ->
        Some (Super_location.error_of_printer loc (report_error env) err)
      (* typetexp doesn't expose Error_forward  *)
      (* | Error_forward err ->
        Some err *)
      | _ ->
        None
    )

end
module Super_main
= struct
#1 "super_main.ml"
(* the entry point. This is used by js_main.ml *)
let setup () =
  Super_location.setup ();
  Super_typetexp.setup ();
  Super_typemod.setup ();
  Super_typecore.setup ();
  Super_env.setup ();
  Super_pparse.setup ();

end
