open Misc
open Asttypes
open Parsetree
open Types
open Typedtree
open Btype
open Ctype

open Format
open Printtyp

(* taken from https://github.com/ocaml/ocaml/blob/4.02/parsing/location.ml#L337 *)
(* we override the default reporter with this one *)
let rec better_error_reporter ppf ({Location.loc; msg; sub; if_highlight} as err) =
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
    Format.fprintf ppf "%a%a %s" Location.print loc Location.print_error_prefix () (msg ^ "\n");
    List.iter (Format.fprintf ppf "@\n@[<2>%a@]" better_error_reporter) sub
  end

(* This will be called in js_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.error_reporter := better_error_reporter