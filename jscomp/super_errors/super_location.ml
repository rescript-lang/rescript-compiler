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
(* we override the default reporter with this one. Everything is the same as of the 4.02, except the part commented below *)
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
    Format.fprintf ppf "%a%a %s" Location.print loc Location.print_error_prefix () (msg ^ "\n");
    (* this `super_error_reporter` part is the part that's different from default_error_reporter. In the future, we might customize the formatting a bit more too. *)
    List.iter (Format.fprintf ppf "@\n@[<2>%a@]" super_error_reporter) sub
  end

let warning_prefix = "Warning"

(* extracted from https://github.com/ocaml/ocaml/blob/4.02/parsing/location.ml#L280 *)
(* this is the same code except we use Super_warnings.print instead of Warnings.print  *)
(* we'll replace the default printer with this one *)
let super_warning_printer loc ppf w =
  if Warnings.is_active w then begin
    Misc.Color.setup !Clflags.color;
    Location.print ppf loc;
    Format.fprintf ppf "@{<warning>%s@} %a@." warning_prefix Super_warnings.print w
  end
;;

(* This will be called in super_main. This is how you override the default error and warning printers *)
let setup () =
  Location.error_reporter := super_error_reporter;
  Location.warning_printer := super_warning_printer;
