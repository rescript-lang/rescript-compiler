(* $Id$ *)

open Types

let ignore_abbrevs ppf ab =
  let s = match ab with
    Mnil -> "Mnil"
  | Mlink _ -> "Mlink _"
  | Mcons _ -> "Mcons _"
  in
  Format.pp_print_string ppf s
