(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Damien Doligez, EPI Gallium, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* WARNING: this file MUST NOT be merged into the trunk, it is for the
   4.02 branch only, to test compatibility with the old implementation
   of printf. Starting with 4.03, all these formats will be statically
   refused by the compiler.
*)

open Printf;;

let show arg f =
  printf "%8s --> |" (string_of_format f);
  printf f arg;
  printf "|\n";
in

  List.iter (show 12.3) [
    "%0.3f";
    "%0.f";
    "%+ .3f";
    "%.f";
    "%3.f";
    "%-9.f";
    "%0.16g";
  ];

  List.iter (show "ab") [
    "%.30s";
    "%-.30s";
    "%-s";
    "%0s";
    "%03s";
    "% s";
  ];

  List.iter (show 4) [
    "%0.3d";
    "%0X";
    "%0x";
  ];

  List.iter (show 'a') [
    "%5c";
  ];
;;

let fmt = format_of_string "%0.*f" in
printf "%8s --> |" (string_of_format fmt);
printf fmt 3 12.3;
printf "|\n";
;;

let fmt = format_of_string "%.2%" in
printf "%8s --> |" (string_of_format fmt);
printf fmt;
printf "|\n";
;;
