(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Lexing
open Location

type kind = Dinfo_call | Dinfo_raise

type t = {
  dinfo_kind: kind;
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int
}

let none = {
  dinfo_kind = Dinfo_call;
  dinfo_file = "";
  dinfo_line = 0;
  dinfo_char_start = 0;
  dinfo_char_end = 0
}

(* PR#5643: cannot use (==) because Debuginfo values are marshalled *)
let is_none t =
  t = none

let to_string d =
  if d = none
  then ""
  else Printf.sprintf "{%s:%d,%d-%d}"
           d.dinfo_file d.dinfo_line d.dinfo_char_start d.dinfo_char_end

let from_location kind loc =
  if loc == Location.none then none else
  { dinfo_kind = kind;
    dinfo_file = loc.loc_start.pos_fname;
    dinfo_line = loc.loc_start.pos_lnum;
    dinfo_char_start = loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    dinfo_char_end =
      if loc.loc_end.pos_fname = loc.loc_start.pos_fname
      then loc.loc_end.pos_cnum - loc.loc_start.pos_bol
      else loc.loc_start.pos_cnum - loc.loc_start.pos_bol }

let from_call ev = from_location Dinfo_call ev.Lambda.lev_loc
let from_raise ev = from_location Dinfo_raise ev.Lambda.lev_loc
