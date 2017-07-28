(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          OCaml port by John Malecki and Xavier Leroy                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Debugger_config
open Instruct
open Parameters
open Primitives
open Printf
open Source

(* Print a line; return the beginning of the next line *)
let print_line buffer line_number start point before =
  let next = next_linefeed buffer start
  and content = buffer_content buffer
  in
    printf "%i " line_number;
    if point <= next && point >= start then
      (print_string (String.sub content start (point - start));
       print_string (if before then event_mark_before else event_mark_after);
       print_string (String.sub content point (next - point)))
    else
      print_string (String.sub content start (next - start));
    print_newline ();
    next

(* Tell Emacs we are nowhere in the source. *)
let show_no_point () =
  if !emacs then printf "\026\026H\n"

(* Print the line containing the point *)
let show_point ev selected =
  let mdle = ev.ev_module in
  let before = (ev.ev_kind = Event_before) in
  if !emacs && selected then
    begin try
      let buffer = get_buffer (Events.get_pos ev) mdle in
      let source = source_of_module ev.ev_loc.Location.loc_start mdle in
      printf "\026\026M%s:%i:%i" source
        (snd (start_and_cnum buffer ev.ev_loc.Location.loc_start))
        (snd (start_and_cnum buffer ev.ev_loc.Location.loc_end));
      printf "%s\n" (if before then ":before" else ":after")
    with
      Out_of_range -> (* point_of_coord *)
        prerr_endline "Position out of range."
    | Not_found    -> (* Events.get_pos || get_buffer *)
        prerr_endline ("No source file for " ^ mdle ^ ".");
        show_no_point ()
    end
  else
    begin try
      let pos = Events.get_pos ev in
      let buffer = get_buffer pos mdle in
      let start, point = start_and_cnum buffer pos in
      ignore(print_line buffer pos.Lexing.pos_lnum start point before)
    with
      Out_of_range -> (* point_of_coord *)
        prerr_endline "Position out of range."
    | Not_found    -> (* Events.get_pos || get_buffer *)
        prerr_endline ("No source file for " ^ mdle ^ ".")
    end

(* Display part of the source. *)
let show_listing pos mdle start stop point before =
  try
    let buffer = get_buffer pos mdle in
      let rec aff (line_start, line_number) =
        if line_number <= stop then
          aff (print_line buffer line_number line_start point before + 1,
               line_number + 1)
      in
        aff (pos_of_line buffer start)
  with
    Out_of_range -> (* pos_of_line *)
      prerr_endline "Position out of range."
  | Not_found    -> (* get_buffer *)
      prerr_endline ("No source file for " ^ mdle ^ ".")
