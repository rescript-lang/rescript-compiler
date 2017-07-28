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

(******************************* Breakpoints ***************************)

open Checkpoints
open Debugcom
open Instruct
open Primitives
open Printf

(*** Debugging. ***)
let debug_breakpoints = ref false

(*** Data. ***)

(* Number of the last added breakpoint. *)
let breakpoint_number = ref 0

(* Breakpoint number -> event. *)
let breakpoints = ref ([] : (int * debug_event) list)

(* Program counter -> breakpoint count. *)
let positions = ref ([] : (int * int ref) list)

(* Versions of the breakpoint list. *)
let current_version = ref 0
let max_version = ref 0

(*** Miscellaneous. ***)

(* Mark breakpoints as installed in current checkpoint. *)
let copy_breakpoints () =
  !current_checkpoint.c_breakpoints <- !positions;
  !current_checkpoint.c_breakpoint_version <- !current_version

(* Announce a new version of the breakpoint list. *)
let new_version () =
  incr max_version;
  current_version := !max_version

(*** Information about breakpoints. ***)

let breakpoints_count () =
  List.length !breakpoints

(* List of breakpoints at `pc'. *)
let rec breakpoints_at_pc pc =
  begin try
    let ev = Symbols.event_at_pc pc in
    match ev.ev_repr with
      Event_child {contents = pc'} -> breakpoints_at_pc pc'
    | _                            -> []
  with Not_found ->
   []
  end
    @
  List.map fst (List.filter (function (_, {ev_pos = pos}) -> pos = pc)
                            !breakpoints)

(* Is there a breakpoint at `pc' ? *)
let breakpoint_at_pc pc =
  breakpoints_at_pc pc <> []

(*** Set and remove breakpoints ***)

(* Remove all breakpoints. *)
let remove_breakpoints pos =
  if !debug_breakpoints then
    (print_string "Removing breakpoints..."; print_newline ());
  List.iter
    (function (pos, _) ->
       if !debug_breakpoints then begin
         print_int pos;
         print_newline()
       end;
       reset_instr pos;
       Symbols.set_event_at_pc pos)
    pos

(* Set all breakpoints. *)
let set_breakpoints pos =
  if !debug_breakpoints then
    (print_string "Setting breakpoints..."; print_newline ());
  List.iter
    (function (pos, _) ->
       if !debug_breakpoints then begin
         print_int pos;
         print_newline()
       end;
       set_breakpoint pos)
    pos

(* Ensure the current version in installed in current checkpoint. *)
let update_breakpoints () =
  if !debug_breakpoints then begin
    prerr_string "Updating breakpoints... ";
    prerr_int !current_checkpoint.c_breakpoint_version;
    prerr_string " ";
    prerr_int !current_version;
    prerr_endline ""
  end;
  if !current_checkpoint.c_breakpoint_version <> !current_version then
    Exec.protect
      (function () ->
         remove_breakpoints !current_checkpoint.c_breakpoints;
         set_breakpoints !positions;
         copy_breakpoints ())

let change_version version pos =
  Exec.protect
    (function () ->
       current_version := version;
       positions := pos)

(* Execute given function with no breakpoint in current checkpoint. *)
(* --- `goto' runs faster this way (does not stop on each breakpoint). *)
let execute_without_breakpoints f =
  let version = !current_version
  and pos = !positions
  in
    change_version 0 [];
    try
      f ();
      change_version version pos
    with
      x ->
        change_version version pos

(* Add a position in the position list. *)
(* Change version if necessary. *)
let insert_position pos =
  try
    incr (List.assoc pos !positions)
  with
    Not_found ->
      positions := (pos, ref 1) :: !positions;
      new_version ()

(* Remove a position in the position list. *)
(* Change version if necessary. *)
let remove_position pos =
  let count = List.assoc pos !positions in
    decr count;
    if !count = 0 then begin
      positions := List.remove_assoc pos !positions;
      new_version ()
    end

(* Insert a new breakpoint in lists. *)
let rec new_breakpoint =
  function
    {ev_repr = Event_child pc} ->
      new_breakpoint (Symbols.any_event_at_pc !pc)
  | event ->
      Exec.protect
        (function () ->
           incr breakpoint_number;
           insert_position event.ev_pos;
           breakpoints := (!breakpoint_number, event) :: !breakpoints);
      printf "Breakpoint %d at %d: %s" !breakpoint_number event.ev_pos
             (Pos.get_desc event);
      print_newline ()

(* Remove a breakpoint from lists. *)
let remove_breakpoint number =
  try
    let ev = List.assoc number !breakpoints in
    let pos = ev.ev_pos in
      Exec.protect
        (function () ->
           breakpoints := List.remove_assoc number !breakpoints;
           remove_position pos;
           printf "Removed breakpoint %d at %d: %s" number ev.ev_pos
                  (Pos.get_desc ev);
           print_newline ()
        )
  with
    Not_found ->
      prerr_endline ("No breakpoint number " ^ (string_of_int number) ^ ".");
      raise Not_found

let remove_all_breakpoints () =
  List.iter (function (number, _) -> remove_breakpoint number) !breakpoints

(*** Temporary breakpoints. ***)

(* Temporary breakpoint position. *)
let temporary_breakpoint_position = ref (None : int option)

(* Execute `funct' with a breakpoint added at `pc'. *)
(* --- Used by `finish'. *)
let exec_with_temporary_breakpoint pc funct =
  let previous_version = !current_version in
    let remove () =
      temporary_breakpoint_position := None;
      current_version := previous_version;
      let count = List.assoc pc !positions in
        decr count;
        if !count = 0 then begin
          positions := List.remove_assoc pc !positions;
          reset_instr pc;
          Symbols.set_event_at_pc pc
        end

    in
      Exec.protect (function () -> insert_position pc);
      temporary_breakpoint_position := Some pc;
      try
        funct ();
        Exec.protect remove
      with
        x ->
          Exec.protect remove;
          raise x
