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

open Instruct
open Format
open Debugcom
open Checkpoints
open Events
open Symbols
open Frames
open Source
open Show_source
open Breakpoints
open Parameters

(* Display information about the current event. *)
let show_current_event ppf =
  fprintf ppf "Time: %Li" (current_time ());
  (match current_pc () with
   | Some pc ->
       fprintf ppf " - pc: %i" pc
   | _ -> ());
  update_current_event ();
  reset_frame ();
  match current_report ()  with
  | None ->
      fprintf ppf "@.Beginning of program.@.";
      show_no_point ()
  | Some {rep_type = (Event | Breakpoint); rep_program_pointer = pc} ->
        let ev = get_current_event () in
        fprintf ppf " - module %s@." ev.ev_module;
        (match breakpoints_at_pc pc with
         | [] ->
             ()
         | [breakpoint] ->
             fprintf ppf "Breakpoint: %i@." breakpoint
         | breakpoints ->
             fprintf ppf "Breakpoints: %a@."
             (fun ppf l ->
               List.iter
                (function x -> fprintf ppf "%i " x) l)
             (List.sort compare breakpoints));
        show_point ev true
  | Some {rep_type = Exited} ->
      fprintf ppf "@.Program exit.@.";
      show_no_point ()
  | Some {rep_type = Uncaught_exc} ->
      fprintf ppf
        "@.Program end.@.\
         @[Uncaught exception:@ %a@]@."
      Printval.print_exception (Debugcom.Remote_value.accu ());
      show_no_point ()
  | Some {rep_type = Trap_barrier} ->
                                        (* Trap_barrier not visible outside *)
                                        (* of module `time_travel'. *)
      Misc.fatal_error "Show_information.show_current_event"

(* Display short information about one frame. *)

let show_one_frame framenum ppf event =
  let pos = Events.get_pos event in
  let cnum =
    try
      let buffer = get_buffer pos event.ev_module in
      snd (start_and_cnum buffer pos)
    with _ -> pos.Lexing.pos_cnum in
  if !machine_readable then
    fprintf ppf "#%i  Pc: %i  %s char %i@."
           framenum event.ev_pos event.ev_module
           cnum
  else
    fprintf ppf "#%i %s %s:%i:%i@."
           framenum event.ev_module
           pos.Lexing.pos_fname pos.Lexing.pos_lnum
           (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

(* Display information about the current frame. *)
(* --- `select frame' must have succeded before calling this function. *)
let show_current_frame ppf selected =
  match !selected_event with
  | None ->
      fprintf ppf "@.No frame selected.@."
  | Some sel_ev ->
      show_one_frame !current_frame ppf sel_ev;
      begin match breakpoints_at_pc sel_ev.ev_pos with
      | [] -> ()
      | [breakpoint] ->
          fprintf ppf "Breakpoint: %i@." breakpoint
      | breakpoints ->
          fprintf ppf "Breakpoints: %a@."
          (fun ppf l ->
            List.iter (function x -> fprintf ppf "%i " x) l)
          (List.sort compare breakpoints);
      end;
      show_point sel_ev selected
