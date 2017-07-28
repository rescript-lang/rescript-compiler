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

(***************************** Frames **********************************)

open Instruct
open Debugcom
open Events
open Symbols

(* Current frame number *)
let current_frame = ref 0

(* Event at selected position *)
let selected_event = ref (None : debug_event option)

(* Selected position in source. *)
(* Raise `Not_found' if not on an event. *)
let selected_point () =
  match !selected_event with
    None ->
      raise Not_found
  | Some ev ->
      (ev.ev_module,
       (Events.get_pos ev).Lexing.pos_lnum,
       (Events.get_pos ev).Lexing.pos_cnum - (Events.get_pos ev).Lexing.pos_bol)

let selected_event_is_before () =
  match !selected_event with
    None ->
      raise Not_found
  | Some {ev_kind = Event_before} ->
      true
  | _ ->
      false

(* Move up `frame_count' frames, assuming current frame pointer
   corresponds to event `event'. Return event of final frame. *)

let rec move_up frame_count event =
  if frame_count <= 0 then event else begin
    let (sp, pc) = up_frame event.ev_stacksize in
    if sp < 0 then raise Not_found;
    move_up (frame_count - 1) (any_event_at_pc pc)
  end

(* Select a frame. *)
(* Raise `Not_found' if no such frame. *)
(* --- Assume the current events have already been updated. *)
let select_frame frame_number =
  if frame_number < 0 then raise Not_found;
  let (initial_sp, _) = get_frame() in
  try
    match !current_event with
      None ->
        raise Not_found
    | Some curr_event ->
        match !selected_event with
          Some sel_event when frame_number >= !current_frame ->
            selected_event :=
              Some(move_up (frame_number - !current_frame) sel_event);
            current_frame := frame_number
        | _ ->
            set_initial_frame();
            selected_event := Some(move_up frame_number curr_event);
            current_frame := frame_number
  with Not_found ->
    set_frame initial_sp;
    raise Not_found

(* Select a frame. *)
(* Same as `select_frame' but raise no exception if the frame is not found. *)
(* --- Assume the currents events have already been updated. *)
let try_select_frame frame_number =
  try
    select_frame frame_number
  with
    Not_found ->
      ()

(* Return to default frame (frame 0). *)
let reset_frame () =
  set_initial_frame();
  selected_event := !current_event;
  current_frame := 0

(* Perform a stack backtrace.
   Call the given function with the events for each stack frame,
   or None if we've encountered a stack frame with no debugging info
   attached. Stop when the function returns false, or frame with no
   debugging info reached, or top of stack reached. *)

let do_backtrace action =
  match !current_event with
    None -> Misc.fatal_error "Frames.do_backtrace"
  | Some curr_ev ->
      let (initial_sp, _) = get_frame() in
      set_initial_frame();
      let event = ref curr_ev in
      begin try
        while action (Some !event) do
          let (sp, pc) = up_frame !event.ev_stacksize in
          if sp < 0 then raise Exit;
          event := any_event_at_pc pc
        done
      with Exit -> ()
         | Not_found -> ignore (action None)
      end;
      set_frame initial_sp

(* Return the number of frames in the stack *)

let stack_depth () =
  let num_frames = ref 0 in
  do_backtrace (function Some ev -> incr num_frames; true
                       | None -> num_frames := -1; false);
  !num_frames
