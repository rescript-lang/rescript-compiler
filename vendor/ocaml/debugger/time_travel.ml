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

(**************************** Time travel ******************************)

open Int64ops
open Instruct
open Events
open Debugcom
open Primitives
open Checkpoints
open Breakpoints
open Trap_barrier
open Input_handling
open Debugger_config
open Program_loading
open Question

exception Current_checkpoint_lost
exception Current_checkpoint_lost_start_at of int64 * int64

let remove_1st key list =
  let rec remove =
    function
      []   -> []
    | a::l -> if a == key then l else a::(remove l)
  in
    remove list

(*** Debugging. ***)

let debug_time_travel = ref false

(*** Internal utilities. ***)

(* Insert a checkpoint in the checkpoint list.
 * Raise `Exit' if there is already a checkpoint at the same time.
 *)
let insert_checkpoint ({c_time = time} as checkpoint) =
  let rec traverse =
    function
      [] -> [checkpoint]
    | (({c_time = t} as a)::l) as l' ->
        if t > time then
          a::(traverse l)
        else if t = time then
          raise Exit
        else
          checkpoint::l'
  in
    checkpoints := traverse !checkpoints

(* Remove a checkpoint from the checkpoint list.
 * --- No error if not found.
 *)
let remove_checkpoint checkpoint =
  checkpoints := remove_1st checkpoint !checkpoints

(* Wait for the process used by `checkpoint' to connect.
 * --- Usually not called (the process is already connected).
 *)
let wait_for_connection checkpoint =
  try
    Exec.unprotect
      (function () ->
         let old_controller = Input_handling.current_controller !connection in
           execute_with_other_controller
             (function
                fd ->
                  old_controller fd;
                  if checkpoint.c_valid = true then
                    exit_main_loop ())
             !connection
             main_loop)
  with
    Sys.Break ->
      checkpoint.c_parent <- root;
      remove_checkpoint checkpoint;
      checkpoint.c_pid <- -1;
      raise Sys.Break

(* Select a checkpoint as current. *)
let set_current_checkpoint checkpoint =
  if !debug_time_travel then
    prerr_endline ("Select: " ^ (string_of_int checkpoint.c_pid));
  if not checkpoint.c_valid then
    wait_for_connection checkpoint;
  current_checkpoint := checkpoint;
  set_current_connection checkpoint.c_fd

(* Kill `checkpoint'. *)
let kill_checkpoint checkpoint =
  if !debug_time_travel then
    prerr_endline ("Kill: " ^ (string_of_int checkpoint.c_pid));
  if checkpoint.c_pid > 0 then          (* Ghosts don't have to be killed ! *)
    (if not checkpoint.c_valid then
       wait_for_connection checkpoint;
     stop checkpoint.c_fd;
     if checkpoint.c_parent.c_pid > 0 then
       wait_child checkpoint.c_parent.c_fd;
     checkpoint.c_parent <- root;
     close_io checkpoint.c_fd;
     remove_file checkpoint.c_fd;
     remove_checkpoint checkpoint);
  checkpoint.c_pid <- -1                (* Don't exist anymore *)

(*** Cleaning the checkpoint list. ***)

(* Separe checkpoints before (<=) and after (>) `t'. *)
(* ### t checkpoints -> (after, before) *)
let cut t =
  let rec cut_t =
    function
      [] -> ([], [])
    | ({c_time = t'} as a::l) as l' ->
        if t' <= t then
          ([], l')
        else
          let (b, e) = cut_t l in
            (a::b, e)
  in
    cut_t

(* Partition the checkpoints list. *)
let cut2 t0 t l =
  let rec cut2_t0 t =
    function
      [] -> []
    | l ->
       let (after, before) = cut (t0 -- t -- _1) l in
         let l = cut2_t0 (t ++ t) before in
           after::l
  in
    let (after, before) = cut (t0 -- _1) l in
      after::(cut2_t0 t before)

(* Separe first elements and last element of a list of checkpoint. *)
let chk_merge2 cont =
  let rec chk_merge2_cont =
    function
      [] -> cont
    | [a] ->
        let (accepted, rejected) = cont in
          (a::accepted, rejected)
    | a::l ->
        let (accepted, rejected) = chk_merge2_cont l in
          (accepted, a::rejected)
  in chk_merge2_cont

(* Separe the checkpoint list. *)
(* ### list -> accepted * rejected *)
let rec chk_merge =
  function
    [] -> ([], [])
  | l::tail ->
       chk_merge2 (chk_merge tail) l

let new_checkpoint_list checkpoint_count accepted rejected =
  if List.length accepted >= checkpoint_count then
    let (k, l) = list_truncate2 checkpoint_count accepted in
      (k, l @ rejected)
  else
    let (k, l) =
      list_truncate2 (checkpoint_count - List.length accepted) rejected
    in
      (List.merge (fun {c_time = t1} {c_time = t2} -> compare t2 t1) accepted k,
       l)

(* Clean the checkpoint list. *)
(* Reference time is `time'. *)
let clean_checkpoints time checkpoint_count =
  let (after, before) = cut time !checkpoints in
    let (accepted, rejected) =
      chk_merge (cut2 time !checkpoint_small_step before)
    in
      let (kept, lost) =
        new_checkpoint_list checkpoint_count accepted after
      in
        List.iter kill_checkpoint (lost @ rejected);
        checkpoints := kept

(*** Internal functions for moving. ***)

(* Find the first checkpoint before (or at) `time'.
 * Ask for reloading the program if necessary.
 *)
let find_checkpoint_before time =
  let rec find =
    function
      [] ->
        print_string "Can't go that far in the past !"; print_newline ();
        if yes_or_no "Reload program" then begin
          load_program ();
          find !checkpoints
          end
        else
          raise Toplevel
    | { c_time = t } as a::l ->
        if t > time then
          find l
        else
          a
  in find !checkpoints

(* Make a copy of the current checkpoint and clean the checkpoint list. *)
(* --- The new checkpoint in not put in the list. *)
let duplicate_current_checkpoint () =
  let checkpoint = !current_checkpoint in
    if not checkpoint.c_valid then
      wait_for_connection checkpoint;
    let new_checkpoint =                        (* Ghost *)
      {c_time = checkpoint.c_time;
       c_pid = 0;
       c_fd = checkpoint.c_fd;
       c_valid = false;
       c_report = checkpoint.c_report;
       c_state = C_stopped;
       c_parent = checkpoint;
       c_breakpoint_version = checkpoint.c_breakpoint_version;
       c_breakpoints = checkpoint.c_breakpoints;
       c_trap_barrier = checkpoint.c_trap_barrier}
    in
      checkpoints := list_replace checkpoint new_checkpoint !checkpoints;
      set_current_checkpoint checkpoint;
      clean_checkpoints (checkpoint.c_time ++ _1) (!checkpoint_max_count - 1);
      if new_checkpoint.c_pid = 0 then  (* The ghost has not been killed *)
        (match do_checkpoint () with    (* Duplicate checkpoint *)
           Checkpoint_done pid ->
             (new_checkpoint.c_pid <- pid;
              if !debug_time_travel then
                prerr_endline ("Waiting for connection: " ^ string_of_int pid))
         | Checkpoint_failed ->
             prerr_endline
               "A fork failed. Reducing maximum number of checkpoints.";
             checkpoint_max_count := List.length !checkpoints - 1;
             remove_checkpoint new_checkpoint)

(* Was the movement interrupted ? *)
(* --- An exception could have been used instead, *)
(* --- but it is not clear where it should be caught. *)
(* --- For instance, it should not be caught in `step' *)
(* --- (as `step' is used in `next_1'). *)
(* --- On the other side, other modules does not need to know *)
(* --- about this exception. *)
let interrupted = ref false

(* Informations about last breakpoint encountered *)
let last_breakpoint = ref None

(* Ensure we stop on an event. *)
let rec stop_on_event report =
  match report with
    {rep_type = Breakpoint; rep_program_pointer = pc;
     rep_stack_pointer = sp} ->
      last_breakpoint := Some (pc, sp);
      Symbols.update_current_event ();
      begin match !current_event with
        None   -> find_event ()
      | Some _ -> ()
      end
  | {rep_type = Trap_barrier; rep_stack_pointer = trap_frame} ->
      (* No event at current position. *)
      find_event ()
  | _ ->
      ()

and find_event () =
  if !debug_time_travel then begin
    print_string "Searching next event...";
    print_newline ()
  end;
  let report = do_go _1 in
  !current_checkpoint.c_report <- Some report;
  stop_on_event report

(* Internal function for running debugged program.
 * Requires `duration > 0'.
 *)
let internal_step duration =
  match current_report () with
    Some {rep_type = Exited | Uncaught_exc} -> ()
  | _ ->
      Exec.protect
        (function () ->
           if !make_checkpoints then
             duplicate_current_checkpoint ()
           else
             remove_checkpoint !current_checkpoint;
           update_breakpoints ();
           update_trap_barrier ();
           !current_checkpoint.c_state <- C_running duration;
           let report = do_go duration in
             !current_checkpoint.c_report <- Some report;
             !current_checkpoint.c_state <- C_stopped;
             if report.rep_type = Event then begin
               !current_checkpoint.c_time <-
                 !current_checkpoint.c_time ++ duration;
               interrupted := false;
               last_breakpoint := None
               end
             else begin
               !current_checkpoint.c_time <-
                  !current_checkpoint.c_time ++ duration
                  -- (Int64.of_int report.rep_event_count) ++ _1;
               interrupted := true;
               last_breakpoint := None;
               stop_on_event report
               end;
             (try
                insert_checkpoint !current_checkpoint
              with
                Exit ->
                  kill_checkpoint !current_checkpoint;
                  set_current_checkpoint
                    (find_checkpoint_before (current_time ()))));
        if !debug_time_travel then begin
          print_string "Checkpoints: pid(time)"; print_newline ();
          List.iter
            (function {c_time = time; c_pid = pid; c_valid = valid} ->
              Printf.printf "%d(%Ld)%s " pid time
                            (if valid then "" else "(invalid)"))
            !checkpoints;
          print_newline ()
        end

(*** Miscellaneous functions (exported). ***)

(* Create a checkpoint at time 0 (new program). *)
let new_checkpoint pid fd =
  let new_checkpoint =
    {c_time = _0;
     c_pid = pid;
     c_fd = fd;
     c_valid = true;
     c_report = None;
     c_state = C_stopped;
     c_parent = root;
     c_breakpoint_version = 0;
     c_breakpoints = [];
     c_trap_barrier = 0}
  in
    insert_checkpoint new_checkpoint

(* Set the file descriptor of a checkpoint *)
(* (a new process has connected with the debugger). *)
(* --- Return `true' on success (close the connection otherwise). *)
let set_file_descriptor pid fd =
  let rec find =
    function
      [] ->
        prerr_endline "Unexpected connection";
        close_io fd;
        false
    | ({c_pid = pid'} as checkpoint)::l ->
        if pid <> pid' then
          find l
        else
          (checkpoint.c_fd <- fd;
           checkpoint.c_valid <- true;
           true)
  in
    if !debug_time_travel then
      prerr_endline ("New connection: " ^(string_of_int pid));
    find (!current_checkpoint::!checkpoints)

(* Kill all the checkpoints. *)
let kill_all_checkpoints () =
  List.iter kill_checkpoint (!current_checkpoint::!checkpoints)

(* Kill a checkpoint without killing the process. *)
(* (used when connection with the process is lost). *)
(* --- Assume that the checkpoint is valid. *)
let forget_process fd pid =
  let checkpoint =
    List.find (function c -> c.c_pid = pid) (!current_checkpoint::!checkpoints)
  in
    Printf.eprintf "Lost connection with process %d" pid;
    let kont =
      if checkpoint == !current_checkpoint then begin
        Printf.eprintf " (active process)\n";
        match !current_checkpoint.c_state with
          C_stopped ->
            Printf.eprintf "at time %Ld" !current_checkpoint.c_time;
            fun () -> raise Current_checkpoint_lost
        | C_running duration ->
            Printf.eprintf "between time %Ld and time %Ld"
                          !current_checkpoint.c_time
                          (!current_checkpoint.c_time ++ duration);
            fun () -> raise (Current_checkpoint_lost_start_at
                              (!current_checkpoint.c_time, duration))
        end
      else ignore in
    Printf.eprintf "\n"; flush stderr;
    Input_handling.remove_file fd;
    close_io checkpoint.c_fd;
    remove_file checkpoint.c_fd;
    remove_checkpoint checkpoint;
    checkpoint.c_pid <- -1;             (* Don't exist anymore *)
    if checkpoint.c_parent.c_pid > 0 then
      wait_child checkpoint.c_parent.c_fd;
    kont ()

(* Try to recover when the current checkpoint is lost. *)
let recover () =
  set_current_checkpoint
    (find_checkpoint_before (current_time ()))

(*** Simple movements. ***)

(* Forward stepping.  Requires `duration >= 0'. *)
let rec step_forward duration =
  if duration > !checkpoint_small_step then begin
    let first_step =
      if duration > !checkpoint_big_step then
        !checkpoint_big_step
      else
        !checkpoint_small_step
    in
      internal_step first_step;
      if not !interrupted then
        step_forward (duration -- first_step)
    end
  else if duration != _0 then
    internal_step duration

(* Go to time `time' from current checkpoint (internal). *)
let internal_go_to time =
  let duration = time -- (current_time ()) in
    if duration > _0 then
      execute_without_breakpoints (function () -> step_forward duration)

(* Move to a given time. *)
let go_to time =
  let checkpoint = find_checkpoint_before time in
    set_current_checkpoint checkpoint;
    internal_go_to time

(* Return the time of the last breakpoint *)
(* between current time and `max_time'. *)
let rec find_last_breakpoint max_time =
  let rec find break =
    let time = current_time () in
    step_forward (max_time -- time);
    match !last_breakpoint, !temporary_breakpoint_position with
      (Some _, _) when current_time () < max_time ->
        find !last_breakpoint
    | (Some (pc, _), Some pc') when pc = pc' ->
        (max_time, !last_breakpoint)
    | _ ->
        (time, break)
  in
    find
      (match current_pc_sp () with
         (Some (pc, _)) as state when breakpoint_at_pc pc -> state
       | _                                                -> None)


(* Run from `time_max' back to `time'. *)
(* --- Assume 0 <= time < time_max *)
let rec back_to time time_max =
  let
    {c_time = t} = find_checkpoint_before (pre64 time_max)
  in
    go_to (max time t);
    let (new_time, break) = find_last_breakpoint time_max in
    if break <> None || (new_time <= time) then begin
      go_to new_time;
      interrupted := break <> None;
      last_breakpoint := break
    end else
      back_to time new_time

(* Backward stepping. *)
(* --- Assume duration > 1 *)
let step_backward duration =
  let time = current_time () in
    if time > _0 then
      back_to (max _0 (time -- duration)) time

(* Run the program from current time. *)
(* Stop at the first breakpoint, or at the end of the program. *)
let rec run () =
  internal_step !checkpoint_big_step;
  if not !interrupted then
    run ()

(* Run backward the program form current time. *)
(* Stop at the first breakpoint, or at the beginning of the program. *)
let back_run () =
  if current_time () > _0 then
    back_to _0 (current_time ())

(* Step in any direction. *)
(* Stop at the first brakpoint, or after `duration' steps. *)
let step duration =
  if duration >= _0 then
    step_forward duration
  else
    step_backward (_0 -- duration)

(*** Next, finish. ***)

(* Finish current function. *)
let finish () =
  Symbols.update_current_event ();
  match !current_event with
    None ->
      prerr_endline "`finish' not meaningful in outermost frame.";
      raise Toplevel
  | Some curr_event ->
      set_initial_frame();
      let (frame, pc) = up_frame curr_event.ev_stacksize in
      if frame < 0 then begin
        prerr_endline "`finish' not meaningful in outermost frame.";
        raise Toplevel
      end;
      begin
        try ignore(Symbols.any_event_at_pc pc)
        with Not_found ->
               prerr_endline "Calling function has no debugging information.";
               raise Toplevel
      end;
      exec_with_trap_barrier
        frame
        (fun () ->
           exec_with_temporary_breakpoint
             pc
             (fun () ->
                while
                  run ();
                  match !last_breakpoint with
                    Some (pc', frame') when pc = pc' ->
                      interrupted := false;
                      frame <> frame'
                  | _ ->
                      false
                do
                  ()
                done))

let next_1 () =
  Symbols.update_current_event ();
  match !current_event with
    None ->                             (* Beginning of the program. *)
      step _1
  | Some event1 ->
      let (frame1, pc1) = initial_frame() in
      step _1;
      if not !interrupted then begin
        Symbols.update_current_event ();
        match !current_event with
          None -> ()
        | Some event2 ->
            let (frame2, pc2) = initial_frame() in
            (* Call `finish' if we've entered a function. *)
            if frame1 >= 0 && frame2 >= 0 &&
               frame2 - event2.ev_stacksize > frame1 - event1.ev_stacksize
            then finish()
      end

(* Same as `step' (forward) but skip over function calls. *)
let rec next =
  function
    0 -> ()
  | n ->
      next_1 ();
      if not !interrupted then
        next (n - 1)

(* Run backward until just before current function. *)
let start () =
  Symbols.update_current_event ();
  match !current_event with
    None ->
      prerr_endline "`start not meaningful in outermost frame.";
      raise Toplevel
  | Some curr_event ->
      let (frame, _) = initial_frame() in
      let (frame', pc) = up_frame curr_event.ev_stacksize in
      if frame' < 0 then begin
        prerr_endline "`start not meaningful in outermost frame.";
        raise Toplevel
      end;
      let nargs =
        match
          try Symbols.any_event_at_pc pc with Not_found ->
            prerr_endline "Calling function has no debugging information.";
            raise Toplevel
        with
          {ev_info = Event_return nargs} -> nargs
        | _ ->  Misc.fatal_error "Time_travel.start"
      in
      let offset = if nargs < 4 then 1 else 2 in
      let pc = pc - 4 * offset in
      while
        exec_with_temporary_breakpoint pc back_run;
        match !last_breakpoint with
          Some (pc', frame') when pc = pc' ->
            step _minus1;
            (not !interrupted)
              &&
            (frame' - nargs > frame - curr_event.ev_stacksize)
        | _ ->
            false
      do
        ()
      done

let previous_1 () =
  Symbols.update_current_event ();
  match !current_event with
    None ->                             (* End of the program. *)
      step _minus1
  | Some event1 ->
      let (frame1, pc1) = initial_frame() in
      step _minus1;
      if not !interrupted then begin
        Symbols.update_current_event ();
        match !current_event with
          None -> ()
        | Some event2 ->
            let (frame2, pc2) = initial_frame() in
            (* Call `start' if we've entered a function. *)
            if frame1 >= 0 && frame2 >= 0 &&
               frame2 - event2.ev_stacksize > frame1 - event1.ev_stacksize
            then start()
      end

(* Same as `step' (backward) but skip over function calls. *)
let rec previous =
  function
    0 -> ()
  | n ->
      previous_1 ();
      if not !interrupted then
        previous (n - 1)
