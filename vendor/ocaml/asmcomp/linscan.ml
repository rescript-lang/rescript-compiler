(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Marcell Fischbach, University of Siegen             *)
(*                     Benedikt Meurer, University of Siegen              *)
(*                                                                        *)
(*   Copyright 2011 Lehrstuhl für Compilerbau und Softwareanalyse,        *)
(*     Universität Siegen.                                                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Linear scan register allocation. *)

open Interval
open Reg

(* Live intervals per register class *)

type class_intervals =
  {
    mutable ci_fixed: Interval.t list;
    mutable ci_active: Interval.t list;
    mutable ci_inactive: Interval.t list;
  }

let active = Array.init Proc.num_register_classes (fun _ -> {
  ci_fixed = [];
  ci_active = [];
  ci_inactive = []
})

(* Insert interval into list sorted by end position *)

let rec insert_interval_sorted i = function
    [] -> [i]
  | j :: _ as il when j.iend <= i.iend -> i :: il
  | j :: il -> j :: insert_interval_sorted i il

let rec release_expired_fixed pos = function
    i :: il when i.iend >= pos ->
      Interval.remove_expired_ranges i pos;
      i :: release_expired_fixed pos il
  | _ -> []

let rec release_expired_active ci pos = function
    i :: il when i.iend >= pos ->
      Interval.remove_expired_ranges i pos;
      if Interval.is_live i pos then
        i :: release_expired_active ci pos il
      else begin
        ci.ci_inactive <- insert_interval_sorted i ci.ci_inactive;
        release_expired_active ci pos il
      end
  | _ -> []

let rec release_expired_inactive ci pos = function
    i :: il when i.iend >= pos ->
      Interval.remove_expired_ranges i pos;
      if not (Interval.is_live i pos) then
        i :: release_expired_inactive ci pos il
      else begin
        ci.ci_active <- insert_interval_sorted i ci.ci_active;
        release_expired_inactive ci pos il
      end
  | _ -> []

(* Allocate a new stack slot to the interval. *)

let allocate_stack_slot i =
  let cl = Proc.register_class i.reg in
  let ss = Proc.num_stack_slots.(cl) in
  Proc.num_stack_slots.(cl) <- succ ss;
  i.reg.loc <- Stack(Local ss);
  i.reg.spill <- true

(* Find a register for the given interval and assigns this register.
   The interval is added to active. Raises Not_found if no free registers
   left. *)

let allocate_free_register i =
  begin match i.reg.loc, i.reg.spill with
    Unknown, true ->
      (* Allocate a stack slot for the already spilled interval *)
      allocate_stack_slot i
  | Unknown, _ ->
      (* We need to allocate a register to this interval somehow *)
      let cl = Proc.register_class i.reg in
      begin match Proc.num_available_registers.(cl) with
        0 ->
          (* There are no registers available for this class *)
          raise Not_found
      | rn ->
          let ci = active.(cl) in
          let r0 = Proc.first_available_register.(cl) in
          (* Create register mask for this class
             note: if frame pointers are enabled then some registers may have
                   indexes that are off-bounds; we hence protect write accesses
                   below (given that the assign function will not consider such
                   registers) *)
          let regmask = Array.make rn true in
          (* Remove all assigned registers from the register mask *)
          List.iter
            (function
              {reg = {loc = Reg r}} ->
                if r - r0 < rn then regmask.(r - r0) <- false
            | _ -> ())
            ci.ci_active;
          (* Remove all overlapping registers from the register mask *)
          let remove_bound_overlapping = function
              {reg = {loc = Reg r}} as j ->
                if (r - r0 < rn) && regmask.(r - r0) && Interval.overlap j i then
                regmask.(r - r0) <- false
            | _ -> () in
          List.iter remove_bound_overlapping ci.ci_inactive;
          List.iter remove_bound_overlapping ci.ci_fixed;
          (* Assign the first free register (if any) *)
          let rec assign r =
            if r = rn then
              raise Not_found
            else if regmask.(r) then begin
              (* Assign the free register and insert the
                 current interval into the active list *)
              i.reg.loc <- Reg (r0 + r);
              i.reg.spill <- false;
              ci.ci_active <- insert_interval_sorted i ci.ci_active
            end else
              assign (succ r) in
          assign 0
      end
  | _ -> ()
  end

let allocate_blocked_register i =
  let cl = Proc.register_class i.reg in
  let ci = active.(cl) in
  match ci.ci_active with
  | ilast :: il when
      ilast.iend > i.iend &&
      (* Last interval in active is the last interval, so spill it. *)
      let chk r = r.reg.loc = ilast.reg.loc && Interval.overlap r i in
      (* But only if its physical register is admissible for the current
         interval. *)
      not (List.exists chk ci.ci_fixed || List.exists chk ci.ci_inactive)
    ->
      begin match ilast.reg.loc with Reg _ -> () | _ -> assert false end;
      (* Use register from last interval for current interval *)
      i.reg.loc <- ilast.reg.loc;
      (* Remove the last interval from active and insert the current *)
      ci.ci_active <- insert_interval_sorted i il;
      (* Now get a new stack slot for the spilled register *)
      allocate_stack_slot ilast
  | _ ->
      (* Either the current interval is last and we have to spill it,
         or there are no registers at all in the register class (i.e.
         floating point class on i386). *)
      allocate_stack_slot i

let walk_interval i =
  let pos = i.ibegin land (lnot 0x01) in
  (* Release all intervals that have been expired at the current position *)
  Array.iter
    (fun ci ->
      ci.ci_fixed <- release_expired_fixed pos ci.ci_fixed;
      ci.ci_active <- release_expired_active ci pos ci.ci_active;
      ci.ci_inactive <- release_expired_inactive ci pos ci.ci_inactive)
    active;
  try
    (* Allocate free register (if any) *)
    allocate_free_register i
  with
    Not_found ->
      (* No free register, need to decide which interval to spill *)
      allocate_blocked_register i

let allocate_registers() =
  (* Initialize the stack slots and interval lists *)
  for cl = 0 to Proc.num_register_classes - 1 do
    (* Start with empty interval lists *)
    active.(cl) <- {
      ci_fixed = [];
      ci_active = [];
      ci_inactive = []
    };
    Proc.num_stack_slots.(cl) <- 0
  done;
  (* Add all fixed intervals (sorted by end position) *)
  List.iter
    (fun i ->
      let ci = active.(Proc.register_class i.reg) in
      ci.ci_fixed <- insert_interval_sorted i ci.ci_fixed)
    (Interval.all_fixed_intervals());
  (* Walk all the intervals within the list *)
  List.iter walk_interval (Interval.all_intervals())
