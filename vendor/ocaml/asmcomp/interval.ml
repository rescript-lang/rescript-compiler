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

(* Live intervals for the linear scan register allocator. *)

open Mach
open Reg

type range =
  {
    mutable rbegin: int;
    mutable rend: int;
  }

type t =
  {
    mutable reg: Reg.t;
    mutable ibegin: int;
    mutable iend: int;
    mutable ranges: range list;
  }

type kind =
    Result
  | Argument
  | Live

let interval_list = ref ([] : t list)
let fixed_interval_list = ref ([] : t list)
let all_intervals() = !interval_list
let all_fixed_intervals() = !fixed_interval_list

(* Check if two intervals overlap *)

let overlap i0 i1 =
  let rec overlap_ranges rl0 rl1 =
    match rl0, rl1 with
      r0 :: rl0', r1 :: rl1' ->
        if r0.rend >= r1.rbegin && r1.rend >= r0.rbegin then true
        else if r0.rend < r1.rend then overlap_ranges rl0' rl1
        else if r0.rend > r1.rend then overlap_ranges rl0 rl1'
        else overlap_ranges rl0' rl1'
    | _ -> false in
  overlap_ranges i0.ranges i1.ranges

let is_live i pos =
  let rec is_live_in_ranges = function
    [] -> false
  | r :: rl -> if pos < r.rbegin then false
               else if pos <= r.rend then true
               else is_live_in_ranges rl in
  is_live_in_ranges i.ranges

let remove_expired_ranges i pos =
  let rec filter = function
    [] -> []
  | r :: rl' as rl -> if pos < r.rend then rl
               else filter rl' in
  i.ranges <- filter i.ranges

let update_interval_position intervals pos kind reg =
  let i = intervals.(reg.stamp) in
  let on = pos lsl 1 in
  let off = on + 1 in
  let rbegin = (match kind with Result -> off | _ -> on) in
  let rend = (match kind with Argument -> on | _ -> off) in
  if i.iend = 0 then begin
    i.ibegin <- rbegin;
    i.reg <- reg;
    i.ranges <- [{rbegin = rbegin; rend = rend}]
  end else begin
    let r = List.hd i.ranges in
    let ridx = r.rend asr 1 in
    if pos - ridx <= 1 then
      r.rend <- rend
    else
      i.ranges <- {rbegin = rbegin; rend = rend} :: i.ranges
  end;
  i.iend <- rend

let update_interval_position_by_array intervals regs pos kind =
  Array.iter (update_interval_position intervals pos kind) regs

let update_interval_position_by_set intervals regs pos kind =
  Set.iter (update_interval_position intervals pos kind) regs

let update_interval_position_by_instr intervals instr pos =
  update_interval_position_by_array intervals instr.arg pos Argument;
  update_interval_position_by_array intervals instr.res pos Result;
  update_interval_position_by_set intervals instr.live pos Live

let insert_destroyed_at_oper intervals instr pos =
  let destroyed = Proc.destroyed_at_oper instr.desc in
  if Array.length destroyed > 0 then
    update_interval_position_by_array intervals destroyed pos Result

let insert_destroyed_at_raise intervals pos =
  let destroyed = Proc.destroyed_at_raise in
  if Array.length destroyed > 0 then
    update_interval_position_by_array intervals destroyed pos Result

(* Build all intervals.
   The intervals will be expanded by one step at the start and end
   of a basic block. *)

let build_intervals fd =
  let intervals = Array.init
                    (Reg.num_registers())
                    (fun _ -> {
                      reg = Reg.dummy;
                      ibegin = 0;
                      iend = 0;
                      ranges = []; }) in
  let pos = ref 0 in
  let rec walk_instruction i =
    incr pos;
    update_interval_position_by_instr intervals i !pos;
    begin match i.desc with
      Iend -> ()
    | Iop(Icall_ind _ | Icall_imm _ | Iextcall{alloc = true; _}
          | Itailcall_ind _ | Itailcall_imm _) ->
        walk_instruction i.next
    | Iop _ ->
        insert_destroyed_at_oper intervals i !pos;
        walk_instruction i.next
    | Ireturn ->
        insert_destroyed_at_oper intervals i !pos;
        walk_instruction i.next
    | Iifthenelse(_, ifso, ifnot) ->
        insert_destroyed_at_oper intervals i !pos;
        walk_instruction ifso;
        walk_instruction ifnot;
        walk_instruction i.next
    | Iswitch(_, cases) ->
        insert_destroyed_at_oper intervals i !pos;
        Array.iter walk_instruction cases;
        walk_instruction i.next
    | Iloop body ->
        insert_destroyed_at_oper intervals i !pos;
        walk_instruction body;
        walk_instruction i.next
    | Icatch(_, handlers, body) ->
        insert_destroyed_at_oper intervals i !pos;
        List.iter (fun (_, i) -> walk_instruction i) handlers;
        walk_instruction body;
        walk_instruction i.next
    | Iexit _ ->
        insert_destroyed_at_oper intervals i !pos;
        walk_instruction i.next
    | Itrywith(body, handler) ->
        insert_destroyed_at_oper intervals i !pos;
        walk_instruction body;
        insert_destroyed_at_raise intervals !pos;
        walk_instruction handler;
        walk_instruction i.next
    | Iraise _ ->
        walk_instruction i.next
    end in
  walk_instruction fd.fun_body;
  (* Generate the interval and fixed interval lists *)
  interval_list := [];
  fixed_interval_list := [];
  Array.iter
    (fun i ->
      if i.iend != 0 then begin
        i.ranges <- List.rev i.ranges;
        begin match i.reg.loc with
          Reg _ ->
            fixed_interval_list := i :: !fixed_interval_list
        | _ ->
            interval_list := i :: !interval_list
        end
      end)
    intervals;
  (* Sort the intervals according to their start position *)
  interval_list := List.sort (fun i0 i1 -> i0.ibegin - i1.ibegin) !interval_list
