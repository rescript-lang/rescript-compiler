(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Insertion of moves to suggest possible spilling / reloading points
   before register allocation. *)

open Reg
open Mach

(* We say that a register is "destroyed" if it is live across a construct
   that potentially destroys all physical registers: function calls or
   try...with constructs.

   The "destroyed" registers must therefore reside in the stack during
   these instructions.. We will insert spills (stores) just after they
   are defined, and reloads just before their first use following a
   "destroying" construct.

   Instructions with more live registers than actual registers also
   "destroy" registers: we mark as "destroyed" the registers live
   across the instruction that haven't been used for the longest time.
   These registers will be spilled and reloaded as described above. *)

(* Association of spill registers to registers *)

let spill_env = ref (Reg.Map.empty : Reg.t Reg.Map.t)

let spill_reg r =
  try
    Reg.Map.find r !spill_env
  with Not_found ->
    let spill_r = Reg.create r.typ in
    spill_r.spill <- true;
    if not (Reg.anonymous r) then spill_r.raw_name <- r.raw_name;
    spill_env := Reg.Map.add r spill_r !spill_env;
    spill_r

(* Record the position of last use of registers *)

let use_date = ref (Reg.Map.empty : int Reg.Map.t)
let current_date = ref 0

let record_use regv =
  for i = 0 to Array.length regv - 1 do
    let r = regv.(i) in
    let prev_date = try Reg.Map.find r !use_date with Not_found -> 0 in
    if !current_date > prev_date then
      use_date := Reg.Map.add r !current_date !use_date
  done

(* Check if the register pressure overflows the maximum pressure allowed
   at that point. If so, spill enough registers to lower the pressure. *)

let add_superpressure_regs op live_regs res_regs spilled =
  let max_pressure = Proc.max_register_pressure op in
  let regs = Reg.add_set_array live_regs res_regs in
  (* Compute the pressure in each register class *)
  let pressure = Array.make Proc.num_register_classes 0 in
  Reg.Set.iter
    (fun r ->
      if Reg.Set.mem r spilled then () else begin
        match r.loc with
          Stack s -> ()
        | _ -> let c = Proc.register_class r in
               pressure.(c) <- pressure.(c) + 1
      end)
    regs;
  (* Check if pressure is exceeded for each class. *)
  let rec check_pressure cl spilled =
    if cl >= Proc.num_register_classes then
      spilled
    else if pressure.(cl) <= max_pressure.(cl) then
      check_pressure (cl+1) spilled
    else begin
      (* Find the least recently used, unspilled, unallocated, live register
         in the class *)
      let lru_date = ref 1000000 and lru_reg = ref Reg.dummy in
      Reg.Set.iter
        (fun r ->
          if Proc.register_class r = cl &&
             not (Reg.Set.mem r spilled) &&
             r.loc = Unknown
          then begin
            try
              let d = Reg.Map.find r !use_date in
              if d < !lru_date then begin
                lru_date := d;
                lru_reg := r
              end
            with Not_found ->                 (* Should not happen *)
              ()
          end)
        live_regs;
      if !lru_reg != Reg.dummy then begin
        pressure.(cl) <- pressure.(cl) - 1;
        check_pressure cl (Reg.Set.add !lru_reg spilled)
      end else
        (* Couldn't find any spillable register, give up for this class *)
        check_pressure (cl+1) spilled
    end in
  check_pressure 0 spilled

(* A-list recording what is destroyed at if-then-else points. *)

let destroyed_at_fork = ref ([] : (instruction * Reg.Set.t) list)

(* First pass: insert reload instructions based on an approximation of
   what is destroyed at pressure points. *)

let add_reloads regset i =
  Reg.Set.fold
    (fun r i -> instr_cons (Iop Ireload) [|spill_reg r|] [|r|] i)
    regset i

let reload_at_exit = ref []

let find_reload_at_exit k =
  try
    List.assoc k !reload_at_exit
  with
  | Not_found -> Misc.fatal_error "Spill.find_reload_at_exit"

let reload_at_break = ref Reg.Set.empty

let rec reload i before =
  incr current_date;
  record_use i.arg;
  record_use i.res;
  match i.desc with
    Iend ->
      (i, before)
  | Ireturn | Iop(Itailcall_ind) | Iop(Itailcall_imm _) ->
      (add_reloads (Reg.inter_set_array before i.arg) i,
       Reg.Set.empty)
  | Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) ->
      (* All regs live across must be spilled *)
      let (new_next, finally) = reload i.next i.live in
      (add_reloads (Reg.inter_set_array before i.arg)
                   (instr_cons_debug i.desc i.arg i.res i.dbg new_next),
       finally)
  | Iop op ->
      let new_before =
        (* Quick check to see if the register pressure is below the maximum *)
        if Reg.Set.cardinal i.live + Array.length i.res <=
           Proc.safe_register_pressure op
        then before
        else add_superpressure_regs op i.live i.res before in
      let after =
        Reg.diff_set_array (Reg.diff_set_array new_before i.arg) i.res in
      let (new_next, finally) = reload i.next after in
      (add_reloads (Reg.inter_set_array new_before i.arg)
                   (instr_cons_debug i.desc i.arg i.res i.dbg new_next),
       finally)
  | Iifthenelse(test, ifso, ifnot) ->
      let at_fork = Reg.diff_set_array before i.arg in
      let date_fork = !current_date in
      let (new_ifso, after_ifso) = reload ifso at_fork in
      let date_ifso = !current_date in
      current_date := date_fork;
      let (new_ifnot, after_ifnot) = reload ifnot at_fork in
      current_date := max date_ifso !current_date;
      let (new_next, finally) =
        reload i.next (Reg.Set.union after_ifso after_ifnot) in
      let new_i =
        instr_cons (Iifthenelse(test, new_ifso, new_ifnot))
        i.arg i.res new_next in
      destroyed_at_fork := (new_i, at_fork) :: !destroyed_at_fork;
      (add_reloads (Reg.inter_set_array before i.arg) new_i,
       finally)
  | Iswitch(index, cases) ->
      let at_fork = Reg.diff_set_array before i.arg in
      let date_fork = !current_date in
      let date_join = ref 0 in
      let after_cases = ref Reg.Set.empty in
      let new_cases =
        Array.map
          (fun c ->
            current_date := date_fork;
            let (new_c, after_c) = reload c at_fork in
            after_cases := Reg.Set.union !after_cases after_c;
            date_join := max !date_join !current_date;
            new_c)
          cases in
      current_date := !date_join;
      let (new_next, finally) = reload i.next !after_cases in
      (add_reloads (Reg.inter_set_array before i.arg)
                   (instr_cons (Iswitch(index, new_cases))
                               i.arg i.res new_next),
       finally)
  | Iloop(body) ->
      let date_start = !current_date in
      let at_head = ref before in
      let final_body = ref body in
      begin try
        while true do
          current_date := date_start;
          let (new_body, new_at_head) = reload body !at_head in
          let merged_at_head = Reg.Set.union !at_head new_at_head in
          if Reg.Set.equal merged_at_head !at_head then begin
            final_body := new_body;
            raise Exit
          end;
          at_head := merged_at_head
        done
      with Exit -> ()
      end;
      let (new_next, finally) = reload i.next Reg.Set.empty in
      (instr_cons (Iloop(!final_body)) i.arg i.res new_next,
       finally)
  | Icatch(nfail, body, handler) ->
      let new_set = ref Reg.Set.empty in
      reload_at_exit := (nfail, new_set) :: !reload_at_exit ;
      let (new_body, after_body) = reload body before in
      let at_exit = !new_set in
      reload_at_exit := List.tl !reload_at_exit ;
      let (new_handler, after_handler) = reload handler at_exit in
      let (new_next, finally) =
        reload i.next (Reg.Set.union after_body after_handler) in
      (instr_cons (Icatch(nfail, new_body, new_handler)) i.arg i.res new_next,
       finally)
  | Iexit nfail ->
      let set = find_reload_at_exit nfail in
      set := Reg.Set.union !set before;
      (i, Reg.Set.empty)
  | Itrywith(body, handler) ->
      let (new_body, after_body) = reload body before in
      (* All registers live at the beginning of the handler are destroyed,
         except the exception bucket *)
      let before_handler =
        Reg.Set.remove Proc.loc_exn_bucket
                       (Reg.add_set_array handler.live handler.arg) in
      let (new_handler, after_handler) = reload handler before_handler in
      let (new_next, finally) =
        reload i.next (Reg.Set.union after_body after_handler) in
      (instr_cons (Itrywith(new_body, new_handler)) i.arg i.res new_next,
       finally)
  | Iraise _ ->
      (add_reloads (Reg.inter_set_array before i.arg) i, Reg.Set.empty)

(* Second pass: add spill instructions based on what we've decided to reload.
   That is, any register that may be reloaded in the future must be spilled
   just after its definition. *)

(*
   As an optimization, if a register needs to be spilled in one branch of
   a conditional but not in the other, then we spill it late on entrance
   in the branch that needs it spilled.
   NB: This strategy is turned off in loops, as it may prevent a spill from
   being lifted up all the way out of the loop.
   NB again: This strategy is also off in switch arms
   as it generates many useless spills inside switch arms
   NB ter: is it the same thing for catch bodies ?
*)


let spill_at_exit = ref []
let find_spill_at_exit k =
  try
    List.assoc k !spill_at_exit
  with
  | Not_found -> Misc.fatal_error "Spill.find_spill_at_exit"

let spill_at_raise = ref Reg.Set.empty
let inside_loop = ref false
and inside_arm = ref false
and inside_catch = ref false

let add_spills regset i =
  Reg.Set.fold
    (fun r i -> instr_cons (Iop Ispill) [|r|] [|spill_reg r|] i)
    regset i

let rec spill i finally =
  match i.desc with
    Iend ->
      (i, finally)
  | Ireturn | Iop(Itailcall_ind) | Iop(Itailcall_imm _) ->
      (i, Reg.Set.empty)
  | Iop Ireload ->
      let (new_next, after) = spill i.next finally in
      let before1 = Reg.diff_set_array after i.res in
      (instr_cons i.desc i.arg i.res new_next,
       Reg.add_set_array before1 i.res)
  | Iop _ ->
      let (new_next, after) = spill i.next finally in
      let before1 = Reg.diff_set_array after i.res in
      let before =
        match i.desc with
          Iop Icall_ind | Iop(Icall_imm _) | Iop(Iextcall _)
        | Iop(Iintop Icheckbound) | Iop(Iintop_imm(Icheckbound, _)) ->
            Reg.Set.union before1 !spill_at_raise
        | _ ->
            before1 in
      (instr_cons_debug i.desc i.arg i.res i.dbg
                  (add_spills (Reg.inter_set_array after i.res) new_next),
       before)
  | Iifthenelse(test, ifso, ifnot) ->
      let (new_next, at_join) = spill i.next finally in
      let (new_ifso, before_ifso) = spill ifso at_join in
      let (new_ifnot, before_ifnot) = spill ifnot at_join in
      if
        !inside_loop || !inside_arm
      then
        (instr_cons (Iifthenelse(test, new_ifso, new_ifnot))
                     i.arg i.res new_next,
         Reg.Set.union before_ifso before_ifnot)
      else begin
        let destroyed = List.assq i !destroyed_at_fork in
        let spill_ifso_branch =
          Reg.Set.diff (Reg.Set.diff before_ifso before_ifnot) destroyed
        and spill_ifnot_branch =
          Reg.Set.diff (Reg.Set.diff before_ifnot before_ifso) destroyed in
        (instr_cons
            (Iifthenelse(test, add_spills spill_ifso_branch new_ifso,
                               add_spills spill_ifnot_branch new_ifnot))
            i.arg i.res new_next,
         Reg.Set.diff (Reg.Set.diff (Reg.Set.union before_ifso before_ifnot)
                                    spill_ifso_branch)
                       spill_ifnot_branch)
      end
  | Iswitch(index, cases) ->
      let (new_next, at_join) = spill i.next finally in
      let saved_inside_arm = !inside_arm in
      inside_arm := true ;
      let before = ref Reg.Set.empty in
      let new_cases =
        Array.map
          (fun c ->
            let (new_c, before_c) = spill c at_join in
            before := Reg.Set.union !before before_c;
            new_c)
          cases in
      inside_arm := saved_inside_arm ;
      (instr_cons (Iswitch(index, new_cases)) i.arg i.res new_next,
       !before)
  | Iloop(body) ->
      let (new_next, _) = spill i.next finally in
      let saved_inside_loop = !inside_loop in
      inside_loop := true;
      let at_head = ref Reg.Set.empty in
      let final_body = ref body in
      begin try
        while true do
          let (new_body, before_body) = spill body !at_head in
          let new_at_head = Reg.Set.union !at_head before_body in
          if Reg.Set.equal new_at_head !at_head then begin
            final_body := new_body; raise Exit
          end;
          at_head := new_at_head
        done
      with Exit -> ()
      end;
      inside_loop := saved_inside_loop;
      (instr_cons (Iloop(!final_body)) i.arg i.res new_next,
       !at_head)
  | Icatch(nfail, body, handler) ->
      let (new_next, at_join) = spill i.next finally in
      let (new_handler, at_exit) = spill handler at_join in
      let saved_inside_catch = !inside_catch in
      inside_catch := true ;
      spill_at_exit := (nfail, at_exit) :: !spill_at_exit ;
      let (new_body, before) = spill body at_join in
      spill_at_exit := List.tl !spill_at_exit;
      inside_catch := saved_inside_catch ;
      (instr_cons (Icatch(nfail, new_body, new_handler)) i.arg i.res new_next,
       before)
  | Iexit nfail ->
      (i, find_spill_at_exit nfail)
  | Itrywith(body, handler) ->
      let (new_next, at_join) = spill i.next finally in
      let (new_handler, before_handler) = spill handler at_join in
      let saved_spill_at_raise = !spill_at_raise in
      spill_at_raise := before_handler;
      let (new_body, before_body) = spill body at_join in
      spill_at_raise := saved_spill_at_raise;
      (instr_cons (Itrywith(new_body, new_handler)) i.arg i.res new_next,
       before_body)
  | Iraise _ ->
      (i, !spill_at_raise)

(* Entry point *)

let reset () =
  spill_env := Reg.Map.empty;
  use_date := Reg.Map.empty;
  current_date := 0

let fundecl f =
  reset ();

  let (body1, _) = reload f.fun_body Reg.Set.empty in
  let (body2, tospill_at_entry) = spill body1 Reg.Set.empty in
  let new_body =
    add_spills (Reg.inter_set_array tospill_at_entry f.fun_args) body2 in
  spill_env := Reg.Map.empty;
  use_date := Reg.Map.empty;
  { fun_name = f.fun_name;
    fun_args = f.fun_args;
    fun_body = new_body;
    fun_fast = f.fun_fast;
    fun_dbg  = f.fun_dbg }
