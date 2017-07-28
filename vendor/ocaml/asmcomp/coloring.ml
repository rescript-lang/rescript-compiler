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

(* Register allocation by coloring of the interference graph *)

module OrderedRegSet =
  Set.Make(struct
    type t = Reg.t
    let compare r1 r2 =
      let open Reg in
      let c1 = r1.spill_cost and d1 = r1.degree in
      let c2 = r2.spill_cost and d2 = r2.degree in
      let n = c2 * d1 - c1 * d2 in
      if n <> 0 then n else
        let n = c2 - c1 in
        if n <> 0 then n else
          let n = d1 - d2 in
          if n <> 0 then n else r1.stamp - r2.stamp
  end)

open Reg

let allocate_registers() =

  (* Constrained regs with degree >= number of available registers,
     sorted by spill cost (highest first).
     The spill cost measure is [r.spill_cost / r.degree].
     [r.spill_cost] estimates the number of accesses to [r]. *)
  let constrained = ref OrderedRegSet.empty in

  (* Unconstrained regs with degree < number of available registers *)
  let unconstrained = ref [] in

  (* Preallocate the spilled registers in the stack.
     Split the remaining registers into constrained and unconstrained. *)
  let remove_reg reg =
    let cl = Proc.register_class reg in
    if reg.spill then begin
      (* Preallocate the registers in the stack *)
      let nslots = Proc.num_stack_slots.(cl) in
      let conflict = Array.make nslots false in
      List.iter
        (fun r ->
          match r.loc with
            Stack(Local n) ->
              if Proc.register_class r = cl then conflict.(n) <- true
          | _ -> ())
        reg.interf;
      let slot = ref 0 in
      while !slot < nslots && conflict.(!slot) do incr slot done;
      reg.loc <- Stack(Local !slot);
      if !slot >= nslots then Proc.num_stack_slots.(cl) <- !slot + 1
    end else if reg.degree < Proc.num_available_registers.(cl) then
      unconstrained := reg :: !unconstrained
    else begin
      constrained := OrderedRegSet.add reg !constrained
    end in

  (* Iterate over all registers preferred by the given register (transitive) *)
  let iter_preferred f reg =
    let rec walk r w =
      if not r.visited then begin
        f r w;
        begin match r.prefer with
            [] -> ()
          | p  -> r.visited <- true;
                  List.iter (fun (r1, w1) -> walk r1 (min w w1)) p;
                  r.visited <- false
        end
      end in
    reg.visited <- true;
    List.iter (fun (r, w) -> walk r w) reg.prefer;
    reg.visited <- false in

  (* Where to start the search for a suitable register.
     Used to introduce some "randomness" in the choice between registers
     with equal scores. This offers more opportunities for scheduling. *)
  let start_register = Array.make Proc.num_register_classes 0 in

  (* Assign a location to a register, the best we can. *)
  let assign_location reg =
    let cl = Proc.register_class reg in
    let first_reg = Proc.first_available_register.(cl) in
    let num_regs = Proc.num_available_registers.(cl) in
    let score = Array.make num_regs 0 in
    let best_score = ref (-1000000) and best_reg = ref (-1) in
    let start = start_register.(cl) in
    if num_regs <> 0 then begin
      (* Favor the registers that have been assigned to pseudoregs for which
         we have a preference. If these pseudoregs have not been assigned
         already, avoid the registers with which they conflict. *)
      iter_preferred
        (fun r w ->
          match r.loc with
            Reg n -> let n = n - first_reg in
                     if n < num_regs then
                       score.(n) <- score.(n) + w
          | Unknown ->
              List.iter
                (fun neighbour ->
                  match neighbour.loc with
                    Reg n -> let n = n - first_reg in
                             if n < num_regs then
                               score.(n) <- score.(n) - w
                  | _ -> ())
                r.interf
          | _ -> ())
        reg;
      List.iter
        (fun neighbour ->
          (* Prohibit the registers that have been assigned
             to our neighbours *)
          begin match neighbour.loc with
            Reg n -> let n = n - first_reg in
                     if n < num_regs then
                       score.(n) <- (-1000000)
          | _ -> ()
          end;
          (* Avoid the registers that have been assigned to pseudoregs
             for which our neighbours have a preference *)
          iter_preferred
            (fun r w ->
              match r.loc with
                Reg n -> let n = n - first_reg in
                         if n < num_regs then
                           score.(n) <- score.(n) - (w-1)
                         (* w-1 to break the symmetry when two conflicting regs
                            have the same preference for a third reg. *)
              | _ -> ())
            neighbour)
        reg.interf;
      (* Pick the register with the best score *)
      for n = start to num_regs - 1 do
        if score.(n) > !best_score then begin
          best_score := score.(n);
          best_reg := n
        end
      done;
      for n = 0 to start - 1 do
        if score.(n) > !best_score then begin
          best_score := score.(n);
          best_reg := n
        end
      done
    end;
    (* Found a register? *)
    if !best_reg >= 0 then begin
      reg.loc <- Reg(first_reg + !best_reg);
      if Proc.rotate_registers then
        start_register.(cl) <- (let start = start + 1 in
                                if start >= num_regs then 0 else start)
    end else begin
      (* Sorry, we must put the pseudoreg in a stack location *)
      let nslots = Proc.num_stack_slots.(cl) in
      let score = Array.make nslots 0 in
      (* Compute the scores as for registers *)
      List.iter
        (fun (r, w) ->
          match r.loc with
            Stack(Local n) -> score.(n) <- score.(n) + w
          | Unknown ->
              List.iter
                (fun neighbour ->
                  match neighbour.loc with
                    Stack(Local n) -> score.(n) <- score.(n) - w
                  | _ -> ())
                r.interf
          | _ -> ())
        reg.prefer;
      List.iter
        (fun neighbour ->
          begin match neighbour.loc with
              Stack(Local n) -> score.(n) <- (-1000000)
          | _ -> ()
          end;
          List.iter
            (fun (r, w) ->
              match r.loc with
                Stack(Local n) -> score.(n) <- score.(n) - w
              | _ -> ())
            neighbour.prefer)
        reg.interf;
      (* Pick the location with the best score *)
      let best_score = ref (-1000000) and best_slot = ref (-1) in
      for n = 0 to nslots - 1 do
        if score.(n) > !best_score then begin
          best_score := score.(n);
          best_slot := n
        end
      done;
      (* Found one? *)
      if !best_slot >= 0 then
        reg.loc <- Stack(Local !best_slot)
      else begin
        (* Allocate a new stack slot *)
        reg.loc <- Stack(Local nslots);
        Proc.num_stack_slots.(cl) <- nslots + 1
      end
    end;
    (* Cancel the preferences of this register so that they don't influence
       transitively the allocation of registers that prefer this reg. *)
    reg.prefer <- [] in

  (* Reset the stack slot counts *)
  for i = 0 to Proc.num_register_classes - 1 do
    Proc.num_stack_slots.(i) <- 0;
  done;

  (* First pass: preallocate spill registers and split remaining regs
     Second pass: assign locations to constrained regs
     Third pass: assign locations to unconstrained regs *)
  List.iter remove_reg (Reg.all_registers());
  OrderedRegSet.iter assign_location !constrained;
  List.iter assign_location !unconstrained
