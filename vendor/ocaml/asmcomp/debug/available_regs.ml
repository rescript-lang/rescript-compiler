(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Mark Shinwell and Thomas Refis, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 2013--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module M = Mach
module R = Reg
module RAS = Reg_availability_set
module RD = Reg_with_debug_info

(* This pass treats [avail_at_exit] like a "result" structure whereas the
   equivalent in [Liveness] is like an "environment".  (Which means we need
   to be careful not to throw away information about further-out catch
   handlers collected in [avail_at_exit].) *)
let avail_at_exit = Hashtbl.create 42
let avail_at_raise = ref RAS.Unreachable

let augment_availability_at_raise avail =
  avail_at_raise := RAS.inter avail !avail_at_raise

let check_invariants (instr : M.instruction) ~(avail_before : RAS.t) =
  match avail_before with
  | Unreachable -> ()
  | Ok avail_before ->
    (* Every register that is live across an instruction should also be
       available before the instruction. *)
    if not (R.Set.subset instr.live (RD.Set.forget_debug_info avail_before))
    then begin
      Misc.fatal_errorf "Live registers not a subset of available registers: \
          live={%a} avail_before=%a missing={%a} insn=%a"
        Printmach.regset instr.live
        (RAS.print ~print_reg:Printmach.reg)
        (RAS.Ok avail_before)
        Printmach.regset (R.Set.diff instr.live
          (RD.Set.forget_debug_info avail_before))
        Printmach.instr ({ instr with M. next = M.end_instr (); })
    end;
    (* Every register that is an input to an instruction should be
       available. *)
    let args = R.set_of_array instr.arg in
    let avail_before_fdi = RD.Set.forget_debug_info avail_before in
    if not (R.Set.subset args avail_before_fdi) then begin
      Misc.fatal_errorf "Instruction has unavailable input register(s): \
          avail_before=%a avail_before_fdi={%a} inputs={%a} insn=%a"
        (RAS.print ~print_reg:Printmach.reg) (RAS.Ok avail_before)
        Printmach.regset avail_before_fdi
        Printmach.regset args
        Printmach.instr ({ instr with M. next = M.end_instr (); })
    end

(* [available_regs ~instr ~avail_before] calculates, given the registers
   "available before" an instruction [instr], the registers that are available
   both "across" and immediately after [instr].  This is a forwards dataflow
   analysis.

   "available before" can be thought of, at the assembly level, as the set of
   registers available when the program counter is equal to the address of the
   particular instruction under consideration (that is to say, immediately
   prior to the instruction being executed).  Inputs to that instruction are
   available at this point even if the instruction will clobber them.  Results
   from the previous instruction are also available at this point.

   "available across" is the registers available during the execution of
   some particular instruction.  These are the registers "available before"
   minus registers that may be clobbered or otherwise invalidated by the
   instruction.  (The notion of "available across" is only useful for [Iop]
   instructions.  Recall that some of these may expand into multiple
   machine instructions including clobbers, e.g. for [Ialloc].)

   The [available_before] and [available_across] fields of each instruction
   is updated by this function.
*)
let rec available_regs (instr : M.instruction)
      ~(avail_before : RAS.t) : RAS.t =
  check_invariants instr ~avail_before;
  instr.available_before <- avail_before;
  let avail_across, avail_after =
    let ok set = RAS.Ok set in
    let unreachable = RAS.Unreachable in
    match avail_before with
    | Unreachable -> None, unreachable
    | Ok avail_before ->
      match instr.desc with
      | Iend -> None, ok avail_before
      | Ireturn -> None, unreachable
      | Iop (Itailcall_ind _) | Iop (Itailcall_imm _) ->
        Some (ok Reg_with_debug_info.Set.empty), unreachable
      | Iop (Iname_for_debugger { ident; which_parameter; provenance;
          is_assignment; }) ->
        (* First forget about any existing debug info to do with [ident]
           if the naming corresponds to an assignment operation. *)
        let forgetting_ident =
          if not is_assignment then
            avail_before
          else
            RD.Set.map (fun reg ->
                match RD.debug_info reg with
                | None -> reg
                | Some debug_info ->
                  if Ident.same (RD.Debug_info.holds_value_of debug_info) ident
                  then RD.clear_debug_info reg
                  else reg)
              avail_before
        in
        let avail_after = ref forgetting_ident in
        let num_parts_of_value = Array.length instr.arg in
        (* Add debug info about [ident], but only for registers that are known
           to be available. *)
        for part_of_value = 0 to num_parts_of_value - 1 do
          let reg = instr.arg.(part_of_value) in
          if RD.Set.mem_reg forgetting_ident reg then begin
            let regd =
              RD.create ~reg
                ~holds_value_of:ident
                ~part_of_value
                ~num_parts_of_value
                ~which_parameter
                ~provenance
            in
            avail_after := RD.Set.add regd (RD.Set.filter_reg !avail_after reg)
          end
        done;
        Some (ok avail_before), ok !avail_after
      | Iop (Imove | Ireload | Ispill) ->
        (* Moves are special: they enable us to propagate names.
           No-op moves need to be handled specially---in this case, we may
           learn that a given hard register holds the value of multiple
           pseudoregisters (all of which have the same value).  This makes us
           match up properly with [Liveness]. *)
        let move_to_same_location =
          let move_to_same_location = ref true in
          for i = 0 to Array.length instr.arg - 1 do
            let arg = instr.arg.(i) in
            let res = instr.res.(i) in
            (* Note that the register classes must be the same, so we don't
                need to check that. *)
            if arg.loc <> res.loc then begin
              move_to_same_location := false
            end
          done;
          !move_to_same_location
        in
        let made_unavailable =
          if move_to_same_location then
            RD.Set.empty
          else
            RD.Set.made_unavailable_by_clobber avail_before
              ~regs_clobbered:instr.res
              ~register_class:Proc.register_class
        in
        let results =
          Array.map2 (fun arg_reg result_reg ->
              match RD.Set.find_reg_exn avail_before arg_reg with
              | exception Not_found ->
                assert false  (* see second invariant in [check_invariants] *)
              | arg_reg ->
                RD.create_copying_debug_info ~reg:result_reg
                  ~debug_info_from:arg_reg)
            instr.arg instr.res
        in
        let avail_across = RD.Set.diff avail_before made_unavailable in
        let avail_after = RD.Set.union avail_across (RD.Set.of_array results) in
        Some (ok avail_across), ok avail_after
      | Iop op ->
        (* We split the calculation of registers that become unavailable after
           a call into two parts.  First: anything that the target marks as
           destroyed by the operation, combined with any registers that will
           be clobbered by the operation writing out its results. *)
        let made_unavailable_1 =
          let regs_clobbered =
            Array.append (Proc.destroyed_at_oper instr.desc) instr.res
          in
          RD.Set.made_unavailable_by_clobber avail_before ~regs_clobbered
            ~register_class:Proc.register_class
        in
        (* Second: the cases of (a) allocations and (b) OCaml to OCaml function
           calls.  In these cases, since the GC may run, registers always
           become unavailable unless:
           (a) they are "live across" the instruction; and/or
           (b) they hold immediates and are assigned to the stack.
           For the moment we assume that [Ispecific] instructions do not
           run the GC. *)
        (* CR-someday mshinwell: Consider factoring this out from here and
           [Available_ranges.Make_ranges.end_pos_offset]. *)
        let made_unavailable_2 =
          match op with
          | Icall_ind _ | Icall_imm _ | Ialloc _ ->
            RD.Set.filter (fun reg ->
                let holds_immediate = RD.holds_non_pointer reg in
                let on_stack = RD.assigned_to_stack reg in
                let live_across = Reg.Set.mem (RD.reg reg) instr.live in
                let remains_available =
                  live_across
                    || (holds_immediate && on_stack)
                in
                not remains_available)
              avail_before
          | _ -> RD.Set.empty
        in
        let made_unavailable =
          RD.Set.union made_unavailable_1 made_unavailable_2
        in
        let avail_across = RD.Set.diff avail_before made_unavailable in
        if M.operation_can_raise op then begin
          augment_availability_at_raise (ok avail_across)
        end;
        let avail_after =
          RD.Set.union
            (RD.Set.without_debug_info (Reg.set_of_array instr.res))
            avail_across
        in
        Some (ok avail_across), ok avail_after
      | Iifthenelse (_, ifso, ifnot) -> join [ifso; ifnot] ~avail_before
      | Iswitch (_, cases) -> join (Array.to_list cases) ~avail_before
      | Iloop body ->
        let avail_after = ref (ok avail_before) in
        begin try
          while true do
            let avail_after' =
              RAS.inter !avail_after
                (available_regs body ~avail_before:!avail_after)
            in
            if RAS.equal !avail_after avail_after' then begin
              raise Exit
              end;
            avail_after := avail_after'
          done
        with Exit -> ()
        end;
        None, unreachable
      | Icatch (recursive, handlers, body) ->
        List.iter (fun (nfail, _handler) ->
            (* In case there are nested [Icatch] expressions with the same
               handler numbers, we rely on the [Hashtbl] shadowing
               semantics. *)
            Hashtbl.add avail_at_exit nfail unreachable)
          handlers;
        let avail_after_body =
          available_regs body ~avail_before:(ok avail_before)
        in
        (* CR-someday mshinwell: Consider potential efficiency speedups
           (see suggestions from @chambart on GPR#856). *)
        let aux (nfail, handler) (nfail', avail_at_top_of_handler) =
          assert (nfail = nfail');
          available_regs handler ~avail_before:avail_at_top_of_handler
        in
        let aux_equal (nfail, avail_before_handler)
              (nfail', avail_before_handler') =
          assert (nfail = nfail');
          RAS.equal avail_before_handler avail_before_handler'
        in
        let rec fixpoint avail_at_top_of_handlers =
          let avail_after_handlers =
            List.map2 aux handlers avail_at_top_of_handlers
          in
          let avail_at_top_of_handlers' =
            List.map (fun (nfail, _handler) ->
                match Hashtbl.find avail_at_exit nfail with
                | exception Not_found -> assert false  (* see above *)
                | avail_at_top_of_handler -> nfail, avail_at_top_of_handler)
              handlers
          in
          match recursive with
          | Nonrecursive -> avail_after_handlers
          | Recursive ->
            if List.for_all2 aux_equal avail_at_top_of_handlers
              avail_at_top_of_handlers'
            then avail_after_handlers
            else fixpoint avail_at_top_of_handlers'
        in
        let init_avail_at_top_of_handlers =
          List.map (fun (nfail, _handler) ->
              match Hashtbl.find avail_at_exit nfail with
              | exception Not_found -> assert false  (* see above *)
              | avail_at_top_of_handler -> nfail, avail_at_top_of_handler)
            handlers
        in
        let avail_after_handlers = fixpoint init_avail_at_top_of_handlers in
        List.iter (fun (nfail, _handler) ->
            Hashtbl.remove avail_at_exit nfail)
          handlers;
        let avail_after =
          List.fold_left (fun avail_at_join avail_after_handler ->
              RAS.inter avail_at_join avail_after_handler)
            avail_after_body
            avail_after_handlers
        in
        None, avail_after
      | Iexit nfail ->
        let avail_before = ok avail_before in
        let avail_at_top_of_handler =
          match Hashtbl.find avail_at_exit nfail with
          | exception Not_found ->  (* also see top of [Icatch] clause above *)
            Misc.fatal_errorf "Iexit %d not in scope of Icatch" nfail
          | avail_at_top_of_handler -> avail_at_top_of_handler
        in
        let avail_at_top_of_handler =
          RAS.inter avail_at_top_of_handler avail_before
        in
        Hashtbl.replace avail_at_exit nfail avail_at_top_of_handler;
        None, unreachable
      | Itrywith (body, handler) ->
        let saved_avail_at_raise = !avail_at_raise in
        avail_at_raise := unreachable;
        let avail_before = ok avail_before in
        let after_body = available_regs body ~avail_before in
        let avail_before_handler =
          match !avail_at_raise with
          | Unreachable -> unreachable
          | Ok avail_at_raise ->
            let without_exn_bucket =
              RD.Set.filter_reg avail_at_raise Proc.loc_exn_bucket
            in
            let with_anonymous_exn_bucket =
              RD.Set.add (RD.create_without_debug_info ~reg:Proc.loc_exn_bucket)
                without_exn_bucket
            in
            ok with_anonymous_exn_bucket
        in
        avail_at_raise := saved_avail_at_raise;
        let avail_after =
          RAS.inter after_body
            (available_regs handler ~avail_before:avail_before_handler)
        in
        None, avail_after
      | Iraise _ ->
        let avail_before = ok avail_before in
        augment_availability_at_raise avail_before;
        None, unreachable
  in
  instr.available_across <- avail_across;
  match instr.desc with
  | Iend -> avail_after
  | _ -> available_regs instr.next ~avail_before:avail_after

and join branches ~avail_before =
  let avail_before = RAS.Ok avail_before in
  let avails = List.map (available_regs ~avail_before) branches in
  let avail_after =
    match avails with
    | [] -> avail_before
    | avail::avails -> List.fold_left RAS.inter avail avails
  in
  None, avail_after

let fundecl (f : M.fundecl) =
  if !Clflags.debug && !Clflags.debug_runavail then begin
    assert (Hashtbl.length avail_at_exit = 0);
    avail_at_raise := RAS.Unreachable;
    let fun_args = R.set_of_array f.fun_args in
    let avail_before = RAS.Ok (RD.Set.without_debug_info fun_args) in
    ignore ((available_regs f.fun_body ~avail_before) : RAS.t);
  end;
  f
