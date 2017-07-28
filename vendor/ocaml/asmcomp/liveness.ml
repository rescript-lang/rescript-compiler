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

(* Liveness analysis.
   Annotate mach code with the set of regs live at each point. *)

open Mach

let live_at_exit = ref []

let find_live_at_exit k =
  try
    List.assoc k !live_at_exit
  with
  | Not_found -> Misc.fatal_error "Liveness.find_live_at_exit"

let live_at_raise = ref Reg.Set.empty

let rec live i finally =
  (* finally is the set of registers live after execution of the
     instruction sequence.
     The result of the function is the set of registers live just
     before the instruction sequence.
     The instruction i is annotated by the set of registers live across
     the instruction. *)
  match i.desc with
    Iend ->
      i.live <- finally;
      finally
  | Ireturn | Iop(Itailcall_ind) | Iop(Itailcall_imm _) ->
      i.live <- Reg.Set.empty; (* no regs are live across *)
      Reg.set_of_array i.arg
  | Iop op ->
      let after = live i.next finally in
      if Proc.op_is_pure op                    (* no side effects *)
      && Reg.disjoint_set_array after i.res    (* results are not used after *)
      && not (Proc.regs_are_volatile i.arg)    (* no stack-like hard reg *)
      && not (Proc.regs_are_volatile i.res)    (*            is involved *)
      then begin
        (* This operation is dead code.  Ignore its arguments. *)
        i.live <- after;
        after
      end else begin
        let across_after = Reg.diff_set_array after i.res in
        let across =
          match op with
          | Icall_ind | Icall_imm _ | Iextcall _
          | Iintop Icheckbound | Iintop_imm(Icheckbound, _) ->
              (* The function call may raise an exception, branching to the
                 nearest enclosing try ... with. Similarly for bounds checks.
                 Hence, everything that must be live at the beginning of
                 the exception handler must also be live across this instr. *)
               Reg.Set.union across_after !live_at_raise
           | _ ->
               across_after in
        i.live <- across;
        Reg.add_set_array across i.arg
      end
  | Iifthenelse(test, ifso, ifnot) ->
      let at_join = live i.next finally in
      let at_fork = Reg.Set.union (live ifso at_join) (live ifnot at_join) in
      i.live <- at_fork;
      Reg.add_set_array at_fork i.arg
  | Iswitch(index, cases) ->
      let at_join = live i.next finally in
      let at_fork = ref Reg.Set.empty in
      for i = 0 to Array.length cases - 1 do
        at_fork := Reg.Set.union !at_fork (live cases.(i) at_join)
      done;
      i.live <- !at_fork;
      Reg.add_set_array !at_fork i.arg
  | Iloop(body) ->
      let at_top = ref Reg.Set.empty in
      (* Yes, there are better algorithms, but we'll just iterate till
         reaching a fixpoint. *)
      begin try
        while true do
          let new_at_top = Reg.Set.union !at_top (live body !at_top) in
          if Reg.Set.equal !at_top new_at_top then raise Exit;
          at_top := new_at_top
        done
      with Exit -> ()
      end;
      i.live <- !at_top;
      !at_top
  | Icatch(nfail, body, handler) ->
      let at_join = live i.next finally in
      let before_handler = live handler at_join in
      let before_body =
          live_at_exit := (nfail,before_handler) :: !live_at_exit ;
          let before_body = live body at_join in
          live_at_exit := List.tl !live_at_exit ;
          before_body in
      i.live <- before_body;
      before_body
  | Iexit nfail ->
      let this_live = find_live_at_exit nfail in
      i.live <- this_live ;
      this_live
  | Itrywith(body, handler) ->
      let at_join = live i.next finally in
      let before_handler = live handler at_join in
      let saved_live_at_raise = !live_at_raise in
      live_at_raise := Reg.Set.remove Proc.loc_exn_bucket before_handler;
      let before_body = live body at_join in
      live_at_raise := saved_live_at_raise;
      i.live <- before_body;
      before_body
  | Iraise _ ->
      i.live <- !live_at_raise;
      Reg.add_set_array !live_at_raise i.arg

let reset () =
  live_at_raise := Reg.Set.empty;
  live_at_exit := []

let fundecl ppf f =
  let initially_live = live f.fun_body Reg.Set.empty in
  (* Sanity check: only function parameters can be live at entrypoint *)
  let wrong_live = Reg.Set.diff initially_live (Reg.set_of_array f.fun_args) in
  if not (Reg.Set.is_empty wrong_live) then begin
    Format.fprintf ppf "%a@." Printmach.regset wrong_live;
    Misc.fatal_error "Liveness.fundecl"
  end
