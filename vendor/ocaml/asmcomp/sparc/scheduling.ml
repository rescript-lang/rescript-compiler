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

open Cmm
open Mach

(* Instruction scheduling for the Sparc *)

class scheduler = object

inherit Schedgen.scheduler_generic

(* Latencies (in cycles). *)

(* UltraSPARC issues two integer operations, plus a single load or store,
   per cycle.  At most one of the integer instructions may be a shift.
   Most integer operations have one cycle latency.  Unsigned loads take
   two cycles.  Signed loads take three cycles.  Conditional moves have
   two cycle latency and may not issue in the same cycle as any other
   instruction.  Floating point issue rules are complicated, but in
   general independent add and multiply can dual issue with four cycle
   latency.  *)

method oper_latency = function
    Ireload -> 2
  | Iload((Byte_signed|Sixteen_signed|Thirtytwo_signed), _) -> 3
  | Iload(_, _) -> 2
  | Iconst_float _ -> 2 (* turned into a load *)
  | Inegf | Iabsf | Iaddf | Isubf | Imulf -> 4
  | Idivf -> 15
  | _ -> 1

(* Issue cycles.  Rough approximations. *)

method oper_issue_cycles = function
    Iconst_float _ -> 2
  | Iconst_symbol _ -> 2
  | Ialloc _ -> 6
  | Iintop(Icomp _) -> 4
  | Iintop(Icheckbound) -> 2
  | Iintop_imm(Icomp _, _) -> 4
  | Iintop_imm(Icheckbound, _) -> 2
  | Inegf -> 2
  | Iabsf -> 2
  | Ifloatofint -> 6
  | Iintoffloat -> 6
  | _ -> 1

end

let fundecl f = (new scheduler)#schedule_fundecl f
