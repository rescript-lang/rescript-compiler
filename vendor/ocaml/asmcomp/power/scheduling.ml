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

(* Instruction scheduling for the Power PC *)

open Arch
open Mach

class scheduler = object

inherit Schedgen.scheduler_generic

(* Latencies (in cycles). Based roughly on the "common model". *)

method oper_latency = function
    Ireload -> 2
  | Iload(_, _) -> 2
  | Iconst_float _ -> 2 (* turned into a load *)
  | Iconst_symbol _ -> 1
  | Iintop(Imul | Imulh) -> 9
  | Iintop_imm(Imul, _) -> 5
  | Iintop(Idiv | Imod) -> 36
  | Iaddf | Isubf -> 4
  | Imulf -> 5
  | Idivf -> 33
  | Ispecific(Imultaddf | Imultsubf) -> 5
  | _ -> 1

method reload_retaddr_latency = 12
  (* If we can have that many cycles between the reloadretaddr and the
     return, we can expect that the blr branch will be completely folded. *)

(* Issue cycles.  Rough approximations. *)

method oper_issue_cycles = function
    Iconst_float _ | Iconst_symbol _ -> 2
  | Iload(_, Ibased(_, _)) -> 2
  | Istore(_, Ibased(_, _), _) -> 2
  | Ialloc _ -> 4
  | Iintop(Imod) -> 40 (* assuming full stall *)
  | Iintop(Icomp _) -> 4
  | Iintop_imm(Icomp _, _) -> 4
  | Ifloatofint -> 9
  | Iintoffloat -> 4
  | _ -> 1

method reload_retaddr_issue_cycles = 3
  (* load then stalling mtlr *)

end

let fundecl f = (new scheduler)#schedule_fundecl f
