(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Benedikt Meurer, University of Siegen              *)
(*                                                                     *)
(*    Copyright 1998 Institut National de Recherche en Informatique    *)
(*    et en Automatique. Copyright 2012 Benedikt Meurer. All rights    *)
(*    reserved.  This file is distributed  under the terms of the Q    *)
(*    Public License version 1.0.                                      *)
(*                                                                     *)
(***********************************************************************)

open Arch
open Mach

(* Instruction scheduling for the ARM *)

class scheduler = object(self)

inherit Schedgen.scheduler_generic as super

(* Scheduling -- based roughly on the ARM11 (ARMv6) *)

method oper_latency = function
  (* Loads have a latency of two cycles in general *)
    Iconst_symbol _
  | Iconst_float _
  | Iload(_, _)
  | Ireload
  | Ifloatofint       (* mcr/mrc count as memory access *)
  | Iintoffloat -> 2
  (* Multiplys have a latency of two cycles *)
  | Iintop (Imul | Imulh)
  | Ispecific(Imuladd | Imulsub | Imulhadd) -> 2
  (* VFP instructions *)
  | Iaddf
  | Isubf
  | Idivf
  | Imulf | Ispecific Inegmulf
  | Ispecific(Imuladdf | Inegmuladdf | Imulsubf | Inegmulsubf)
  | Ispecific Isqrtf
  | Inegf | Iabsf when !fpu >= VFPv2 -> 2
  (* Everything else *)
  | _ -> 1

method! is_checkbound = function
    Ispecific(Ishiftcheckbound _) -> true
  | op -> super#is_checkbound op

(* Issue cycles. Rough approximations *)

method oper_issue_cycles = function
    Ialloc _ -> 4
  | Iintop(Ilsl | Ilsr | Iasr) -> 2
  | Iintop(Icomp _)
  | Iintop_imm(Icomp _, _) -> 3
  | Iintop(Icheckbound)
  | Iintop_imm(Icheckbound, _) -> 2
  | Ispecific(Ishiftcheckbound _) -> 3
  | Iintop(Imul | Imulh)
  | Ispecific(Imuladd | Imulsub | Imulhadd) -> 2
  (* VFP instructions *)
  | Iaddf
  | Isubf -> 7
  | Imulf
  | Ispecific Inegmulf -> 9
  | Ispecific(Imuladdf | Inegmuladdf | Imulsubf | Inegmulsubf) -> 17
  | Idivf
  | Ispecific Isqrtf -> 27
  | Inegf | Iabsf | Iconst_float _ when !fpu >= VFPv2 -> 4
  (* Everything else *)
  | _ -> 1

end

let fundecl f = (new scheduler)#schedule_fundecl f
