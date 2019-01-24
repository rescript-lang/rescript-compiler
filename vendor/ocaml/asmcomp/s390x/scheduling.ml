(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt            *)
(*                          Bill O'Farrell, IBM                           *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2015 IBM (Bill O'Farrell with help from Tristan Amini).    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction scheduling for the Z processor *)

open Arch
open Mach

(* The z10 processor is in-order, dual-issue.  It could benefit from some
   basic-block scheduling, although precise latency information
   is not available.
   The z196 and later are out-of-order processors.  Basic-block
   scheduling probably makes no difference. *)

class scheduler = object

inherit Schedgen.scheduler_generic

(* Latencies (in cycles). Wild guesses.  We multiply all latencies by 2
   to favor dual-issue. *)

method oper_latency = function
    Ireload -> 4
  | Iload(_, _) -> 4
  | Iconst_float _ -> 4 (* turned into a load *)
  | Iintop(Imul) -> 10
  | Iintop_imm(Imul, _) -> 10
  | Iaddf | Isubf | Imulf -> 8
  | Idivf -> 40
  | Ispecific(Imultaddf | Imultsubf) -> 8
  | _ -> 2

method! reload_retaddr_latency = 4

(* Issue cycles.  Rough approximations. *)

method oper_issue_cycles = function
  | Ialloc _ -> 4
  | Iintop(Imulh) -> 15
  | Iintop(Idiv|Imod) -> 20
  | Iintop(Icomp _) -> 4
  | Iintop_imm(Icomp _, _) -> 4
  | _ -> 1

method! reload_retaddr_issue_cycles = 1

end

let fundecl f = (new scheduler)#schedule_fundecl f
