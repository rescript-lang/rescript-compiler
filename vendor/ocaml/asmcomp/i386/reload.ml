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
open Arch
open Reg
open Mach

(* Reloading for the Intel x86 *)

let stackp r =
  match r.loc with
    Stack _ -> true
  | _ -> false

class reload = object (self)

inherit Reloadgen.reload_generic as super

method! makereg r =
  match r.typ with
    Float -> r
  | _ -> super#makereg r

(* By overriding makereg, we make sure that pseudoregs of type float
   will never be reloaded. Hence there is no need to make special cases for
   floating-point operations. *)

method! reload_operation op arg res =
  match op with
    Iintop(Iadd|Isub|Iand|Ior|Ixor|Icomp _|Icheckbound) ->
      (* One of the two arguments can reside in the stack *)
      if stackp arg.(0) && stackp arg.(1)
      then ([|arg.(0); self#makereg arg.(1)|], res)
      else (arg, res)
  | Iintop(Imul) ->
      (* First argument (and destination) must be in register,
         second arg can reside in stack *)
      if stackp arg.(0)
      then let r = self#makereg arg.(0) in ([|r; arg.(1)|], [|r|])
      else (arg, res)
  | Iintop_imm(Iadd, _) when arg.(0).loc <> res.(0).loc ->
      (* This add will be turned into a lea; args and results must be
         in registers *)
      super#reload_operation op arg res
  | Iintop_imm(Imul, _) ->
      (* First argument and destination must be in register *)
      if stackp arg.(0)
      then let r = self#makereg arg.(0) in ([|r|], [|r|])
      else (arg, res)
  | Iintop(Imulh | Ilsl | Ilsr | Iasr) | Iintop_imm(_, _)
  | Ifloatofint | Iintoffloat | Ispecific(Ipush) ->
      (* The argument(s) can be either in register or on stack *)
      (* Note: Imulh: arg(0 and res(0) already forced in regs
               Ilsl, Ilsr, Iasr: arg(1) already forced in regs *)
      (arg, res)
  | _ -> (* Other operations: all args and results in registers *)
      super#reload_operation op arg res

method! reload_test tst arg =
  match tst with
    Iinttest cmp ->
      (* One of the two arguments can reside on stack *)
      if stackp arg.(0) && stackp arg.(1)
      then [| self#makereg arg.(0); arg.(1) |]
      else arg
  | _ ->
      (* The argument(s) can be either in register or on stack *)
      arg

end

let fundecl f =
  (new reload)#fundecl f
