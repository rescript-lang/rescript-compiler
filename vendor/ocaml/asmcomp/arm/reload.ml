(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Arch
open Mach

(* Reloading for the ARM *)

class reload = object

inherit Reloadgen.reload_generic as super

method! reload_operation op arg res =
  let ((arg', res') as argres') = super#reload_operation op arg res in
  match op with
  | Iintop Imul | Ispecific Imuladd ->
      (* On ARM v4 and v5, module [Selection] adds a second, dummy
         result to multiplication instructions (mul and muladd).  This
         second result is the same pseudoregister as the first
         argument to the multiplication.  As shown in MPR#7642,
         reloading must maintain this invariant.  Otherwise, the second
         result and the first argument can end up in different registers,
         and the second result can be used later, even though
         it is not initialized. *)
      if Array.length res' >= 2 then res'.(1) <- arg'.(0);
      argres'
  | Ispecific(Imuladdf | Inegmuladdf | Imulsubf | Inegmulsubf) ->
      (* VFP float multiply-add instructions are "two-address" in the
         sense that they must have [arg.(0) = res.(0)].
         Preserve this invariant. *)
      (arg', [|arg'.(0)|])
  | Iabsf | Inegf when !fpu = Soft ->
      (* Soft FP neg and abs also have a "two-address" constraint of sorts.
         64-bit floats are represented by pairs of 32-bit integers,
	 hence there are two arguments and two results.
	 The code emitter assumes [arg.(0) = res.(0)] but supports
	 [arg.(1)] and [res.(1)] being in different registers. *)
      res'.(0) <- arg'.(0);
      argres'
  | _ ->
      argres'
end

let fundecl f =
  (new reload)#fundecl f
