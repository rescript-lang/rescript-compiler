(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* CSE for the i386 *)

open Cmm
open Arch
open Mach
open CSEgen

class cse = object (self)

inherit cse_generic as super

method! class_of_operation op =
  match op with
  (* Operations that affect the floating-point stack cannot be factored *)
  | Iconst_float _ | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Iintoffloat | Ifloatofint
  | Iload((Single | Double | Double_u), _) -> Op_other
  (* Specific ops *)
  | Ispecific(Ilea _) -> Op_pure
  | Ispecific(Istore_int(_, _, is_asg)) -> Op_store is_asg
  | Ispecific(Istore_symbol(_, _, is_asg)) -> Op_store is_asg
  | Ispecific(Ioffset_loc(_, _)) -> Op_store true
  | Ispecific _ -> Op_other
  | _ -> super#class_of_operation op

method! is_cheap_operation op =
  match op with
  | Iconst_int _ | Iconst_blockheader _ -> true
  | Iconst_symbol _ -> true
  | _ -> false

end

let fundecl f =
  (new cse)#fundecl f
