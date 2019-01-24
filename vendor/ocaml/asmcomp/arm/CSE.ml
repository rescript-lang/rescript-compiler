(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CSE for ARM *)

open Arch
open Mach
open CSEgen

class cse = object

inherit cse_generic as super

method! class_of_operation op =
  match op with
  | Ispecific(Ishiftcheckbound _) -> Op_checkbound
  | Ispecific _ -> Op_pure
  | _ -> super#class_of_operation op

method! is_cheap_operation op =
  match op with
  | Iconst_int n -> n <= 255n && n >= 0n
  | _ -> false

end

let fundecl f =
  (new cse)#fundecl f
