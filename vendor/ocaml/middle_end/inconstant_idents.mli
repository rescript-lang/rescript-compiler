(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type result

(** [inconstants_on_program] finds those variables and set-of-closures
    identifiers that cannot be compiled to constants by [Flambda_to_clambda].
*)
val inconstants_on_program
   : compilation_unit:Compilation_unit.t
  -> backend:(module Backend_intf.S)
  -> Flambda.program
  -> result

(** [variable var res] returns [true] if [var] is marked as inconstant
    in [res]. *)
val variable : Variable.t -> result -> bool

(** [closure cl res] returns [true] if [cl] is marked as inconstant
    in [res]. *)
val closure : Set_of_closures_id.t -> result -> bool
