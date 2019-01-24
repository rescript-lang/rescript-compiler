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

(** Assign numerical offsets, within closure blocks, for code pointers and
    environment entries. *)

type result = private {
  function_offsets : int Closure_id.Map.t;
  free_variable_offsets : int Var_within_closure.Map.t;
}

val compute : Flambda.program -> result

(** If compilation unit [C] references [B], which contains functions inlined
    from another compilation unit [A], then we may need to know the layout of
    closures inside (or constructed by code inside) a.cmx in order to
    compile c.cmx.  Unfortunately a.cmx is permitted to be absent during such
    compilation; c.cmx will be compiled using just b.cmx.  As such, when
    building the .cmx export information for a given compilation unit, we
    also include information about the layout of any closures that it depends
    on from other compilation units.  This means that when situations as just
    describe arise, we always have access to the necessary closure offsets. *)
val compute_reexported_offsets
   : Flambda.program
  -> current_unit_offset_fun:int Closure_id.Map.t
  -> current_unit_offset_fv:int Var_within_closure.Map.t
  -> imported_units_offset_fun:int Closure_id.Map.t
  -> imported_units_offset_fv:int Var_within_closure.Map.t
  -> int Closure_id.Map.t * int Var_within_closure.Map.t
