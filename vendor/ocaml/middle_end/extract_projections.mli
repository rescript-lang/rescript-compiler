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

(** Identify projections from variables used in function bodies (free
    variables or specialised args, for example, according to [which_variables]
    below).  Projections from variables that are also used boxed are not
    returned. *)

(** [which_variables] maps (existing) inner variables to (existing) outer
    variables in the manner of [free_vars] and [specialised_args] in
    [Flambda.set_of_closures].

    The returned projections are [projecting_from] (cf. projection.mli)
    the "existing inner vars".
*)
val from_function_decl
   : env:Inline_and_simplify_aux.Env.t
  -> which_variables:Flambda.specialised_to Variable.Map.t
  -> function_decl:Flambda.function_declaration
  -> Projection.Set.t
