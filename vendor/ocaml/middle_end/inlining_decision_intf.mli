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

(* CR-someday mshinwell: name of this source file could now be improved *)

type 'a by_copying_function_body =
     env:Inline_and_simplify_aux.Env.t
  -> r:Inline_and_simplify_aux.Result.t
  -> clos:Flambda.function_declarations
  -> lfunc:Flambda.t
  -> fun_id:Closure_id.t
  -> func:Flambda.function_declaration
  -> args:Flambda.t list
  -> Flambda.t * Inline_and_simplify_aux.Result.t

type 'a by_copying_function_declaration =
     env:Inline_and_simplify_aux.Env.t
  -> r:Inline_and_simplify_aux.Result.t
  -> funct:Flambda.t
  -> clos:Flambda.function_declarations
  -> fun_id:Closure_id.t
  -> func:Flambda.function_declaration
  -> args_with_approxs:
      (Flambda.t list) * (Simple_value_approx.t list)
  -> invariant_params:Variable.Set.t
  -> specialised_args:Variable.Set.t
  -> dbg:Debuginfo.t
  -> (Flambda.t * Inline_and_simplify_aux.Result.t) option

type simplify =
     Inline_and_simplify_aux.Env.t
  -> Inline_and_simplify_aux.Result.t
  -> Flambda.t
  -> Flambda.t * Inline_and_simplify_aux.Result.t
