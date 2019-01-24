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

type result = {
  expr : Clambda.ulambda;
  preallocated_blocks : Clambda.preallocated_block list;
  structured_constants : Clambda.ustructured_constant Symbol.Map.t;
  exported : Export_info.t;
}

(** Convert an Flambda program, with associated proto-export information,
    to Clambda.
    This yields a Clambda expression together with augmented export
    information and details about required statically-allocated values
    (preallocated blocks, for [Initialize_symbol], and structured
    constants).

    It is during this process that accesses to variables within
    closures are transformed to field accesses within closure values.
    For direct calls, the hidden closure parameter is added.  Switch
    tables are also built.
*)
val convert : Flambda.program * Export_info.t -> result
