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

(** "Recursive functions" are those functions [f] that might call either:
    - themselves, or
    - another function that in turn might call [f].

    For example in the following simultaneous definition of [f] [g] and [h],
    [f] and [g] are recursive functions, but not [h]:
      [let rec f x = g x
       and g x = f x
       and h x = g x]
*)

(** Determine the recursive functions, if any, bound by the given set of
    function declarations.
    This is only intended to be used by [Flambda.create_function_declarations].
*)
val in_function_declarations
   : Flambda.function_declarations
  -> backend:(module Backend_intf.S)
  -> Variable.Set.t
