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

(** The aim of this pass is to assign symbols to values known to be
    constant (in other words, whose values we know at compile time), with
    appropriate sharing of constants, and replace the occurrences of the
    constants with their corresponding symbols.

    This pass uses the results of two other passes, [Inconstant_idents] and
    [Alias_analysis].  The relationship between these two deserves some
    attention.

    [Inconstant_idents] is a "backwards" analysis that propagates implications
    about inconstantness of variables and set of closures IDs.

    [Alias_analysis] is a "forwards" analysis that is analogous to the
    propagation of [Simple_value_approx.t] values during [Inline_and_simplify].
    It gives us information about relationships between values but not actually
    about their constantness.

    Combining these two into a single pass has been attempted previously,
    but was not thought to be successful; this experiment could be repeated in
    the future.  (If "constant" is considered as "top" and "inconstant" is
    considered as "bottom", then [Alias_analysis] corresponds to a least fixed
    point and [Inconstant_idents] corresponds to a greatest fixed point.)

    At a high level, this pass operates as follows.  Symbols are assigned to
    variables known to be constant and their defining expressions examined.
    Based on the results of [Alias_analysis], we simplify the destructive
    elements within the defining expressions (specifically, projection of
    fields from blocks), to eventually yield [Flambda.constant_defining_value]s
    that are entirely constructive.  These will be bound to symbols in the
    resulting program.

    Another approach to this pass could be to only use the results of
    [Inconstant_idents] and then repeatedly lift constants and run
    [Inline_and_simplify] until a fixpoint.  It was thought more robust to
    instead use [Alias_analysis], where the fixpointing involves a less
    complicated function.

    We still run [Inline_and_simplify] once after this pass since the lifting
    of constants may enable more functions to become closed; the simplification
    pass provides an easy way of cleaning up (e.g. making sure [free_vars]
    maps in sets of closures are correct).
*)

val lift_constants
   : Flambda.program
  -> backend:(module Backend_intf.S)
  -> Flambda.program
