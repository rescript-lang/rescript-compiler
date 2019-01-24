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

(** Description of the semantics of primitives, to be used for optimization
    purposes.

    "No effects" means that the primitive does not change the observable state
    of the world.  For example, it must not write to any mutable storage,
    call arbitrary external functions or change control flow (e.g. by raising
    an exception).  Note that allocation is not "No effects" (see below).

    It is assumed in the compiler that applications of primitives with no
    effects, whose results are not used, may be eliminated.  It is further
    assumed that applications of primitives with no effects may be
    duplicated (and thus possibly executed more than once).

    (Exceptions arising from allocation points, for example "out of memory" or
    exceptions propagated from finalizers or signal handlers, are treated as
    "effects out of the ether" and thus ignored for our determination here
    of effectfulness.  The same goes for floating point operations that may
    cause hardware traps on some platforms.)

    "Only generative effects" means that a primitive does not change the
    observable state of the world save for possibly affecting the state of
    the garbage collector by performing an allocation.  Applications of
    primitives that only have generative effects and whose results are unused
    may be eliminated by the compiler.  However, unlike "No effects"
    primitives, such applications will never be eligible for duplication.

    "Arbitrary effects" covers all other primitives.

    "No coeffects" means that the primitive does not observe the effects (in
    the sense described above) of other expressions.  For example, it must not
    read from any mutable storage or call arbitrary external functions.

    It is assumed in the compiler that, subject to data dependencies,
    expressions with neither effects nor coeffects may be reordered with
    respect to other expressions.
*)

type effects = No_effects | Only_generative_effects | Arbitrary_effects
type coeffects = No_coeffects | Has_coeffects

(** Describe the semantics of a primitive.  This does not take into account of
    the (non-)(co)effectfulness of the arguments in a primitive application.
    To determine whether such an application is (co)effectful, the arguments
    must also be analysed. *)
val for_primitive: Lambda.primitive -> effects * coeffects

type return_type =
  | Float
  | Other

val return_type_of_primitive: Lambda.primitive -> return_type
