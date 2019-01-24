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

(* Keys representing value descriptions that may be written into
   intermediate files and loaded by a dependent compilation unit.
   These keys are used to ensure maximal sharing of value descriptions,
   which may be substantial. *)

include Identifiable.S

val create : ?name:string -> Compilation_unit.t -> t
val name : t -> string option
val get_compilation_unit : t -> Compilation_unit.t
