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

include Identifiable.S

val wrap : Variable.t -> t
val unwrap : t -> Variable.t

val wrap_map : 'a Variable.Map.t -> 'a Map.t
val unwrap_set : Set.t -> Variable.Set.t

val in_compilation_unit : t -> Compilation_unit.t -> bool
val get_compilation_unit : t -> Compilation_unit.t

val unique_name : t -> string

val output_full : out_channel -> t -> unit
