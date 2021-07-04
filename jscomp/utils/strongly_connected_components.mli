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

(** Kosaraju's algorithm for strongly connected components. *)

module type S = sig
  module Id : Identifiable.S

  type directed_graph = Id.Set.t Id.Map.t
  (** If (a -> set) belongs to the map, it means that there are edges
      from [a] to every element of [set].  It is assumed that no edge
      points to a vertex not represented in the map. *)

  type component =
    | Has_loop of Id.t list
    | No_loop of Id.t

  val connected_components_sorted_from_roots_to_leaf
     : directed_graph
    -> component array

  val component_graph : directed_graph -> (component * int list) array
end

module Make (Id : Identifiable.S) : S with module Id := Id
