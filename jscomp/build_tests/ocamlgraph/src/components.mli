(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Strongly connected components. *)

(** Minimal graph signature required by {!Make}.
    Sub-signature of {!Sig.G}. *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
end

(** Functor providing functions to compute strongly connected components of a
    graph. *)
module Make (G: G) : sig

  val scc : G.t -> int * (G.V.t -> int)
  (** [scc g] computes the strongly connected components of [g].
      The result is a pair [(n,f)] where [n] is the number of
      components. Components are numbered from [0] to [n-1], and
      [f] is a function mapping each vertex to its component
      number. In particular, [f u = f v] if and only if [u] and
      [v] are in the same component. Another property of the
      numbering is that components are numbered in a topological
      order: if there is an arc from [u] to [v], then [f u >= f u]

      Not tail-recursive.
      Complexity: O(V+E)
      The function returned has complexity O(1) *)

  val scc_array : G.t -> G.V.t list array
  (** [scc_array g] computes the strongly connected components of [g].
      Components are stored in the resulting array, indexed with a
      numbering with the same properties as for [scc] above. *)

  val scc_list : G.t -> G.V.t list list
  (** [scc_list g] computes the strongly connected components of [g].
      The result is a partition of the set of the vertices of [g].
      The [n]-th components is [(scc_array g).(n-1)]. *)

end

(** Connected components (for undirected graphs).
    The implementation uses union-find. Time complexity is (quasi) O(V+E).
    Space complexity is O(V). *)

module type U = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges : (V.t -> V.t -> unit) -> t -> unit
end

module Undirected(G: U) : sig
  val components: G.t -> int * (G.V.t -> int)
  val components_array: G.t -> G.V.t list array
  val components_list: G.t -> G.V.t list list
end
