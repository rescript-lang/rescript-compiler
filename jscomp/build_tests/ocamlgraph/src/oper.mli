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

(** Basic operations over graphs *)

(** {2 Basic operations over graphs} *)

module type S = sig

  type g

  val transitive_closure : ?reflexive:bool -> g -> g
  (** [transitive_closure ?reflexive g] returns the transitive closure
      of [g] (as a new graph). Loops (i.e. edges from a vertex to itself)
      are added only if [reflexive] is [true] (default is [false]). *)

  val add_transitive_closure : ?reflexive:bool -> g -> g
  (** [add_transitive_closure ?reflexive g] replaces [g] by its
      transitive closure. Meaningless for persistent implementations
      (then acts as [transitive_closure]). *)

  val transitive_reduction : ?reflexive:bool -> g -> g
  (** [transitive_reduction ?reflexive g] returns the transitive reduction
      of [g] (as a new graph). Loops (i.e. edges from a vertex to itself)
      are removed only if [reflexive] is [true] (default is [false]). *)

  val replace_by_transitive_reduction : ?reflexive:bool -> g -> g
  (** [replace_by_transitive_reduction ?reflexive g] replaces [g] by its
      transitive reduction. Meaningless for persistent implementations
      (then acts as [transitive_reduction]). *)

  val mirror : g -> g
  (** [mirror g] returns a new graph which is the mirror image of [g]:
      each edge from [u] to [v] has been replaced by an edge from [v] to [u].
      For undirected graphs, it simply returns [g].
      Note: Vertices are shared between [g] and [mirror g]; you may need to
      make a copy of [g] before using [mirror] *)

  val complement : g -> g
  (** [complement g] returns a new graph which is the complement of [g]:
      each edge present in [g] is not present in the resulting graph and
      vice-versa. Edges of the returned graph are unlabeled. *)

  val intersect : g -> g -> g
  (** [intersect g1 g2] returns a new graph which is the intersection of [g1]
      and [g2]: each vertex and edge present in [g1] *and* [g2] is present
      in the resulting graph. *)

  val union : g -> g -> g
  (** [union g1 g2] returns a new graph which is the union of [g1] and [g2]:
      each vertex and edge present in [g1] *or* [g2] is present in the
      resulting graph. *)

end

module Make(B : Builder.S) : S with type g = B.G.t
(** Basic operations over graphs *)

module P(G : Sig.P) : S with type g = G.t
(** Basic operations over persistent graphs *)

module I(G : Sig.I) : S with type g = G.t
(** Basic operations over imperative graphs *)

(** {2 Choose} *)

(** Choose an element in a graph *)
module Choose(G : sig
    type t
    type vertex
    type edge
    val iter_vertex : (vertex -> unit) -> t -> unit
    val iter_edges_e : (edge -> unit) -> t -> unit
  end) :
sig

  val choose_vertex : G.t -> G.vertex
  (** [choose_vertex g] returns a vertex from the graph.
      @raise Invalid_argument if the graph is empty. *)

  val choose_edge : G.t -> G.edge
  (** [choose_edge g] returns an edge from the graph.
      @raise Invalid_argument if the graph has no edge. *)

end

(** {2 Neighbourhood} *)

(** Neighbourhood of vertex / vertices *)
module Neighbourhood(G : sig
    type t
    module V : Sig.COMPARABLE
    val fold_succ: (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
    val succ: t -> V.t -> V.t list
  end) :
sig

  module Vertex_Set : Set.S with type elt = G.V.t

  (** The neighbourhood of a vertex [v] is
      \{ v' | (succ g v) and (v <> v') \} *)

  val list_from_vertex : G.t -> G.V.t -> G.V.t list
  (** Neighbourhood of a vertex as a list. *)

  val set_from_vertex : G.t -> G.V.t -> Vertex_Set.t
  (** Neighbourhood of a vertex as a set.
      Less efficient that [list_from_vertex]. *)

  (** The neighbourhood of a set [S] of vertices is [U \ S] where
      [U] is the union of neighbourhoods of each vertex of [S]. *)

  val list_from_vertices : G.t -> G.V.t list -> G.V.t list
  (** Neighbourhood of a list of vertices as a list. *)

  val set_from_vertices : G.t -> G.V.t list -> Vertex_Set.t
  (** Neighbourhood of a list of vertices as a set.
      More efficient that [list_from_vertices]. *)

end
