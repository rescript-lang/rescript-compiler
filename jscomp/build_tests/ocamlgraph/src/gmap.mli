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

(** Graph mapping. Map a graph to another one. *)

(** {2 Mapping of vertices} *)

(** Signature for the source graph. *)
module type V_SRC = sig
  type t
  module V : Sig.HASHABLE
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
end

(** Signature for the destination graph. *)
module type V_DST = sig
  type t
  type vertex
  val empty : unit -> t
  val add_vertex : t -> vertex -> t
end

(** Provide a mapping function from a mapping of vertices. *)
module Vertex(G_Src : V_SRC)(G_Dst : V_DST) : sig

  val map : (G_Src.V.t -> G_Dst.vertex) -> G_Src.t -> G_Dst.t
  (** [map f g] applies [f] to each vertex of [g] and so builds a new graph
      based on [g] *)

  val filter_map : (G_Src.V.t -> G_Dst.vertex option) -> G_Src.t -> G_Dst.t
  (** [filter_map f g] applies [f] to each vertex of [g] and so
      builds a new graph based on [g]; if [None] is returned by [f]
      the vertex is omitted in the new graph. *)

end

(** {2 Mapping of edges} *)

(** Signature for the source graph. *)
module type E_SRC = sig
  type t
  module E : Sig.ORDERED_TYPE
  val fold_edges_e : (E.t -> 'a -> 'a) -> t -> 'a -> 'a
end

(** Signature for the destination graph. *)
module type E_DST = sig
  type t
  type edge
  val empty : unit -> t
  val add_edge_e : t -> edge -> t
end

(** Provide a mapping function from a mapping of edges. *)
module Edge(G_Src: E_SRC)(G_Dst: E_DST) : sig

  val map : (G_Src.E.t -> G_Dst.edge) -> G_Src.t -> G_Dst.t
  (** [map f g] applies [f] to each edge of [g] and so builds a new graph
      based on [g] *)

  val filter_map : (G_Src.E.t -> G_Dst.edge option) -> G_Src.t -> G_Dst.t
  (** [filter_map f g] applies [f] to each edge of [g] and so builds
      a new graph based on [g]; if [None] is returned by [f] the
      edge is omitted in the new graph. *)

end
