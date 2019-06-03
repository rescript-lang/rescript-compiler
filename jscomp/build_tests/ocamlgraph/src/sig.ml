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

(** {b Signatures for graph implementations.} *)

(** {2 Signature for ordered and hashable types} *)

(** Signature with only an abstract type. *)
module type ANY_TYPE = sig type t end

(** Signature equivalent to [Set.OrderedType]. *)
module type ORDERED_TYPE = sig type t val compare : t -> t -> int end

(** Signature equivalent to [Set.OrderedType] with a default value. *)
module type ORDERED_TYPE_DFT = sig include ORDERED_TYPE val default : t end

(** Signature equivalent to [Hashtbl.HashedType]. *)
module type HASHABLE = sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

(** Signature merging {!ORDERED_TYPE} and {!HASHABLE}. *)
module type COMPARABLE = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
end

(** {2 Signatures for graph implementations} *)

(** Signature for vertices. *)
module type VERTEX = sig

  (** Vertices are {!COMPARABLE}. *)

  type t

  include COMPARABLE with type t := t

  (** Vertices are labeled. *)

  type label
  val create : label -> t
  val label : t -> label

end

(** Signature for edges. *)
module type EDGE = sig

  (** Edges are {!ORDERED_TYPE}. *)

  type t
  val compare : t -> t -> int

  (** Edges are directed. *)

  type vertex

  val src : t -> vertex
  (** Edge origin. *)
  val dst : t -> vertex
  (** Edge destination. *)

  (** Edges are labeled. *)

  type label
  val create : vertex -> label -> vertex -> t
  (** [create v1 l v2] creates an edge from [v1] to [v2] with label [l] *)
  val label : t -> label
  (** Get the label of an edge. *)

end

(** Common signature for all graphs. *)
module type G = sig

  (** {2 Graph structure} *)

  (** Abstract type of graphs *)
  type t

  (** Vertices have type [V.t] and are labeled with type [V.label]
      (note that an implementation may identify the vertex with its
      label) *)
  module V : VERTEX
  type vertex = V.t

  (** Edges have type [E.t] and are labeled with type [E.label].
      [src] (resp. [dst]) returns the origin (resp. the destination) of a
      given edge. *)
  module E : EDGE with type vertex = vertex
  type edge = E.t

  (** Is this an implementation of directed graphs? *)
  val is_directed : bool

  (** {2 Size functions} *)

  val is_empty : t -> bool
  val nb_vertex : t -> int
  val nb_edges : t -> int

  (** Degree of a vertex *)

  val out_degree : t -> vertex -> int
  (** [out_degree g v] returns the out-degree of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. *)

  val in_degree : t -> vertex -> int
  (** [in_degree g v] returns the in-degree of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. *)

  (** {2 Membership functions} *)

  val mem_vertex : t -> vertex -> bool
  val mem_edge : t -> vertex -> vertex -> bool
  val mem_edge_e : t -> edge -> bool

  val find_edge : t -> vertex -> vertex -> edge
  (** [find_edge g v1 v2] returns the edge from [v1] to [v2] if it exists.
      Unspecified behaviour if [g] has several edges from [v1] to [v2].
      @raise Not_found if no such edge exists. *)

  val find_all_edges : t -> vertex -> vertex -> edge list
  (** [find_all_edges g v1 v2] returns all the edges from [v1] to [v2].
      @since ocamlgraph 1.8 *)

  (** {2 Successors and predecessors}

      You should better use iterators on successors/predecessors (see
      Section "Vertex iterators"). *)

  val succ : t -> vertex -> vertex list
  (** [succ g v] returns the successors of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. *)

  val pred : t -> vertex -> vertex list
  (** [pred g v] returns the predecessors of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. *)

  (** Labeled edges going from/to a vertex *)

  val succ_e : t -> vertex -> edge list
  (** [succ_e g v] returns the edges going from [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. *)

  val pred_e : t -> vertex -> edge list
  (** [pred_e g v] returns the edges going to [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. *)

  (** {2 Graph iterators} *)

  val iter_vertex : (vertex -> unit) -> t -> unit
  (** Iter on all vertices of a graph. *)

  val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold on all vertices of a graph. *)

  val iter_edges : (vertex -> vertex -> unit) -> t -> unit
  (** Iter on all edges of a graph. Edge label is ignored. *)

  val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold on all edges of a graph. Edge label is ignored. *)

  val iter_edges_e : (edge -> unit) -> t -> unit
  (** Iter on all edges of a graph. *)

  val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold on all edges of a graph. *)

  val map_vertex : (vertex -> vertex) -> t -> t
  (** Map on all vertices of a graph. *)

  (** {2 Vertex iterators}

      Each iterator [iterator f v g] iters [f] to the successors/predecessors
      of [v] in the graph [g] and raises [Invalid_argument] if [v] is not in
      [g]. It is the same for functions [fold_*] which use an additional
      accumulator.

      <b>Time complexity for ocamlgraph implementations:</b>
      operations on successors are in O(1) amortized for imperative graphs and
      in O(ln(|V|)) for persistent graphs while operations on predecessors are
      in O(max(|V|,|E|)) for imperative graphs and in O(max(|V|,|E|)*ln|V|) for
      persistent graphs. *)

  (** iter/fold on all successors/predecessors of a vertex. *)

  val iter_succ : (vertex -> unit) -> t -> vertex -> unit
  val iter_pred : (vertex -> unit) -> t -> vertex -> unit
  val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

  (** iter/fold on all edges going from/to a vertex. *)

  val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
  val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
  val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
  val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a

end

(** Signature for persistent (i.e. immutable) graph. *)
module type P = sig

  include G
  (** A persistent graph is a graph. *)

  val empty : t
  (** The empty graph. *)

  val add_vertex : t -> vertex -> t
  (** [add_vertex g v] adds the vertex [v] to the graph [g].
      Just return [g] if [v] is already in [g]. *)

  val remove_vertex : t -> vertex -> t
  (** [remove g v] removes the vertex [v] from the graph [g]
      (and all the edges going from [v] in [g]).
      Just return [g] if [v] is not in [g].

      <b>Time complexity for ocamlgraph implementations:</b>
      O(|V|*ln(|V|)) for unlabeled graphs and
      O(|V|*max(ln(|V|),D)) for labeled graphs.
      D is the maximal degree of the graph. *)

  val add_edge : t -> vertex -> vertex -> t
  (** [add_edge g v1 v2] adds an edge from the vertex [v1] to the vertex [v2]
      in the graph [g].
      Add also [v1] (resp. [v2]) in [g] if [v1] (resp. [v2]) is not in [g].
      Just return [g] if this edge is already in [g]. *)

  val add_edge_e : t -> edge -> t
  (** [add_edge_e g e] adds the edge [e] in the graph [g].
      Add also [E.src e] (resp. [E.dst e]) in [g] if [E.src e] (resp. [E.dst
      e]) is not in [g].
      Just return [g] if [e] is already in [g]. *)

  val remove_edge : t -> vertex -> vertex -> t
  (** [remove_edge g v1 v2] removes the edge going from [v1] to [v2] from the
      graph [g]. If the graph is labelled, all the edges going from [v1] to
      [v2] are removed from [g].
      Just return [g] if this edge is not in [g].
      @raise Invalid_argument if [v1] or [v2] are not in [g]. *)

  val remove_edge_e : t -> edge -> t
  (** [remove_edge_e g e] removes the edge [e] from the graph [g].
      Just return [g] if [e] is not in [g].
      @raise Invalid_argument if [E.src e] or [E.dst e] are not in [g]. *)

end

(** Signature for imperative (i.e. mutable) graphs. *)
module type I = sig

  include G
  (** An imperative graph is a graph. *)

  val create : ?size:int -> unit -> t
  (** [create ()] returns an empty graph. Optionally, a size can be
      given, which should be on the order of the expected number of
      vertices that will be in the graph (for hash tables-based
      implementations).  The graph grows as needed, so [size] is
      just an initial guess. *)

  val clear: t -> unit
  (** Remove all vertices and edges from the given graph.
      @since ocamlgraph 1.4 *)

  val copy : t -> t
  (** [copy g] returns a copy of [g]. Vertices and edges (and eventually
      marks, see module [Mark]) are duplicated. *)

  val add_vertex : t -> vertex -> unit
  (** [add_vertex g v] adds the vertex [v] to the graph [g].
      Do nothing if [v] is already in [g]. *)

  val remove_vertex : t -> vertex -> unit
  (** [remove g v] removes the vertex [v] from the graph [g]
      (and all the edges going from [v] in [g]).
      Do nothing if [v] is not in [g].

      <b>Time complexity for ocamlgraph implementations:</b>
      O(|V|*ln(D)) for unlabeled graphs and O(|V|*D)  for
      labeled graphs. D is the maximal degree of the graph. *)

  val add_edge : t -> vertex -> vertex -> unit
  (** [add_edge g v1 v2] adds an edge from the vertex [v1] to the vertex [v2]
      in the graph [g].
      Add also [v1] (resp. [v2]) in [g] if [v1] (resp. [v2]) is not in [g].
      Do nothing if this edge is already in [g]. *)

  val add_edge_e : t -> edge -> unit
  (** [add_edge_e g e] adds the edge [e] in the graph [g].
      Add also [E.src e] (resp. [E.dst e]) in [g] if [E.src e] (resp. [E.dst
      e]) is not in [g].
      Do nothing if [e] is already in [g]. *)

  val remove_edge : t -> vertex -> vertex -> unit
  (** [remove_edge g v1 v2] removes the edge going from [v1] to [v2] from the
      graph [g]. If the graph is labelled, all the edges going from [v1] to
      [v2] are removed from [g].
      Do nothing if this edge is not in [g].
      @raise Invalid_argument if [v1] or [v2] are not in [g]. *)

  val remove_edge_e : t -> edge -> unit
  (** [remove_edge_e g e] removes the edge [e] from the graph [g].
      Do nothing if [e] is not in [g].
      @raise Invalid_argument if [E.src e] or [E.dst e] are not in [g]. *)

end

(** Signature for edges' weights. *)
module type WEIGHT = sig
  type edge
  (** Type for graph edges. *)
  type t
  (** Type of edges' weights. *)
  val weight : edge -> t
  (** Get the weight of an edge. *)
  val compare : t -> t -> int
  (** Weights must be ordered. *)
  val add : t -> t -> t
  (** Addition of weights. *)
  val zero : t
  (** Neutral element for {!add}. *)
end

(** Signature for marks on vertices. *)
module type MARK = sig
  type graph
  (** Type of graphs. *)
  type vertex
  (** Type of graph vertices. *)
  val clear : graph -> unit
  (** [clear g] sets all the marks to 0 for all the vertices of [g]. *)
  val get : vertex -> int
  (** Mark value (in O(1)). *)
  val set : vertex -> int -> unit
  (** Set the mark of the given vertex. *)
end

(** Signature for imperative graphs with marks on vertices. *)
module type IM = sig
  include I
  (** An imperative graph with marks is an imperative graph. *)

  (** Mark on vertices.
      Marks can be used if you want to store some information on vertices:
      it is more efficient to use marks than an external table. *)
  module Mark : MARK with type graph = t and type vertex = vertex
end

(*
Local Variables:
compile-command: "make -C .."
End:
*)
