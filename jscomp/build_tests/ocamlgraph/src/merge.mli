(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2012                                               *)
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

(** Provides functions to extend any module satisfying one of the signatures
    Sig.P, Sig.I and Builder.S .*)

module type S = sig

  type graph
  type vertex
  type edge
  type edge_label

  val merge_vertex: graph -> vertex list -> graph
  (** If no element of [vl] belongs to [g] then [merge_vertex g (v::vl)] is the
      graph [g]. Otherwise the collection of vertices of [merge_vertex g
      (v::vl)] is the collection of vertices of [g] from which all the elements
      of [vl] were removed and to which [v] was added. Any edge of [merge_vertex
      g (v::vl)] is an edge of [g] whose source (destination) was changed to [v]
      if it belongs to [vl]. The function [merge_vertex] always returns a graph
      with a smaller collection of vertices and a smaller collection of edges
      (in the weak sense).  However the labels appearing in [merge_vertex g
      v::vl] are exactly the ones appearing in [g].*)

  val merge_edges_e: ?src:vertex -> ?dst:vertex -> graph -> edge list -> graph
  (** If no element of [el] belongs to [g] then [merge_edges_e g (e::el)] is the
      graph [g]. Otherwise the collection of vertices of [merge_edges_e g
      (e::el)] is precisely the collection of vertices of [g] from which the
      sources and the destinations of all the elements of [el] were removed and
      to which the vertices [v] and [w] were added. If [dst] was provided then
      [v] is [src] otherwise it is the source of [e]. If [dst] was provided then
      [w] is [y] otherwise it is the destination of [e]. The collection of edges
      of [merge_edges_e g e::el] is precisely the collection of edges of [g]
      from which all the elements of [el] were removed and to which an edge from
      [v] to [w] sharing the label of [e] was added; the edges of [g] being
      understood up to the fact their source and destination were updated. Note
      [v=w] if and only if the source of some element of [el] matches the
      destination of some element of [el] (possibly the same).*)

  val merge_edges_with_label:
    ?src:vertex -> ?dst:vertex -> ?label:edge_label -> graph -> edge_label
    -> graph
  (** The graph [merge_edges_with_label ?src ?tgt ?label g l] is the graph
      [merge_edges_e ?src ?dst g el] with [el] being the list of all edges of
      [g] carrying the label [l]. If the optional value [label] is provided then
      the edge to which all the elements of [el] are identified carries the
      label [label]. Otherwise it carries the label [l]. In particular
      [merge_edges_with_label ?src ?tgt ?label g l] is the graph [g] if and only
      if there is at most one edge of [g] carrying the label [l].*)

  val merge_isolabelled_edges: graph -> graph
  (** The graph [merge_isolabelled_edges g] is obtained from [g] by
      identifying two vertices when they are the sources (destinations) of two
      edges sharing the same label. Therefore two distinct edges of the
      returned graph cannot carry the same label. In particular if all the
      edges share the same label then the returned graph is either empty (if
      [g] is so) or a single vertex (if [g] has no edge and at least one
      vertex) or a single vertex and a single edge (if [g] has both a vertex
      and an edge). A label is carried by some edge of
      [merge_isolabelled_edges g] if and only if it is carried by some edge of
      [g].*)

  val merge_ends: ?strict:bool -> ?specified_vertex:vertex -> graph -> graph
  (** A vertex [v] of [g] is called an end if every edge of [g] arriving to [v]
      also starts from [v]. It is called a strict end if no edge of [g] arrives
      to it. The graph [merge_ends g] is the graph [merge_vertex vl] where [vl]
      is the list of (strict) ends of [g]. The vertex substituted to the ends
      can be specified.*)

  val merge_starts: ?strict:bool -> ?specified_vertex:vertex -> graph -> graph
  (** A vertex [v] of [g] is called a start if every edge of [g] starting from
      [v] also arrives to [v]. It is called a strict start if no edge of [g]
      starts from it. The graph [merge_starts g] is the graph [merge_vertex vl]
      where [vl] is the list of (strict) starts of [g]. The vertex substituted
      to the starts can be specified.*)

  val merge_scc:
    ?loop_killer:bool -> ?specified_vertex:(vertex list -> vertex) -> graph ->
    graph
    (** The vertex of every strongly connected component are identified. If the
        option [loop_killer] is set to [true] then all the edges between identified
        vertices are removed. The option [specified_vertex] allows to choose the
        vertex that replaces the elements of a strongly connected component.*)

end

(** Extension for the module [X].*)
module B(X: Builder.S) : S with type graph = X.G.t
                            and type vertex := X.G.vertex
                            and type edge := X.G.edge
                            and type edge_label = X.G.E.label

(**Extension for the module [G].*)
module P(G: Sig.P): S with type graph = G.t
                       and type vertex := G.vertex
                       and type edge := G.edge
                       and type edge_label = G.E.label

(**Extension for the module [G].*)
module I(G: Sig.I): sig

  (** Same specification than module type {!S} but modify the graph inplace
      instead of returning a new graph. *)

  type graph = G.t
  type vertex = G.vertex
  type edge = G.edge
  type edge_label = G.E.label

  val merge_vertex: graph -> vertex list -> unit
  val merge_edges_e: ?src:vertex -> ?dst:vertex -> graph -> edge list -> unit
  val merge_edges_with_label:
    ?src:vertex -> ?dst:vertex -> ?label:edge_label -> graph -> edge_label
    -> unit
  val merge_isolabelled_edges: graph -> unit
  val merge_ends: ?strict:bool -> ?specified_vertex:vertex -> graph -> unit
  val merge_starts: ?strict:bool -> ?specified_vertex:vertex -> graph -> unit
  val merge_scc:
    ?loop_killer:bool -> ?specified_vertex:(vertex list -> vertex) -> graph ->
    unit

end

(*
Local Variables:
compile-command: "make -C .."
End:
*)

(*OCaml >= 3.12*)
