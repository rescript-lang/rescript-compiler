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

(* $Id: sig_pack.mli,v 1.23 2005-07-18 07:10:35 filliatr Exp $ *)

(** Immediate access to the library: contain a signature gathering an
    imperative graph signature and all algorithms.
    Vertices and edges are labeled with integers. *)

(** Signature gathering an imperative graph signature and all algorithms.
    Vertices and edges are labeled with integers. *)
module type S = sig

  (** {2 Graph structure} *)

  (** abstract type of graphs *)
  type t

  (** Vertices *)
  module V : sig
    (** Vertices are [COMPARABLE] *)

    type t
    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool

    (** vertices are labeled with integers *)

    type label = int
    val create : label -> t
    val label : t -> label
  end

  type vertex = V.t

  (** Edges *)
  module E : sig
    (** Edges are [ORDERED]. *)

    type t
    val compare : t -> t -> int

    (** Edges are directed. *)

    val src : t -> V.t
    val dst : t -> V.t

    (** Edges are labeled with integers. *)

    type label = int
    val create : V.t -> label -> V.t -> t
    (** [create v1 l v2] creates an edge from [v1] to [v2] with label [l] *)
    val label : t -> label

    type vertex = V.t
  end

  type edge = E.t

  (** is this an implementation of directed graphs? *)
  val is_directed : bool

  (** {2 Graph constructors and destructors} *)

  val create : ?size:int -> unit -> t
  (** Return an empty graph. Optionally, a size can be
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

  val add_vertex : t -> V.t -> unit
  (** [add_vertex g v] adds the vertex [v] from the graph [g].
      Do nothing if [v] is already in [g]. *)

  val remove_vertex : t -> V.t -> unit
  (** [remove g v] removes the vertex [v] from the graph [g]
      (and all the edges going from [v] in [g]).
      Do nothing if [v] is not in [g]. *)

  val add_edge : t -> V.t -> V.t -> unit
  (** [add_edge g v1 v2] adds an edge from the vertex [v1] to the vertex [v2]
      in the graph [g].
      Add also [v1] (resp. [v2]) in [g] if [v1] (resp. [v2]) is not in [g].
      Do nothing if this edge is already in [g]. *)

  val add_edge_e : t -> E.t -> unit
  (** [add_edge_e g e] adds the edge [e] in the graph [g].
      Add also [E.src e] (resp. [E.dst e]) in [g] if [E.src e] (resp. [E.dst
      e]) is not in [g].
      Do nothing if [e] is already in [g]. *)

  val remove_edge : t -> V.t -> V.t -> unit
  (** [remove_edge g v1 v2] removes the edge going from [v1] to [v2] from the
      graph [g].
      Do nothing if this edge is not in [g].
      @raise Invalid_argument if [v1] or [v2] are not in [g]. *)

  val remove_edge_e : t -> E.t -> unit
  (** [remove_edge_e g e] removes the edge [e] from the graph [g].
      Do nothing if [e] is not in [g].
      @raise Invalid_argument if [E.src e] or [E.dst e] are not in [g]. *)

  (** Vertices contains integers marks, which can be set or used by some
      algorithms (see for instance module [Marking] below) *)
  module Mark : sig
    type graph = t
    type vertex = V.t
    val clear : t -> unit
    (** [clear g] sets all marks to 0 from all the vertives of [g]. *)
    val get : V.t -> int
    val set : V.t -> int -> unit
  end

  (** {2 Size functions} *)

  val is_empty : t -> bool
  val nb_vertex : t -> int
  val nb_edges : t -> int

  (** Degree of a vertex *)

  val out_degree : t -> V.t -> int
  (** [out_degree g v] returns the out-degree of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. *)

  val in_degree : t -> V.t -> int
  (** [in_degree g v] returns the in-degree of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. *)

  (** {2 Membership functions} *)

  val mem_vertex : t -> V.t -> bool
  val mem_edge : t -> V.t -> V.t -> bool
  val mem_edge_e : t -> E.t -> bool
  val find_edge : t -> V.t -> V.t -> E.t
  val find_all_edges : t -> V.t -> V.t -> E.t list

  (** {2 Successors and predecessors of a vertex} *)

  val succ : t -> V.t -> V.t list
  (** [succ g v] returns the successors of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. *)

  val pred : t -> V.t -> V.t list
  (** [pred g v] returns the predecessors of [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. *)

  (** Labeled edges going from/to a vertex *)

  val succ_e : t -> V.t -> E.t list
  (** [succ_e g v] returns the edges going from [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. *)

  val pred_e : t -> V.t -> E.t list
  (** [pred_e g v] returns the edges going to [v] in [g].
      @raise Invalid_argument if [v] is not in [g]. *)

  (** {2 Graph iterators} *)

  (** iter/fold on all vertices/edges of a graph *)

  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges : (V.t -> V.t -> unit) -> t -> unit
  val fold_vertex : (V.t -> 'a -> 'a) -> t  -> 'a -> 'a
  val fold_edges : (V.t -> V.t -> 'a -> 'a) -> t -> 'a -> 'a

  (** map iterator on vertex *)
  val map_vertex : (V.t -> V.t) -> t -> t

  (** iter/fold on all labeled edges of a graph *)

  val iter_edges_e : (E.t -> unit) -> t -> unit
  val fold_edges_e : (E.t -> 'a -> 'a) -> t -> 'a -> 'a

  (** {2 Vertex iterators}

      Each iterator [iterator f v g] iters [f] to the successors/predecessors
      of [v] in the graph [g] and raises [Invalid_argument] if [v] is not in
      [g]. *)

  (** iter/fold on all successors/predecessors of a vertex. *)

  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val iter_pred : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val fold_pred : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a

  (** iter/fold on all edges going from/to a vertex. *)

  val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
  val fold_succ_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val iter_pred_e : (E.t -> unit) -> t -> V.t -> unit
  val fold_pred_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a

  (** {2 Basic operations} *)

  val find_vertex : t -> int -> V.t
  (** [vertex g i] returns a vertex of label [i] in [g]. The behaviour is
      unspecified if [g] has several vertices with label [i].
      Note: this function is inefficient (linear in the number of vertices);
      you should better keep the vertices as long as you create them. *)

  val transitive_closure : ?reflexive:bool -> t -> t
  (** [transitive_closure ?reflexive g] returns the transitive closure
      of [g] (as a new graph). Loops (i.e. edges from a vertex to itself)
      are added only if [reflexive] is [true] (default is [false]). *)

  val add_transitive_closure : ?reflexive:bool -> t -> t
  (** [add_transitive_closure ?reflexive g] replaces [g] by its
      transitive closure. Meaningless for persistent implementations
      (then acts as [transitive_closure]). *)

  val transitive_reduction : ?reflexive:bool -> t -> t
  (** [transitive_reduction ?reflexive g] returns the transitive reduction
      of [g] (as a new graph). Loops (i.e. edges from a vertex to itself)
      are removed only if [reflexive] is [true] (default is [false]). *)

  val replace_by_transitive_reduction : ?reflexive:bool -> t -> t
  (** [replace_by_transitive_reduction ?reflexive g] replaces [g] by its
      transitive reduction. Meaningless for persistent implementations
      (then acts as [transitive_reduction]). *)

  val mirror : t -> t
  (** [mirror g] returns a new graph which is the mirror image of [g]:
      each edge from [u] to [v] has been replaced by an edge from [v] to [u].
      For undirected graphs, it simply returns a copy of [g]. *)

  val complement : t -> t
  (** [complement g] builds a new graph which is the complement of [g]:
      each edge present in [g] is not present in the resulting graph and
      vice-versa. Edges of the returned graph are unlabeled. *)

  val intersect : t -> t -> t
  (** [intersect g1 g2] returns a new graph which is the intersection of [g1]
      and [g2]: each vertex and edge present in [g1] *and* [g2] is present
      in the resulting graph. *)

  val union : t -> t -> t
  (** [union g1 g2] returns a new graph which is the union of [g1] and [g2]:
      each vertex and edge present in [g1] *or* [g2] is present in the
      resulting graph. *)

  (** {2 Traversal} *)

  (** Depth-first search *)
  module Dfs : sig
    val iter : ?pre:(V.t -> unit) ->
      ?post:(V.t -> unit) -> t -> unit
    (** [iter pre post g] visits all nodes of [g] in depth-first search,
        applying [pre] to each visited node before its successors,
        and [post] after them. Each node is visited exactly once. *)
    val prefix : (V.t -> unit) -> t -> unit
    (** applies only a prefix function *)
    val postfix : (V.t -> unit) -> t -> unit
    (** applies only a postfix function *)

    (** Same thing, but for a single connected component *)

    val iter_component :
      ?pre:(V.t -> unit) ->
      ?post:(V.t -> unit) -> t -> V.t -> unit
    val prefix_component : (V.t -> unit) -> t -> V.t -> unit
    val postfix_component : (V.t -> unit) -> t -> V.t -> unit

    val has_cycle : t -> bool
  end

  (** Breadth-first search *)
  module Bfs : sig
    val iter : (V.t -> unit) -> t -> unit
    val iter_component : (V.t -> unit) -> t -> V.t -> unit
  end

  (** Graph traversal with marking *)
  module Marking : sig
    val dfs : t -> unit
    val has_cycle : t -> bool
  end

  (** {2 Graph generators} *)

  (** Classic graphs *)
  module Classic : sig
    val divisors : int -> t
    (** [divisors n] builds the graph of divisors.
        Vertices are integers from [2] to [n]. [i] is connected to [j] if
        and only if [i] divides [j].
        @raise Invalid_argument is [n < 2]. *)

    val de_bruijn : int -> t
    (** [de_bruijn n] builds the de Bruijn graph of order [n].
        Vertices are bit sequences of length [n] (encoded as their
        interpretation as binary integers). The sequence [xw] is connected
        to the sequence [wy] for any bits [x] and [y] and any bit sequence
        [w] of length [n-1].
        @raise Invalid_argument is [n < 1] or [n > Sys.word_size-1]. *)

    val vertex_only : int -> t
    (** [vertex_only n] builds a graph with [n] vertices and no edge. *)

    val full : ?self:bool -> int -> t
    (** [full n] builds a graph with [n] vertices and all possible edges.
        The optional argument [self] indicates if loop edges should be added
        (default value is [true]). *)
  end

  (** Random graphs *)
  module Rand : sig
    val graph : ?loops:bool -> v:int -> e:int -> unit -> t
    (** [random v e] generates a random with [v] vertices and [e] edges. *)

    val labeled :
      (V.t -> V.t -> E.label) ->
      ?loops:bool -> v:int -> e:int -> unit -> t
    (** [random_labeled f] is similar to [random] except that edges are
            labeled using function [f] *)

    val gnp : ?loops:bool -> v:int -> prob:float -> unit -> t
    (** [gnp v prob] generates a random graph with [v] vertices and
        where each edge is selected with probality [prob] (G(n,p) model) *)

    val gnp_labeled :
      (V.t -> V.t -> E.label) ->
      ?loops:bool -> v:int -> prob:float -> unit -> t
      (** [gnp_labeled add_edge v prob] is similar to [gnp] except that
          edges are labeled using function [f] *)

  end

  (** Strongly connected components *)
  module Components : sig
    val scc : t -> int * (V.t -> int)
    (** strongly connected components *)
    val scc_array : t -> V.t list array
    val scc_list : t -> V.t list list
  end

  (** {2 Classical algorithms} *)

  val shortest_path : t -> V.t -> V.t -> E.t list * int
  (** Dijkstra's shortest path algorithm. Weights are the labels. *)

  val ford_fulkerson : t -> V.t -> V.t -> (E.t -> int) * int
  (** Ford Fulkerson maximum flow algorithm *)

  val goldberg : t -> V.t -> V.t -> (E.t -> int) * int
  (** Goldberg maximum flow algorithm *)

  val bellman_ford : t -> V.t -> E.t list
  (** [bellman_ford g v] finds a negative cycle from [v], and returns it,
      or raises [Not_found] if there is no such cycle *)

  (** Path checking *)
  module PathCheck : sig
    type path_checker
    val create : t -> path_checker
    val check_path : path_checker -> V.t -> V.t -> bool
  end

  (** Topological order *)
  module Topological : sig
    val fold : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
    val iter : (V.t -> unit) -> t -> unit

    val fold_stable : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_stable : (V.t -> unit) -> t -> unit
  end

  val spanningtree : t -> E.t list
  (** Kruskal algorithm *)

  (** {2 Input / Output} *)

  val dot_output : t -> string -> unit
  (** DOT output in a file *)

  val display_with_gv : t -> unit
  (** Displays the given graph using the external tools "dot" and "gv"
      and returns when gv's window is closed *)

  val parse_gml_file : string -> t
  val parse_dot_file : string -> t

  val print_gml : Format.formatter -> t -> unit
  val print_gml_file : t -> string -> unit
  (* val print_graphml : Format.formatter -> t -> unit *)

end
