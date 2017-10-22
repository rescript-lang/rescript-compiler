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

(* Copyright (c) 2010 - 2012 Technische Universitaet Muenchen
 * Markus W. Weissmann <markus.weissmann@in.tum.de>
 * All rights reserved. *)

(** Fixpoint computation implemented using the work list algorithm.
    This module makes writing data-flow analysis easy.

    One of the simplest fixpoint analysis is that of reachability.
    Given a directed graph module [G], its analysis can be implemented
    as follows:

    {[
      module Reachability = Graph.Fixpoint.Make(G)
          (struct
            type vertex = G.E.vertex
            type edge = G.E.t
            type g = G.t
            type data = bool
            let direction = Graph.Fixpoint.Forward
            let equal = (=)
            let join = (||)
            let analyze _ = (fun x -> x)
          end)
    ]}

    The types for [vertex], [edge] and [g] are those of the graph to be
    analyzed.  The [data] type is [bool]: It will tell if the
    vertex is reachable from the start vertex.  The [equal] operation
    for [bool] is simply structural equality; the [join] operation is
    logical or.  The [analyze] function is very simple, too: If the
    predecessor vertex is reachable, so is the successor vertex of the
    edge.

    To use the analysis, an instance of a graph [g] is required.  For
    this analysis a predicate [is_root_vertex : G.E.vertex -> bool] is
    required to initialize the reachability of the root vertex to
    [true] and of all other vertices to [false].

    {[
      let g = ...
        let result = Reachability.analyze is_root_vertex g
    ]}

    The [result] is a map of type [G.E.vertex -> bool] that can be
    queried for every vertex to tell if the vertex is reachable from
    the root vertex.

    @author Markus W. Weissmann
    @see "Introduction to Lattices and Order" B. A. Davey and H. A. Priestley, Cambridge University Press, 2002
    @see "Fixed Point Theory" Andrzej Granas and James Dugundji, Springer, 2003
    @see "Principles of Program Analysis" Flemming Nielson, Hanne Riis Nielson and Chris Hankin, Springer, 2005
    @see "Ubersetzerbau 3: Analyse und Transformation" Reinhard Wilhelm and Helmut Seidl, Springer, 2010
*)

(** Minimal graph signature for work list algorithm *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  module E : sig
    type t
    val dst : t -> V.t
    val src : t -> V.t
  end
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val succ_e : t -> V.t -> E.t list
  val pred_e : t -> V.t -> E.t list
  val succ : t -> V.t -> V.t list
  val pred : t -> V.t -> V.t list
end

type direction = Forward | Backward
(** Type of an analysis *)

module type Analysis = sig
  type data
  (** information stored at each vertex *)
  type edge
  (** type of edges of the underlying graph *)
  type vertex
  (** type of vertices of the underlying graph *)
  type g
  (** type of the underlying graph *)
  val direction : direction
  (** the direction of the analysis *)
  val join : data -> data -> data
  (** operation how to join data when paths meet *)
  val equal : data -> data -> bool
  (** predicate to determine the fixpoint *)
  val analyze : edge -> data -> data
  (** the actual analysis of one edge; provided the edge and the incoming
      data, it needs to compute the outgoing data *)
end

module Make
    (G : G)
    (A : Analysis with type g = G.t with type edge = G.E.t
     with type vertex = G.V.t) :
sig
  val analyze : (G.V.t -> A.data) -> A.g -> (G.V.t -> A.data)
  (** [analyze f g] computes the fixpoint on the given graph using the
      work list algorithm. Beware that a misconstructed Analysis will
      not terminate! [f] is used to create the initial analysis
      data. The function returned is a map to see what data was computed
      for which node.

      Beware of applying function [analyze] partially, to arguments
      [f] and [g] only. The result is a function that is to be used to query
      the result of the analysis. *)
end
