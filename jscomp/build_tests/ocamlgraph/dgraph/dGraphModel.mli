(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009-2010                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Benoit Bataille  (benoit.bataille@gmail.com)                      *)
(*                                                                        *)
(**************************************************************************)

(** Abstract graph model *)

open XDot
open Graph

exception DotError of string

(** Immutable graph model.
    Layout accessors, iterators and
    membership functions. *)
class type ['vertex, 'edge, 'cluster] abstract_model = object
  method iter_edges : ('vertex -> 'vertex -> unit) -> unit
  method iter_edges_e : ('edge -> unit) -> unit
  method iter_pred : ('vertex -> unit) -> 'vertex -> unit
  method iter_pred_e : ('edge -> unit) -> 'vertex -> unit
  method iter_succ : ('vertex -> unit) -> 'vertex -> unit
  method iter_succ_e : ('edge -> unit) -> 'vertex -> unit
  method iter_vertex : ('vertex -> unit) -> unit
  method iter_clusters : ('cluster -> unit) -> unit
  method iter_associated_vertex : ('vertex -> unit) -> 'vertex -> unit

  (** Membership functions *)
  method find_edge : 'vertex -> 'vertex -> 'edge
  method mem_edge : 'vertex -> 'vertex -> bool
  method mem_edge_e : 'edge -> bool
  method mem_vertex : 'vertex -> bool
  method src : 'edge -> 'vertex
  method dst : 'edge -> 'vertex

  (** Dot layout *)
  method bounding_box : bounding_box
  method get_edge_layout : 'edge -> edge_layout
  method get_vertex_layout : 'vertex -> node_layout
  method get_cluster_layout : 'cluster -> cluster_layout
end

(** This functor creates a model from a graph *)
module Make(G : Graph.Graphviz.GraphWithDotAttrs) : sig

  type cluster = string

  class model:
    XDot.Make(G).graph_layout -> G.t -> [G.V.t, G.E.t, cluster] abstract_model

  val from_graph : ?cmd:string -> ?tmp_name:string -> G.t -> model
  (** Creates a model using graphviz.
      [tmp_name] is the name of the temporary dot files.
      @raise DotError if issue occurs with the generated dot file *)

end


module DotG : Sig.G
  with type V.label = XDot.node_layout and type E.label = XDot.edge_layout


type cluster = string
type dotg_model = (DotG.vertex, DotG.edge, cluster) abstract_model

(** Creates a model from a dot file. *)
val read_dot : ?cmd:string -> string -> dotg_model

(** Creates a model from an xdot file (the layout is not recomputed)*)
val read_xdot : string -> dotg_model
