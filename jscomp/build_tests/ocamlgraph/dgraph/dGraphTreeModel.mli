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

module type S = sig

  module Tree: Graph.Graphviz.GraphWithDotAttrs

  module TreeManipulation : sig
    type t
    val get_structure : t -> Tree.t
    val get_tree_vertices : Tree.V.label -> t -> Tree.V.t list
    val get_graph_vertex : Tree.V.t -> t -> Tree.V.label
    val is_ghost_node : Tree.V.t -> t -> bool
    val is_ghost_edge : Tree.E.t -> t -> bool
  end

  type cluster = string

  class tree_model :
    XDot.Make(Tree).graph_layout ->
    TreeManipulation.t ->
    [ Tree.V.t, Tree.E.t, cluster ] DGraphModel.abstract_model

  val tree : unit -> TreeManipulation.t

end

(** This functor creates a model centered on a vertex from a graph *)
module SubTreeMake(G : Graph.Graphviz.GraphWithDotAttrs) : sig

  include S with type Tree.V.label = G.V.t

  val from_graph :
    ?depth_forward:int -> ?depth_backward:int ->
    [> `widget] Gtk.obj -> G.t -> G.V.t -> tree_model

end

(** Creates a model centered on a vertex from a dot model *)
module SubTreeDotModelMake : sig

  include S with type Tree.V.label = DGraphModel.DotG.V.t

  val from_model :
    ?depth_forward:int -> ?depth_backward:int
    -> DGraphModel.dotg_model
    -> DGraphModel.DotG.V.t
    -> tree_model

end
