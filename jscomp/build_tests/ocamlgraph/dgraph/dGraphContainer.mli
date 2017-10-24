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

open Graph

type cluster = string

type status = Global | Tree | Both

class type
  ['vertex, 'edge, 'cluster, 'tree_vertex, 'tree_edge, 'tree_cluster]
    view_container_type =
  object

    method global_view :
      ('vertex, 'edge, 'cluster) DGraphView.view option

    method tree_view:
      ('tree_vertex, 'tree_edge, 'tree_cluster) DGraphView.view option

    method tree_root: 'vertex option

    method depth_backward : int
    method depth_forward : int
    method status : status

    method set_depth_backward : int -> unit
    method set_depth_forward : int -> unit
    method set_tree_root: 'vertex -> unit
    method switch : status -> unit

    method adapt_zoom: unit -> unit
  end

module type S = sig

  type graph
  type vertex
  type edge

  module Tree: Sig.G with type V.label = vertex

  module GView: DGraphView.S with type vertex = vertex
                              and type edge = edge
                              and type cluster = cluster

  module TView: DGraphView.S with type vertex = Tree.V.t
                              and type edge = Tree.E.t
                              and type cluster = cluster

  type global_view = (vertex, edge, cluster) DGraphView.view
  type tree_view = (Tree.V.t, Tree.E.t, cluster) DGraphView.view

  class view_container :
    ?packing:(GObj.widget -> unit)
    -> ?status:status
    -> ?default_callbacks:bool (* register default node callbacks (centering
                                  and highlighting). True by default *)
    -> mk_global_view: (unit -> global_view)
    -> mk_tree_view:
    (depth_backward:int -> depth_forward:int -> Gtk.widget Gtk.obj -> vertex
     -> tree_view)
    -> vertex option
    -> [ vertex, edge, cluster, Tree.V.t, Tree.E.t, cluster]
      view_container_type

end

module Make(G: Graphviz.GraphWithDotAttrs) : sig

  include S with type graph = G.t and type vertex = G.V.t and type edge = G.E.t

  val from_graph :
    ?packing:(GObj.widget -> unit)
    -> ?status:status
    -> ?default_callbacks:bool 
    ->
    ?mk_global_view:
      ((G.V.t, G.E.t, cluster) DGraphModel.abstract_model -> global_view)
    ->
    ?mk_tree_view:
      ((Tree.V.t, Tree.E.t, cluster) DGraphModel.abstract_model -> tree_view)
    -> ?root:G.vertex
    -> G.t
    -> view_container

  val from_graph_with_commands :
    ?packing:(GObj.widget -> unit)
    -> ?status:status
    -> ?default_callbacks:bool 
    ->
    ?mk_global_view:
      ((G.V.t, G.E.t, cluster) DGraphModel.abstract_model -> global_view)
    ->
    ?mk_tree_view:
      ((Tree.V.t, Tree.E.t, cluster) DGraphModel.abstract_model -> tree_view)
    -> ?root:G.vertex
    -> G.t
    -> GPack.table * view_container

end

module Dot : sig

  open DGraphModel

  include S with type graph = DotG.t
             and type vertex = DotG.V.t
             and type edge = DotG.E.t

  val from_dot :
    ?packing:(GObj.widget -> unit)
    -> ?status:status
    -> ?default_callbacks:bool 
    -> ?mk_global_view:
      ((DotG.V.t, DotG.E.t, cluster) abstract_model -> global_view)
    -> ?mk_tree_view:
      ((Tree.V.t, Tree.E.t, cluster) abstract_model -> tree_view)
    -> string (* dot filename *)
    -> view_container

  val from_dot_with_commands :
    ?packing:(GObj.widget -> unit)
    -> ?status:status
    -> ?default_callbacks:bool 
    -> ?mk_global_view:
      ((DotG.V.t, DotG.E.t, cluster) abstract_model -> global_view)
    -> ?mk_tree_view:
      ((Tree.V.t, Tree.E.t, cluster) abstract_model -> tree_view)
    -> string (* dot filename *)
    -> GPack.table * view_container

end
