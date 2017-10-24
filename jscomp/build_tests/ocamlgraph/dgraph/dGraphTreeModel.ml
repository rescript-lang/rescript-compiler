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

module type S = sig

  module Tree: Graphviz.GraphWithDotAttrs

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
    [Tree.V.t, Tree.E.t, cluster] DGraphModel.abstract_model

  val tree : unit -> TreeManipulation.t

end

module Build
    (G: Sig.G)
    (T: Graphviz.GraphWithDotAttrs with type V.label = G.V.t)
    (TM: DGraphSubTree.S with type Tree.t = T.t
                          and type Tree.V.t = T.V.t
                          and type Tree.E.t = T.E.t) =
struct

  module TreeManipulation = TM
  type cluster = string

  module X = XDot.Make(T)

  class tree_model layout tree
    : [ T.V.t, T.E.t, cluster ] DGraphModel.abstract_model
    =
    let tree_structure = TM.get_structure tree in
    object

      (* Iterators *)
      method iter_edges f =
        T.iter_edges
          (fun v1 v2 ->
             if not (TM.is_ghost_node v1 tree && TM.is_ghost_node v2 tree) then
               f v1 v2)
          tree_structure

      method iter_edges_e f =
        T.iter_edges_e
          (fun e -> if not (TM.is_ghost_edge e tree) then f e)
          tree_structure

      method iter_pred f v =
        T.iter_pred
          (fun v -> if not (TM.is_ghost_node v tree) then f v)
          tree_structure v

      method iter_pred_e f v =
        T.iter_pred_e
          (fun e -> if not (TM.is_ghost_edge e tree) then f e)
          tree_structure v

      method iter_succ f =
        T.iter_succ
          (fun v -> if not (TM.is_ghost_node v tree) then f v)
          tree_structure

      method iter_succ_e f =
        T.iter_succ_e
          (fun e -> if not (TM.is_ghost_edge e tree) then f e)
          tree_structure

      method iter_vertex f =
        T.iter_vertex
          (fun v -> if not (TM.is_ghost_node v tree) then f v)
          tree_structure

      method iter_associated_vertex f v =
        let origin_vertex = TM.get_graph_vertex v tree in
        List.iter
          (fun v -> if not (TM.is_ghost_node v tree) then f v)
          (TM.get_tree_vertices origin_vertex tree)

      method iter_clusters f =
        Hashtbl.iter (fun k _ -> f k) layout.X.cluster_layouts

      (* Membership functions *)
      method find_edge =
        try T.find_edge tree_structure
        with Not_found -> assert false

      method mem_edge = T.mem_edge tree_structure
      method mem_edge_e = T.mem_edge_e tree_structure
      method mem_vertex = T.mem_vertex tree_structure
      method src = T.E.src
      method dst = T.E.dst

      (* Layout *)
      method bounding_box = layout.X.bbox

      method get_vertex_layout v =
        try X.HV.find layout.X.vertex_layouts v
        with Not_found -> assert false

      method get_edge_layout e =
        try X.HE.find layout.X.edge_layouts e
        with Not_found -> assert false

      method get_cluster_layout c =
        try Hashtbl.find layout.X.cluster_layouts c
        with Not_found -> assert false

    end

end

module SubTreeMake(G: Graphviz.GraphWithDotAttrs) = struct

  module T = Imperative.Digraph.Abstract(G.V)
  module TM = DGraphSubTree.Make(G)(T)

  let tree_ref : TM.t option ref = ref None
  let tree () = match !tree_ref with None -> assert false | Some t -> t

  let graph_ref: G.t option ref = ref None
  let graph () = match !graph_ref with None -> assert false | Some g -> g

  module Tree = struct

    include T

    let graph_attributes _ = G.graph_attributes (graph ())
    let default_vertex_attributes _ = G.default_vertex_attributes (graph ())
    let default_edge_attributes _ = G.default_edge_attributes (graph ())

    let cpt = ref 0
    let name_table = Hashtbl.create 97
    let vertex_name v =
      try Hashtbl.find name_table v
      with Not_found ->
        incr cpt;
        Hashtbl.add name_table v (string_of_int !cpt);
        string_of_int !cpt

    let vertex_attributes v =
      let t = tree () in
      if TM.is_ghost_node v t then [ `Style `Invis ]
      else G.vertex_attributes (TM.get_graph_vertex v t)

    let edge_attributes e =
      let t = tree () in
      if TM.is_ghost_node (T.E.src e) t || TM.is_ghost_node (T.E.dst e) t then
        [ `Style `Dashed; `Dir `None ]
      else
        G.edge_attributes
          (G.find_edge
             (graph ())
             (TM.get_graph_vertex (T.E.src e) t)
             (TM.get_graph_vertex (T.E.dst e) t))

    let get_subgraph v =
      let t = tree () in
      if TM.is_ghost_node v t then None
      else G.get_subgraph (TM.get_graph_vertex v t)

  end

  include Build(G)(Tree)(TM)

  module TreeLayout =
    DGraphTreeLayout.Make
      (Tree)
      (struct let is_ghost_node v = TM.is_ghost_node v (tree ()) end)

  let from_graph
      ?(depth_forward=2)
      ?(depth_backward=2)
      context
      g
      v
    =
    (* Generate subtree *)
    let t = TM.make g v depth_forward depth_backward in
    tree_ref := Some t;
    graph_ref := Some g;
    let layout =
      TreeLayout.from_tree context (TM.get_structure t) (TM.get_root t)
    in
    new tree_model layout t

end

module SubTreeDotModelMake = struct

  module T = Imperative.Digraph.Abstract(DGraphModel.DotG.V)
  module TM = DGraphSubTree.Make_from_dot_model(T)

  let tree_ref : TM.t option ref = ref None
  let tree () = match !tree_ref with None -> assert false | Some t -> t

  module TreeLayout =
    DGraphTreeLayout.MakeFromDotModel
      (T)
      (struct let is_ghost_node v = TM.is_ghost_node v (tree ()) end)

  include TreeLayout
  include Build(DGraphModel.DotG)(Tree)(TM)

  let from_model ?(depth_forward=2) ?(depth_backward=2) model v =
    let t = TM.make model v depth_forward depth_backward in
    tree_ref := Some t;
    let tree_structure = TM.get_structure t in
    let layout = from_model tree_structure (TM.get_root t) model in
    new tree_model layout t

end
