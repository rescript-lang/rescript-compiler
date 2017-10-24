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

module type G = sig
  type t
  module V : sig
    type t
    type label
    val label : t -> label
    val hash : t -> int
    val equal : t -> t -> bool
  end
  module E : sig
    type t
  end
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val iter_pred : (V.t -> unit) -> t -> V.t -> unit
  val find_edge : t -> V.t -> V.t -> E.t
end

module type Tree = sig
  type t
  module V : sig
    type t
    type label
    val create : label -> t
    val label : t -> label
    val hash: t -> int
    val equal: t -> t -> bool
  end
  module E : Sig.EDGE with type vertex = V.t
  val create : ?size:int -> unit -> t
  val add_vertex : t -> V.t -> unit
  val add_edge_e : t -> E.t -> unit
end

module type S = sig

  module Tree: Tree with type E.label = unit
  type t
  val get_structure : t -> Tree.t
  val get_root : t -> Tree.V.t
  val get_tree_vertices : Tree.V.label -> t -> Tree.V.t list
  val is_ghost_node : Tree.V.t -> t -> bool
  val is_ghost_edge : Tree.E.t -> t -> bool
  exception Ghost_node
  val get_graph_vertex : Tree.V.t -> t -> Tree.V.label

end

module Build
    (G : G)
    (Tree : Tree with type V.label = G.V.t and type E.label = unit)
    (GA: sig
       type t
       val iter_succ: (G.V.t -> unit) -> t -> G.V.t -> unit
       val iter_pred: (G.V.t -> unit) -> t -> G.V.t -> unit
     end) =
struct

  module Tree = Tree
  module H = Hashtbl.Make(G.V)
  module HT = Hashtbl.Make(Tree.V)
  module HE =
    Hashtbl.Make
      (struct
        type t = Tree.E.t
        let equal x y = Tree.E.compare x y = 0
        let hash = Hashtbl.hash
      end)

  type t = {
    structure: Tree.t; (* the tree itself *)
    root : Tree.V.t; (* the root *)
    (* nodes of the tree corresponding to the original nodes *)
    assoc_vertex_table: Tree.V.t H.t;

    ghost_vertices: unit HT.t;
    ghost_edges: unit HE.t;
  }

  (* Getter *)
  let get_structure t = t.structure;;
  let get_root t = t.root;;

  (** Give the list of vertices in the tree graph representing a vertex
      from the old graph *)
  let get_tree_vertices vertex tree =
    try H.find_all tree.assoc_vertex_table vertex
    with Not_found -> assert false;;

  (** True if the vertex is not to be shown *)
  let is_ghost_node v tree = HT.mem tree.ghost_vertices v;;

  (** True if the edge is not to be shown *)
  let is_ghost_edge e tree = HE.mem tree.ghost_edges e;;

  exception Ghost_node;;

  (** Give the old graph vertex represented by a vertex in the tree -
      @raise Ghost_node if the vertex is a ghost vertex *)
  let get_graph_vertex vertex tree =
    if is_ghost_node vertex tree then raise Ghost_node
    else Tree.V.label vertex;;

  (* Explore the graph from a vertex and build a tree -
     Will be used forward and backward *)
  let build src_graph tree src_vertex tree_root backward_flag depth =
    let complete_to_depth v missing =
      let pred_vertex = ref v in
      let next_vertex = ref v in
      for _i = 1 to missing - 1 do
        next_vertex := Tree.V.create (Tree.V.label v);
        HT.add tree.ghost_vertices !next_vertex ();
        let new_ghost_edge =
          if backward_flag then Tree.E.create !next_vertex () !pred_vertex
          else Tree.E.create !pred_vertex () !next_vertex
        in Tree.add_edge_e tree.structure new_ghost_edge;
        HE.add tree.ghost_edges new_ghost_edge ();
        pred_vertex := !next_vertex;
      done
    in
    let has_succ = ref false in
    let vertex_visited = H.create 97 in
    let queue = Queue.create () in
    H.add vertex_visited src_vertex true;
    (* Initialize queue *)
    if depth <> 0 then
      if backward_flag then
        GA.iter_pred
          (fun a -> Queue.add (a, tree_root, depth) queue)
          src_graph
          src_vertex
      else
        GA.iter_succ
          (fun a -> Queue.add (a, tree_root, depth) queue)
          src_graph
          src_vertex;
    (* Empty queue *)
    let rec empty_queue () =
      if not(Queue.is_empty queue) then begin
        let vertex, origin_vertex, depth = Queue.take queue in
        if depth > 0 then begin
          let new_vertex = Tree.V.create vertex in
          H.add tree.assoc_vertex_table vertex new_vertex;
          if backward_flag then begin
            let new_edge = Tree.E.create new_vertex () origin_vertex in
            Tree.add_edge_e tree.structure new_edge
          end else begin
            let new_edge = Tree.E.create origin_vertex () new_vertex in
            Tree.add_edge_e tree.structure new_edge
          end;
          if not(H.mem vertex_visited vertex) then begin
            H.add vertex_visited vertex true;
            let iter f =
              f
                (fun a ->
                   Queue.add (a, new_vertex, depth - 1) queue;
                   has_succ := true)
                src_graph
                vertex
            in
            if backward_flag then iter GA.iter_pred else iter GA.iter_succ;
            if not !has_succ then complete_to_depth new_vertex depth;
            has_succ := false;
          end else if depth <> 1 then begin
            if backward_flag then
              GA.iter_pred (fun _ -> has_succ := true) src_graph vertex
            else
              GA.iter_succ (fun _ -> has_succ := true) src_graph vertex;
            if !has_succ then begin
              let ghost_vertex = Tree.V.create vertex in
              HT.add tree.ghost_vertices ghost_vertex ();
              let new_edge =
                if backward_flag then Tree.E.create ghost_vertex () new_vertex
                else Tree.E.create new_vertex () ghost_vertex
              in Tree.add_edge_e tree.structure new_edge;
              complete_to_depth ghost_vertex (depth-1)
            end else
              complete_to_depth new_vertex depth;
            has_succ := false;
          end
        end;
        empty_queue ()
      end
    in
    empty_queue ()
  (* [JS 2010/11/10] trying to simplify the algorithm. Not finish yet
     let new_build graph tree root troot depth backward =
     let first = ref true in
     let q = Queue.create () in
     (* invariant: [h] contains exactly the vertices which have been pushed *)
     let must_add_ghost = ref true in
     let add_tree_vertex v =
      let tv = if !first then troot else Tree.V.create v in
      first := false;
      Tree.add_vertex tree.structure tv;
      H.add tree.assoc_vertex_table v tv;
      tv
     in
     let add_tree_edge tsrc dst =
      let tdst = add_tree_vertex dst in
      let tsrc, tdst = if backward then tdst, tsrc else tsrc, tdst in
      let e = Tree.E.create tsrc () tdst in
      Tree.add_edge_e tree.structure e;
      tdst, e
     in
     let push n src dst =
      if n < depth then Queue.add (dst, n + 1) q;
      ignore (add_tree_edge src dst);
      must_add_ghost := false
     in
     let loop () =
      while not (Queue.is_empty q) do
     let v, n = Queue.pop q in
     let tv = add_tree_vertex v in
     must_add_ghost := true;
     (if backward then GA.iter_pred else GA.iter_succ) (push n tv) graph v;
     if !must_add_ghost then
     let tsrc = ref tv in
     for i = n to depth do
      let tdst, te = add_tree_edge !tsrc v in
      HT.add tree.ghost_vertices tdst ();
      HE.add tree.ghost_edges te ();
      tsrc := tdst
     done
      done
     in
     Queue.add (root, 0) q;
     loop ()
  *)
  (** Build a tree graph centered on a vertex and containing its
      predecessors and successors *)
  let make src_graph src_vertex depth_forward depth_backward =
    let tree = {
      structure = Tree.create ();
      root = Tree.V.create src_vertex;
      assoc_vertex_table = H.create 97;
      ghost_vertices = HT.create 17;
      ghost_edges = HE.create 17;
    }
    in
    H.add tree.assoc_vertex_table src_vertex tree.root;
    Tree.add_vertex tree.structure tree.root;
    build src_graph tree src_vertex tree.root false depth_forward;
    build src_graph tree src_vertex tree.root true depth_backward;
    (*    new_build src_graph tree src_vertex tree.root depth_forward false;
          new_build src_graph tree src_vertex tree.root depth_backward true;*)
    tree

end

module Make
    (G : G)
    (Tree : Tree with type V.label = G.V.t and type E.label = unit) =
  Build(G)(Tree)(G)

module Make_from_dot_model
    (Tree : Tree with type V.label = DGraphModel.DotG.V.t
                  and type E.label = unit) =
  Build
    (DGraphModel.DotG)
    (Tree)
    (struct
      type t =  DGraphModel.dotg_model
      let iter_succ f g = g#iter_succ f
      let iter_pred f g = g#iter_pred f
    end)
