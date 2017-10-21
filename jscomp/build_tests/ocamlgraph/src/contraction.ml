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

(* Copyright (c) 2012 Technische Universitaet Muenchen
 * Markus W. Weissmann <markus.weissmann@in.tum.de>
 * All rights reserved. *)

(* Edge contraction for directed, edge-labeled graphs *)

module type G = sig
  type t
  module V : Sig.COMPARABLE
  type vertex = V.t
  module E : Sig.EDGE with type vertex = vertex
  type edge = E.t

  val empty : t
  val add_edge_e : t -> edge -> t
  val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make
    (G : G) =
struct
  module M = Map.Make(G.V)
  module S = Set.Make(G.V)

  let contract prop g =
    (* if the edge is to be removed (property = true):
     * make a union of the two union-sets of start and end node;
     * put this set in the map for all nodes in this set *)
    let f edge m =
      if prop edge then
        let s_src, s_dst = M.find (G.E.src edge) m, M.find (G.E.dst edge) m in
        let s = S.union s_src s_dst in
        S.fold (fun vertex m -> M.add vertex s m) s m
      else
        m
    in
    (* if the edge is to be kept, add it to the new graph, exchanging
     * the start and end node with the minimum element from the set of
     * to-be-unified nodes; 'minimum is an arbitrary choice: any
     * deterministic choice will do *)
    let add m edge g =
      if prop edge then
        g
      else
        let lookup n = S.min_elt (M.find n m) in
        G.add_edge_e g
          (G.E.create (lookup (G.E.src edge)) (G.E.label edge)
             (lookup (G.E.dst edge)))
    in
    (* initialize map with singleton-sets for every node (of itself) *)
    let m =
      G.fold_vertex (fun vertex m -> M.add vertex (S.singleton vertex) m)
        g M.empty
    in
    (* find all closures *)
    let m = G.fold_edges_e f g m in
    (* rewrite the node numbers to close the gaps *)
    G.fold_edges_e (add m) g G.empty

end

