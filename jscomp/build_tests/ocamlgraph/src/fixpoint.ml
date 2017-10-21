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

(* maximum fixpoint point calculation with the work list algorithm;
   to implement a concrete analysis, implement a module that satisfies
   the Rules signature. Such a module in the Analysis functor gives a
   complete analysis/optimization module that works on a CFG.
*)

type direction = Forward | Backward

module type Analysis = sig
  type data
  type edge
  type vertex
  type g

  val direction : direction
  val join : data -> data -> data
  val equal : data -> data -> bool
  val analyze : edge -> data -> data
end

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


module Make
    (G : G)
    (A : Analysis with type g = G.t with type edge = G.E.t
     with type vertex = G.V.t) =
struct

  module M = Map.Make(G.V)
  module N = Set.Make(G.V)

  let analyze initial g =
    let (nodes, data) =
      G.fold_vertex
        (fun vertex (n, m) ->
           (N.add vertex n, M.add vertex (initial vertex) m))
        g (N.empty, M.empty)
    in
    (* generate an associative map to quickly find the incoming
     * (outgoing) edges of a node during the anaysis store a pair of
     * a partially applied analysis function and the corresponding
     * 'partner' node *)
    let nodemap : ((A.data -> A.data) * G.V.t) list M.t =
      let add = match A.direction with
        | Forward ->
          (fun n ->
             let preds = G.pred_e g n in
             List.map
               (fun edge -> (A.analyze edge, G.E.src edge))
               preds)
        | Backward ->
          (fun n ->
             let succs = G.succ_e g n in
             List.map
               (fun edge -> (A.analyze edge, G.E.dst edge))
               succs)
      in
      G.fold_vertex (fun vertex m -> M.add vertex (add vertex) m) g M.empty
    in

    let rec worklist (data : A.data M.t) (wl : N.t) =
      (* 'meet' an arbitrary number of data-sets *)
      let meet ~default = function
        | [] -> default
        | [x] -> x
        | x::xs -> List.fold_left (fun a b -> A.join a b) x xs
      in

      (* analyze one node, creating a new data-set and node-worklist
         as necessary *)
      let analyze_node analysis n d wl =
        match analysis d n with
        | None -> (d, wl)
        | Some d' -> (d', N.add n wl)
      in

      (* get some node from the node-set -- this will eventually trigger
           an exception *)
      match (try Some (N.choose wl) with Not_found -> None) with
      | None -> data
      | Some n ->
        (* remove the chosen node from the set *)
        let wl = N.remove n wl in

        let (f, ns) = match A.direction with
          (* analyze all INCOMING edges of all SUCCESSOR nodes of the
             node to be processed *)
          | Forward ->
            (* process one node: analyze all it's incoming edges
               and merge the resulting data;
               if the result is different to the previously stored data
               for this node, return a new tuple, else None *)
            let new_node_data (data : A.data M.t) node =
              let edges = M.find node nodemap in
              let analysis =
                List.map
                  (fun (f, src) -> f (M.find src data)) edges
              in
              let node_data = M.find node data in
              let node_data' = meet ~default:node_data analysis in
              if A.equal node_data node_data' then None
              else Some (M.add node node_data' data)
            in

            (new_node_data, G.succ g n)
          (* analyze all OUTGOING edges of all PREDECESSOR nodes
             of the node to be processed *)
          | Backward ->
            let new_node_data (data : A.data M.t) node =
              let edges = M.find node nodemap in
              let analysis =
                List.map
                  (fun (f, dst) -> f (M.find dst data)) edges
              in
              let node_data = M.find node data in
              let node_data' = meet ~default:node_data analysis in
              if A.equal node_data node_data' then None
              else Some (M.add node node_data' data)
            in

            (new_node_data, G.pred g n)
        in
        (* analyze all successor nodes by analyzing all of their
           predecessor edges *)
        let (data, wl) =
          List.fold_left (fun (d, wl) n -> analyze_node f n d wl)
            (data, wl) ns
        in

        (* do a recursive call: the recursion will eventually end with a
         * Not_found exception when no nodes are left in the work list *)
        worklist data wl
    in
    let data = worklist data nodes in
    (fun n -> M.find n data)
end
