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

(* This module is a contribution of Yuto Takei *)

module Imperative
    (G: Sig.IM)
    (W: Sig.WEIGHT with type edge = G.E.t) = struct

  module S = Set.Make(G.V)
  module M = Map.Make(G.V)

  module V = G.V
  module E = G.E

  (* [G.t] represents graph itself.  [unit M.t] maintains a list of
     source vertices to keep track of distances for all vertices.
     [(G.E.t option * W.t) M.t M.t] holds mappings for all vertices,
     each of which contains its shortest-path tree ancestor (parent)
     and a distances from source vertices. *)
  type t = G.t * S.t ref * (G.E.t option * W.t) M.t M.t ref
  type edge = G.edge
  type vertex = G.vertex

  let sov v = string_of_int (Obj.magic (V.label v))
  let dump_cycle cycle =
    let v0 = G.E.src (List.hd cycle) in
    print_string ("(" ^ (sov v0) ^ ")");
    let v1 = List.fold_left (fun v e ->
        assert ((G.V.compare v (G.E.src e)) = 0);
        let v = G.E.dst e in
        print_string ("-(" ^ (sov v) ^ ")");
        v) v0 cycle in
    assert (v0 = v1);
    print_string "\n"
  let dump_set = S.iter (fun x -> print_string ((sov x) ^ ", "))
  let dump (src, dist) =
    print_string "====================\nS: ";
    dump_set !src;
    print_string "\nMap:";
    M.iter (fun k v ->
        print_string ("\n  " ^ (sov k) ^ ": ");
        M.iter (fun k (origin, dist) ->
            print_string (
              "(" ^ (sov k) ^ ">>" ^
              (match origin with
               | None -> "---"
               | Some e -> (sov (G.E.src e)) ^ ">"
                           ^ (sov (G.E.dst e))) ^ ":" ^
              (string_of_int (Obj.magic dist)) ^ ") ")) v) !dist;
    print_string "\n"

  (* If an edge is going to be added to the graph, which will cause
     a negative cycle, raises [Negative_cycle] with edges that can
     form such the cycle. *)
  exception Negative_cycle of G.E.t list

  let create ?size () =
    let g = match size with
      | Some size -> G.create ~size ()
      | None -> G.create () in
    (g, ref S.empty, ref M.empty)

  let copy (g, src, dist) =
    (G.copy g, ref (!src), ref (!dist))

  let clear (g, src, dist) =
    G.clear g;
    src := S.empty;
    dist := M.empty

  let add_vertex (g, src, dist) v =
    (* Before adding vertex to the graph, make sure that the vertex
       is not in the graph.  If already in the graph, just do
       nothing and return as is. *)
    if not (G.mem_vertex g v) then begin
      (* Add a vertex to the original one *)
      G.add_vertex g v;

      (* The new vertex will immediately be added to the source list *)
      src := S.add v !src;

      (* The new edge should contain a distance mapping with only
         from myself with distance zero. *)
      dist := M.add v (M.add v (None, W.zero) M.empty) !dist;
      dump (src, dist)
    end

  let rec propagate (g, src, dist) q start =
    if Queue.is_empty q then (g, src, dist)
    else begin
      let (v1, v1src) = Queue.pop q in
      let v1dist = M.find v1 dist in

      let dist = G.fold_succ_e (fun e dist ->
          let v2 = G.E.dst e in
          let v2dist = if M.mem v2 dist then M.find v2 dist else M.empty in

          (* Compare distances from given source vertices.
                   If relax happens, record it to the new list. *)
          let (v2dist, nextSrc) = S.fold (fun x (v2dist, nextSrc) ->
              let _, dev1 = M.find x v1dist in
              let ndev2 = W.add dev1 (W.weight e) in
              let improvement =
                try
                  let _, dev2 = M.find x v2dist in
                  W.compare ndev2 dev2 < 0
                with Not_found -> true in
              if improvement then
                let v2dist = M.add x (Some e, ndev2) v2dist in
                let nextSrc = S.add x nextSrc in
                (v2dist, nextSrc)
              else
                (v2dist, nextSrc)
            ) v1src (v2dist, S.empty) in

          if S.is_empty nextSrc then
            dist
          else if G.V.equal start v2 then
            (* Propagation reaches back to the starting node, which
               immediately means presence of a negative cycle. *)

            (* We should use one of 'src' to traverse to the start node *)
            let dist = M.add v2 v2dist dist in
            let cycle = S.fold (fun s x ->
                let rec build_cycle x ret =
                  match M.find s (M.find x dist) with
                  | Some e, _ ->
                    let y = G.E.src e in
                    let cycle = e :: ret in
                    if G.V.equal start y then Some cycle
                    else build_cycle y cycle
                  | _ -> None in
                match x with
                | None -> build_cycle v2 []
                | Some _ -> x) nextSrc None in
            let cycle = match cycle with
              | Some x -> x | None -> assert false in
            dump_cycle cycle;
            raise (Negative_cycle cycle)
          else
            begin
              (* TODO: Some room for improvement.
                 If queue has (v2, s) already, technically we can merge
                 nextSrc into s, so that the number of propagation can be
                 reduced. *)
              Queue.push (v2, nextSrc) q;
              M.add v2 v2dist dist
            end
        ) g v1 dist in
      propagate (g, src, dist) q start
    end

  let m_cardinal m = M.fold (fun _ _ acc -> acc+1) m 0
  let set_of_map m = M.fold (fun k _ acc -> S.add k acc) m S.empty

  let add_edge_internal (g, src, dist) v1 v2 =
    (* Distance mappings at v1 *)
    let dv1 = M.find v1 dist in

    (* To reduce the amount of codes, we just start propagation from
       v1. Of course, this can be optimized by starting from v2. But
       it may duplicate the same code in multiple places in the
       file. In addition, such an optimization only cost for small
       amount, which precisely is the operations to relax edges from
       v1, other than which have been existed before this
       [add_edge_e] call. *)
    let q = Queue.create () in

    (* We need to check whether v2 should be kept in the source list
       or not.  That is, if there maybe a cycle with v1, the
       distance from v1 should be still maintained. Otherwise,
       simply ignore the distance from v2 *)
    if m_cardinal dv1 = 1 && M.mem v2 dv1 then (
      (* Now we definitely introduced a loop (and possibly non-negative)!
         Let me see if this would be negative or not... *)
      Queue.add (v1, (S.add v2 S.empty)) q;
      propagate (g, src, dist) q v1
    ) else (
      (* Or even if we fall back to else-clause here, the edge addition
               may have introduced a cycle. Anyway, we need to check if one is
               newly created or not at [propagate] *)

      let (src, dist, dv1) =
        if not (S.mem v2 src) then
          (* If v2 isn't one of the source vertices, just simply do
             propagation. *)
          (src, dist, dv1)
        else
          (* We can exclude v2 from the list of source because
             one can reach v2 from some other vertex. *)
          let src = S.remove v2 src in

          (* Note that following line can be skipped only if the
             user don't remove vertex. Otherwise, such operation
             like [add_edge g v1 v2] > [remove_vertex g v2] >
             [add_vertex g v2] can result in unexpected
             behavior. *)
          let dist = M.map (M.remove v2) dist in

          (* We need to re-obtain the distance mappings at v1,
             since it can be changed by the line above. *)
          let dv1 = M.find v1 dist in
          (src, dist, dv1) in

      (* Now let's start propagation. *)
      Queue.add (v1, set_of_map dv1) q;
      propagate (g, src, dist) q v1)

  let add_edge_e (g, src, dist) e =
    (* Before adding edge to the graph, make sure that the edge is
       not in the graph.  If already in the graph, just do nothing
       and return as is. *)
    if not (G.mem_edge_e g e) then begin
      (* Vertices involved *)
      let v1 = G.E.src e in
      let v2 = G.E.dst e in
      List.iter (add_vertex (g, src, dist)) [v1 ; v2];

      begin try
          (* Because we can restore the graph by calling [G.remove_edge_e]
             even in case of failure, we first add it by [G.add_edge_e]. *)
          G.add_edge_e g e;
          let (_, src', dist') = add_edge_internal (g, !src, !dist) v1 v2 in
          src := src'; dist := dist'
        with exp ->
          (* In case of excecption, restore the graph by removing the
             edge, and rethrow the exception. *)
          G.remove_edge_e g e;
          raise exp
      end;
      dump (src, dist)
    end

  let add_edge (g, src, dist) v1 v2 =
    (* Same as [add_edge_e] *)
    if not (G.mem_edge g v1 v2) then begin
      List.iter (add_vertex (g, src, dist)) [v1 ; v2];

      begin try
          (* Because we cannot know the default value for edge length,
             we first try to add one by [G.add_edge]. If there occurs an
             exception, restore the graph by [G.remove_edge] since there
             were no other connections between [v1] and [v2]. *)
          G.add_edge g v1 v2;
          let (_, src', dist') = add_edge_internal (g, !src, !dist) v1 v2 in
          src := src'; dist := dist'
        with exp ->
          (* In case of excecption, restore the graph by removing the
             edge, and rethrow the exception. *)
          G.remove_edge g v1 v2;
          raise exp
      end;

      dump (src, dist)
    end

  let remove_edge_internal (g, src) v2 =
    (* Actually, we need to rebuild the distance table, rather than
       traverse precedants to remove the edge. *)
    let q = Queue.create () in
    print_string ("dump: ");
    dump_set src;
    let dist = S.fold (fun x dist ->
        print_string ("source: " ^ (sov x) ^ "\n");
        Queue.add (x, (S.add x S.empty)) q;
        M.add x (M.add x (None, W.zero) M.empty) dist) src M.empty  in
    let g, src, dist = propagate (g, src, dist) q (S.choose src) in

    if M.mem v2 dist then
      (g, src, dist)
    else (
      Queue.add (v2, (S.add v2 S.empty)) q;
      let src = S.add v2 src in
      let dist = M.add v2 (M.add v2 (None, W.zero) M.empty) dist in
      propagate (g, src, dist) q v2)

  let remove_edge_e (g, src, dist) e =
    (* Same as [add_edge_e] *)
    if G.mem_edge_e g e then begin
      G.remove_edge_e g e;

      (* Vertices involved *)
      let v2 = G.E.dst e in
      let (_, src', dist') = remove_edge_internal (g, !src) v2 in
      src := src';
      dist := dist';

      dump (src, dist)
    end

  let remove_edge (g, src, dist) v1 v2 =
    (* Same as [add_edge] *)
    if G.mem_edge g v1 v2 then begin
      G.remove_edge g v1 v2;

      let (_, src', dist') = remove_edge_internal (g, !src) v2 in
      src := src';
      dist := dist';

      dump (src, dist)
    end

  let remove_vertex (g, src, dist) v =
    (* Same as [add_edge] *)
    if G.mem_vertex g v then begin
      (* [remove_vertex] first deletes all outgoing edges from [v] *)
      G.iter_succ_e (fun e -> remove_edge_e (g, src, dist) e) g v;
      (* Then after, deletes all incoming edges to [v] *)
      G.iter_pred_e (fun e -> remove_edge_e (g, src, dist) e) g v;
      (* Note that we are iterating on [g] that is being modified during
               iteration. We can do such an above iteration since G is here
               permanent. Do not try this for imperative graph. *)

      (* Now we can feel free to delete [v]. *)
      G.remove_vertex g v;
      src := S.remove v !src;
      dist := M.remove v (M.map (M.remove v) !dist);

      dump (src, dist)
    end

  let map_vertex f (g, src, dist) =
    let map_map update m =
      M.fold (fun v m acc -> M.add (f v) (update m) acc) m M.empty
    in
    let (g, src, dist) = (G.map_vertex f g,
                          S.fold (fun v acc -> S.add (f v) acc) !src S.empty,
                          let update = function
                            | None, _ as v -> v
                            | Some e, w ->
                              Some (E.create (f (E.src e)) (E.label e) (f (E.dst e))), w
                          in
                          map_map (map_map update) !dist) in
    (g, ref src, ref dist)

  let fold_pred_e f (g, _, _) = G.fold_pred_e f g
  let iter_pred_e f (g, _, _) = G.iter_pred_e f g
  let fold_succ_e f (g, _, _) = G.fold_succ_e f g
  let iter_succ_e f (g, _, _) = G.iter_succ_e f g
  let fold_pred f (g, _, _) = G.fold_pred f g
  let fold_succ f (g, _, _) = G.fold_succ f g
  let iter_pred f (g, _, _) = G.iter_pred f g
  let iter_succ f (g, _, _) = G.iter_succ f g
  let fold_edges_e f (g, _, _) = G.fold_edges_e f g
  let iter_edges_e f (g, _, _) = G.iter_edges_e f g
  let fold_edges f (g, _, _) = G.fold_edges f g
  let iter_edges f (g, _, _) = G.iter_edges f g
  let fold_vertex f (g, _, _) = G.fold_vertex f g
  let iter_vertex f (g, _, _) = G.iter_vertex f g
  let pred_e (g, _, _) = G.pred_e g
  let succ_e (g, _, _) = G.succ_e g
  let pred (g, _, _) = G.pred g
  let succ (g, _, _) = G.succ g
  let find_all_edges (g, _, _) = G.find_all_edges g
  let find_edge (g, _, _) = G.find_edge g
  let mem_edge_e (g, _, _) = G.mem_edge_e g
  let mem_edge (g, _, _) = G.mem_edge g
  let mem_vertex (g, _, _) = G.mem_vertex g
  let in_degree (g, _, _) = G.in_degree g
  let out_degree (g, _, _) = G.out_degree g
  let nb_edges (g, _, _) = G.nb_edges g
  let nb_vertex (g, _, _) = G.nb_vertex g
  let is_empty (g, _, _) = G.is_empty g
  let is_directed = G.is_directed

  module Mark = struct
    type graph = t
    type vertex = G.vertex
    let clear g = let (g, _, _) = g in G.Mark.clear g
    let get = G.Mark.get
    let set = G.Mark.set
  end

end

module Persistent
    (G: Sig.P)
    (W: Sig.WEIGHT with type edge = G.E.t) = struct

  module S = Set.Make(G.V)
  module M = Map.Make(G.V)

  module E = G.E
  module V = G.V

  (* [G.t] represents graph itself.  [unit M.t] maintains a list of
     source vertices to keep track of distances for all vertices.
     [(G.E.t option * W.t) M.t M.t] holds mappings for all vertices,
     each of which contains its shortest-path tree ancestor (parent)
     and a distances from source vertices. *)
  type t = G.t * S.t * (G.E.t option * W.t) M.t M.t
  type edge = G.edge
  type vertex = G.vertex

  (* If an edge is going to be added to the graph, which will cause
     a negative cycle, raises [Negative_cycle] with edges that can
     form such the cycle. *)
  exception Negative_cycle of G.E.t list

  let empty : t =
    let g = G.empty in
    let src = S.empty in
    let dist = M.empty in
    (g, src, dist)

  let add_vertex (g, src, dist) v =
    (* Before adding vertex to the graph, make sure that the vertex
       is not in the graph.  If already in the graph, just do
       nothing and return as is. *)
    if G.mem_vertex g v then
      (g, src, dist)
    else
      (* Add a vertex to the original one *)
      (G.add_vertex g v),

      (* The new vertex will immediately be added to the source list *)
      (S.add v src),

      (* The new edge should contain a distance mapping with only
         from myself with distance zero. *)
      (M.add v (M.add v (None, W.zero) M.empty) dist)

  let rec propagate (g, src, dist) q start =
    if Queue.is_empty q then (g, src, dist)
    else begin
      let (v1, v1src) = Queue.pop q in
      let v1dist = M.find v1 dist in

      let dist = G.fold_succ_e (fun e dist ->
          let v2 = G.E.dst e in
          let v2dist = M.find v2 dist in

          (* Compare distances from given source vertices.
                   If relax happens, record it to the new list. *)
          let (v2dist, nextSrc) = S.fold (fun x (v2dist, nextSrc) ->
              let _, dev1 = M.find x v1dist in
              let ndev2 = W.add dev1 (W.weight e) in
              let improvement =
                try
                  let _, dev2 = M.find x v2dist in
                  W.compare ndev2 dev2 < 0
                with Not_found -> true in
              if improvement then
                let v2dist = M.add x (Some e, ndev2) v2dist in
                let nextSrc = S.add x nextSrc in
                (v2dist, nextSrc)
              else
                (v2dist, nextSrc)
            ) v1src (v2dist, S.empty) in

          if S.is_empty nextSrc then
            dist
          else if G.V.equal start v2 then
            (* Propagation reaches back to the starting node, which
               immediately means presence of a negative cycle. *)

            (* We should use one of 'src' to traverse to the start node *)
            let s = S.choose nextSrc in
            let rec build_cycle x ret =
              match M.find s (M.find x dist) with
              | Some e, _ ->
                let y = G.E.src e in
                let cycle = e :: ret in
                if G.V.equal start y then cycle
                else build_cycle y cycle
              | _ -> assert false in
            raise (Negative_cycle (build_cycle v2 []))
          else
            begin
              (* TODO: Some room for improvement.
                 If queue has (v2, s) already, technically we can merge
                 nextSrc into s, so that the number of propagation can be
                 reduced. *)
              Queue.push (v2, nextSrc) q;
              M.add v2 v2dist dist
            end
        ) g v1 dist in
      propagate (g, src, dist) q start
    end

  let m_cardinal m = M.fold (fun _ _ acc -> acc+1) m 0
  let set_of_map m = M.fold (fun k _ acc -> S.add k acc) m S.empty

  let add_edge_internal (g, src, dist) v1 v2 =
    (* Distance mappings at v1 *)
    let dv1 = M.find v1 dist in

    (* To reduce the amount of codes, we just start propagation from
       v1. Of course, this can be optimized by starting from v2. But
       it may duplicate the same code in multiple places in the
       file. In addition, such an optimization only cost for small
       amount, which precisely is the operations to relax edges from
       v1, other than which have been existed before this
       [add_edge_e] call. *)
    let q = Queue.create () in

    (* We need to check whether v2 should be kept in the source list
       or not.  That is, if there maybe a cycle with v1, the
       distance from v1 should be still maintained. Otherwise,
       simply ignore the distance from v2 *)
    if m_cardinal dv1 = 1 && M.mem v2 dv1 then (
      (* Now we definitely introduced a loop (but possibly non-negative)!
         Let me see if this would be negative or not... *)
      Queue.add (v1, (S.add v2 S.empty)) q;
      propagate (g, src, dist) q v1
    ) else (
      (* Or even if we fall back to else-clause here, the edge addition
               may have introduced a cycle. Anyway, we need to check if one is
               newly created or not at [propagate] *)

      let (src, dist, dv1) =
        if not (S.mem v2 src) then
          (* If v2 isn't one of the source vertices, just simply do
             propagation. *)
          (src, dist, dv1)
        else
          (* We can exclude v2 from the list of source because
                   one can reach v2 from some other vertex. *)
          ((S.remove v2 src),

           (* Note that following line can be skipped only if the
              user don't remove vertex. Otherwise, such operation
              like [add_edge g v1 v2] > [remove_vertex g v2] >
              [add_vertex g v2] can result in unexpected
              behaviour. *)
           (M.map (M.remove v2) dist),

           (* We need to re-obtain the distance mappings at v1,
                    since it can be changed by the line above. *)
           (M.find v1 dist)) in

      (* Now let's start propagation. *)
      Queue.add (v1, set_of_map dv1) q;
      propagate (g, src, dist) q v1)

  let add_edge_e (g, src, dist) e =
    (* Before adding edge to the graph, make sure that the edge is
       not in the graph.  If already in the graph, just do nothing
       and return as is. *)
    if G.mem_edge_e g e then
      (g, src, dist)
    else begin
      (* Vertices involved *)
      let v1 = G.E.src e in
      let v2 = G.E.dst e in

      let (g, src, dist) = List.fold_left
          add_vertex (g, src, dist) [v1 ; v2] in
      let g = G.add_edge_e g e in

      add_edge_internal (g, src, dist) v1 v2
    end

  let add_edge (g, src, dist) v1 v2 =
    (* Same as [add_edge_e] *)
    if G.mem_edge g v1 v2 then
      (g, src, dist)
    else begin
      let (g, src, dist) = List.fold_left
          add_vertex (g, src, dist) [v1 ; v2] in
      let g = G.add_edge g v1 v2 in

      add_edge_internal (g, src, dist) v1 v2
    end

  let remove_edge_internal (g, src) v2 =
    (* Actually, we need to rebuild the distance table, rather than
       traverse precedants to remove the edge. *)
    let q = Queue.create () in
    let dist = S.fold (fun x dist ->
        Queue.add (x, (S.add x S.empty)) q;
        M.add x (M.add x (None, W.zero) M.empty) dist) src M.empty  in
    let g, src, dist = propagate (g, src, dist) q (S.choose src) in

    if M.mem v2 dist then
      (g, src, dist)
    else (
      Queue.add (v2, (S.add v2 S.empty)) q;
      let src = S.add v2 src in
      let dist = M.add v2 (M.add v2 (None, W.zero) M.empty) dist in
      propagate (g, src, dist) q v2)

  let remove_edge_e (g, src, dist) e =
    (* Same as [add_edge_e] *)
    if not (G.mem_edge_e g e) then
      (g, src, dist)
    else begin
      let g = G.remove_edge_e g e in

      (* Vertices involved *)
      let v2 = G.E.dst e in

      remove_edge_internal (g, src) v2
    end

  let remove_edge (g, src, dist) v1 v2 =
    (* Same as [add_edge] *)
    if not (G.mem_edge g v1 v2) then
      (g, src, dist)
    else begin
      let g = G.remove_edge g v1 v2 in
      remove_edge_internal (g, src) v2
    end

  let remove_vertex t v =
    (* [remove_vertex] first deletes all outgoing edges from [v] *)
    let (g, _, _) = t in
    let t = G.fold_succ_e (fun e t -> remove_edge_e t e) g v t in
    (* Then after, deletes all incoming edges to [v] *)
    let (g, _, _) = t in
    let t = G.fold_pred_e (fun e t -> remove_edge_e t e) g v t in
    (* Note that we are iterating on [g] that is being modified during
       iteration. We can do such an above iteration since G is here
       permanent. Do not try this for imperative graph. *)
    let (g, src, dist) = t in

    (* Now we can feel free to delete [v]. *)
    (G.remove_vertex g v,
     (S.remove v src),
     (M.map (M.remove v) dist))

  let map_vertex f (g, src, dist) =
    let map_map update m =
      M.fold (fun v m acc -> M.add (f v) (update m) acc) m M.empty
    in
    (G.map_vertex f g,
     S.fold (fun v acc -> S.add (f v) acc) src S.empty,
     let update = function
       | None, _ as v -> v
       | Some e, w ->
         Some (E.create (f (E.src e)) (E.label e) (f (E.dst e))), w
     in
     map_map (map_map update) dist)

  (* All below are wrappers *)
  let fold_pred_e f (g, _, _) = G.fold_pred_e f g
  let iter_pred_e f (g, _, _) = G.iter_pred_e f g
  let fold_succ_e f (g, _, _) = G.fold_succ_e f g
  let iter_succ_e f (g, _, _) = G.iter_succ_e f g
  let fold_pred f (g, _, _) = G.fold_pred f g
  let fold_succ f (g, _, _) = G.fold_succ f g
  let iter_pred f (g, _, _) = G.iter_pred f g
  let iter_succ f (g, _, _) = G.iter_succ f g
  let fold_edges_e f (g, _, _) = G.fold_edges_e f g
  let iter_edges_e f (g, _, _) = G.iter_edges_e f g
  let fold_edges f (g, _, _) = G.fold_edges f g
  let iter_edges f (g, _, _) = G.iter_edges f g
  let fold_vertex f (g, _, _) = G.fold_vertex f g
  let iter_vertex f (g, _, _) = G.iter_vertex f g
  let pred_e (g, _, _) = G.pred_e g
  let succ_e (g, _, _) = G.succ_e g
  let pred (g, _, _) = G.pred g
  let succ (g, _, _) = G.succ g
  let find_all_edges (g, _, _) = G.find_all_edges g
  let find_edge (g, _, _) = G.find_edge g
  let mem_edge_e (g, _, _) = G.mem_edge_e g
  let mem_edge (g, _, _) = G.mem_edge g
  let mem_vertex (g, _, _) = G.mem_vertex g
  let in_degree (g, _, _) = G.in_degree g
  let out_degree (g, _, _) = G.out_degree g
  let nb_edges (g, _, _) = G.nb_edges g
  let nb_vertex (g, _, _) = G.nb_vertex g
  let is_empty (g, _, _) = G.is_empty g
  let is_directed = G.is_directed

end
