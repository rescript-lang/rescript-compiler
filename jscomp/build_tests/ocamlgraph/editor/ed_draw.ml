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

(* This file is a contribution of Benjamin Vadon *)

open Ed_hyper
open Ed_graph

let make_subgraph l =
  let gl = G.create () in
  List.iter (fun v -> G.add_vertex gl v) l;
  List.iter 
    (fun v ->
       List.iter (fun w -> 
           if edge v w 
           then G.add_edge gl v w) 
         l) 
    l; 
  (* TODO: efficacite *)
  gl

let order_children l =
  let gl = make_subgraph l in
  let scc = Components.scc_list gl in
  let order_component c =
    let gc = make_subgraph c in
    (* choose a vertex v of minimal out degree *)
    let v = match c with
      | v :: l ->
        List.fold_left 
          (fun m v -> 
             if G.out_degree gc v < G.out_degree gc m 
             then v 
             else m)
          v l
      | [] -> 
        assert false
    in 
    let l = ref [] in
    Dfs.prefix_component (fun w -> l := w :: !l) gc v;
    !l
  in
  let scc = List.map order_component scc in
  List.flatten scc


(* Depth First Search drawing *)

let rec draw_dfs depth node turtle =
  let lab = G.V.label node in
  lab.turtle <- turtle;
  lab.depth <- depth;
  if hspace_dist_sqr turtle <= rlimit_sqr then begin
    lab.visible <- Visible;
    let l = G.succ !graph node in 
    let l = List.filter (fun x -> (G.V.label x).visible = Hidden) l in
    List.iter (fun w -> (G.V.label w).visible <- BorderNode) l;
    let l = order_children l in
    let n = List.length l in
    if n > 0 then begin
      let distance = step_from (if depth = 0 then max 3 n else 2 * max 3 n)
      and angle = (if depth = 0 then 2. else 1.) *. pi /. (float_of_int n) in
      let turtle = 
        if depth = 0 then turtle else turn_right turtle ((pi -. angle) /. 2.) 
      in
      let _ = draw_edges_dfs node (depth+1) turtle distance angle l in
      ()
    end
  end

and draw_edges_dfs node depth turtle distance angle = function
  | [] -> 
    []
  | v :: l -> 
    let e = G.E.label (G.find_edge !graph node v) in
    e.visited <- true;
    e.edge_turtle <- turtle;
    e.edge_distance <- distance;
    let steps = 10 in
    e.edge_steps <- steps;
    let tv = advance_many turtle distance steps in 
    let turtle = turn_left turtle angle in
    let l = (v,tv) :: draw_edges_dfs node depth turtle distance angle l in
    draw_dfs depth v tv;
    l



(* Breadth First Search drawing *)

let draw_bfs root turtle =
  let q = Queue.create () in
  let add v n t =
    Queue.push v q;
    let lab = G.V.label v in
    lab.turtle <- t;
    lab.depth <- n
  in
  add root 0 turtle;
  while not (Queue.is_empty q) do
    let v = Queue.pop q in
    let lab = G.V.label v in
    let depth = lab.depth in
    let tv = lab.turtle in
    let dist = hspace_dist_sqr tv in
    (*    Format.eprintf"le noeud : %s la val presente apres :%f \n@."lab.label dist;*)
    if dist <= rlimit_sqr then begin
      lab.visible <- Visible;
      let l = try   G.succ !graph v  with Invalid_argument _ -> []  in
      let l = List.filter (fun x -> (G.V.label x).visible = Hidden) l in
      List.iter (fun w -> (G.V.label w).visible <- BorderNode) l;
      let l = order_children l in
      let n = List.length l in
      if n > 0 then begin
        let distance = step_from (if depth = 0 then max 3 n else 2 * max 3 n)
        and angle = (if depth = 0 then 2. else 1.) *. pi /. (float_of_int n) in
        let turtle = 
          ref (if depth = 0 then tv else turn_right tv ((pi -. angle) /. 2.))
        in
        List.iter
          (fun w -> 
             let e = G.E.label (G.find_edge !graph v w) in
             e.visited <- true;
             e.edge_turtle <- !turtle;
             e.edge_distance <- distance;
             let steps = 10 in
             e.edge_steps <- steps;
             let tw = advance_many !turtle distance steps in 
             add w (depth + 1) tw;
             turtle := turn_left !turtle angle)
          l
      end
    end
  done

(* Drawing graph function *)
let draw_graph root turtle =
  G.iter_vertex (fun v -> let l = G.V.label v in l.visible <- Hidden) !graph;
  G.iter_edges_e (fun e -> let l = G.E.label e in l.visited <- false) !graph;
  (if !dfs then draw_dfs 0 else draw_bfs) root turtle


