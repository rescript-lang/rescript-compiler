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

open Graph.Pack.Graph
open Outils_math
open Outils_tort
open Format
open Graphics

let graph = parse_gml_file Sys.argv.(1)

exception Choose of V.t

let grey = rgb 128 128 128


let root = 
  try
    iter_vertex (fun v -> raise (Choose v)) graph;
    Format.eprintf "empty graph@."; exit 0
  with Choose v ->
    v

(* [step_from n] computes the best `distance' for solving the
   dictator's problem in the complex hyperbolic plane for [n]
   dictators.  In a half-plane, we have to use the distance
   given by [step_from (2*n)] or, better, the distance given
   by [step_from (2*max(3 n))]. *)
let step_from n =
  ath (tan (pi_over_4 -. pi/.float(2*n)))

(* [hspace_dist_sqr turtle] computes the square of the distance
   between the origin and the half-space in front of [turtle]. *)
let hspace_dist_sqr turtle =
  let (ax, ay) = turtle.pos
  and (dx, dy) = turtle.dir in
  if ax*.dx +. ay*.dy < 0.0 then 0.0 else
    begin
      let ux = dy and uy = -.dx in
      let alpha = ax*.ax +. ay*.ay
      and beta = 2.0*.(ax*.ux +. ay*.uy) in
      if beta = 0.0 then
        alpha
      else
        begin
          let gamma = (1.0 +. alpha)/.beta in
          let delta = gamma*.gamma -. 1.0 in
          let sol =
            if beta > 0.0
            then -.gamma +. sqrt(delta)
            else -.gamma -. sqrt(delta) in
          let (zx, zy) = translate (ax, ay) (ux*.sol, uy*.sol) in
          zx*.zx +. zy*.zy
        end
    end ;;

let draw_label v =
  draw_string (string_of_int (V.label v))

let edge v w = mem_edge graph v w || mem_edge graph w v 

let make_subgraph l =
  let gl = create () in
  List.iter (fun v -> add_vertex gl v) l;
  List.iter 
    (fun v -> List.iter (fun w -> if edge v w then add_edge gl v w) l) 
    l; 
  (* TODO: efficacite *)
  gl

let order_children l =
  let gl = make_subgraph l in
  let scc = Components.scc_list gl in
  let order_component c =
    let gc = make_subgraph c in
    let v = match c with
      | v :: l ->
        List.fold_left 
          (fun m v -> if out_degree gc v < out_degree gc m then v else m)
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

let rlimit = 0.90 
let rlimit_sqr = rlimit *. rlimit

module Vset = Set.Make(V)
let vset_of_list = List.fold_left (fun s x -> Vset.add x s) Vset.empty


module H = Hashtbl.Make(V)

let pos = H.create 97

let rec draw_graph noeud tortue  =
  if hspace_dist_sqr tortue <= rlimit_sqr then
    begin
      H.add pos noeud (0,tortue);
      tmoveto tortue;
      draw_label noeud;
      let l = succ graph noeud in 
      let l = List.filter (fun x -> not (H.mem pos x) ) l in
      let l = order_children l in
      let n = List.length l in
      if n > 0 then
        begin
          let pas = step_from (max 3 n)
          and angle = 2. *. pi /. (float n) in
          let ll = draw_edges tortue pas angle l in
          List.iter (fun (v,tv) -> H.add pos v (1,tv)) ll;      
          List.iter 
            (fun (w,tw) ->     
               let l = succ graph w in
               let l = List.filter (fun x -> not (H.mem pos x)) l in
               let n = List.length l in
               if n > 0 then
                 begin
                   let pas = step_from (max 3 n)
                   and angle =  pi /. (float n) in
                   let tw = turn_right tw ((pi -. angle) /. 2.) in
                   let l = draw_edges tw pas angle l in
                   List.iter (fun (v,tv) -> H.add pos v (2,tv)) l
                 end) 
            ll;
          (* draw intern edges *)
          set_color grey;
          H.iter 
            (fun v (lv,tv) -> 
               List.iter
                 (fun w ->
                    try
                      let lw,tw = H.find pos w in
                      if abs (lw - lv) <> 1 then begin tmoveto tv; tlineto tw end
                    with Not_found ->
                      ()) 
                 (succ graph v))
            pos;
          set_color black

        end
    end
and  draw_edges t pas angle= function
  | [] -> 
    []
  | v :: l -> 
    let tv = tdraw_edge t pas 10 in 
    if hspace_dist_sqr t <= rlimit_sqr
    then (draw_label v;      H.add pos v (1,t));

    let t = turn_left t angle in
    let list = (v,tv) :: draw_edges t pas angle l in
    draw_graph v tv ;
    list

let draw origine tortue =
  H.clear pos;
  draw_graph  root tortue



let () = open_graph (sprintf " %dx%d" (truncate w) (truncate h))

let tortue =
  let (x,y) = from_tortue !origine in
  moveto x y;
  make_turtle !origine 0.0

let old_xy = ref None
let flags = [Button_down; Button_up; Key_pressed; Mouse_motion]



let rec boucle tortue = 
  let st = wait_next_event flags in
  if st.button then begin match !old_xy with 
    | None -> 
      clear_graph (); 
      draw origine tortue;
      old_xy := Some(st.mouse_x, st.mouse_y); 
      boucle tortue
    | Some(x,y) ->
      let z1 = to_tortue(x,y) 
      and z2 = to_tortue (st.mouse_x, st.mouse_y) in
      clear_graph (); 
      origine := drag_origin !origine z1 z2 ; 
      let tort = make_turtle_dir !origine tortue.dir in
      old_xy := Some(st.mouse_x, st.mouse_y);
      draw origine tortue;
      boucle tort 
  end 
  else 
    begin
      match !old_xy with
      | None ->
        draw origine tortue;
        boucle tortue
      | Some(x,y) ->
        clear_graph ();
        draw origine tortue;
        old_xy := None;
        boucle tortue
    end 

let () = boucle tortue

