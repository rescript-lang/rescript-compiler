(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2007                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Printf
open Graph

module U = Unix

let utime f x =
  let u = (U.times()).U.tms_utime in
  let y = f x in
  let ut = (U.times()).U.tms_utime -. u in
  (y,ut)

let print_utime f x =
  let (y,ut) = utime f x in
  Printf.printf "user time: %2.2f\n" ut; flush Pervasives.stdout;
  y

let () =
  printf "planar graphs demo
  use mouse to select two vertices (blue = source, green = destination)
  keys are
    - `r' generates a new random graph
    - `d' runs DFS
    - `b' runs BFS
    - `p' runs Dijkstra's shortest path
    - `c' runs SCC
    - `j' runs Johnson shortest path
    - `q' to quit
    ";
  flush stdout

(* directed graphs with integer coordinates and integer labels on edges *)

module IntInt = struct
  type t = int * int
end
module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end
module G = Imperative.Digraph.AbstractLabeled(IntInt)(Int)
open G

let n_ = ref 30
let prob_ = ref 0.5
let () =
  Arg.parse
      ["-v", Arg.Int (fun i -> n_ := i),
       " <int>  number of vertices";
       "-prob", Arg.Float (fun f -> prob_ := f),
       " <float>  probability to discrad an edge";
      ]
      (fun _ -> ())
      "usage: demo_planar <options>"
let n = !n_
let prob = !prob_

let round f = truncate (f +. 0.5)
let pi = 4.0 *. atan 1.0

module Point = struct
  type point = V.t
  let ccw v1 v2 v3 =
    Delaunay.IntPoints.ccw (V.label v1) (V.label v2) (V.label v3)
  let in_circle v1 v2 v3 v4 =
    Delaunay.IntPoints.in_circle
      (V.label v1) (V.label v2) (V.label v3) (V.label v4)
  let distance v1 v2 =
    let x1,y1 = V.label v1 in
    let x2,y2 = V.label v2 in
    let sqr x = let x = float x in x *. x in
    truncate (sqrt (sqr (x1 - x2) +. sqr (y1 - y2)))
end
module Triangulation = Delaunay.Make(Point)

let read_graph f =
  let c = open_in f in
  let l = ref [] in
  try
    while true do
      let s = input_line c in
      let x,y = Scanf.sscanf s "%d %f %f" (fun _ x y -> x,y) in
      printf "x=%f y=%f\n" x y;
      l := (x,y) :: !l
    done;
    assert false
  with End_of_file ->
    close_in c;
    let rec min_list cmp = function
      | [] -> assert false
      | [x] -> x
      | x :: l -> let m = min_list cmp l in if cmp x m then x else m
    in
    let xmin,_ = min_list (fun (x,_) (x',_) -> x < x') !l in
    let xmax,_ = min_list (fun (x,_) (x',_) -> x > x') !l in
    let _,ymin = min_list (fun (_,y) (_,y') -> y < y') !l in
    let _,ymax = min_list (fun (_,y) (_,y') -> y > y') !l in
    let calibrate (x,y) =
      round (20. +. 760. *. (x -. xmin) /. (xmax -. xmin)),
      round (20. +. 560. *. (y -. ymin) /. (ymax -. ymin))
    in
    let vertices =
      Array.map (fun xy -> V.create (calibrate xy)) (Array.of_list !l)
    in
    let t = Triangulation.triangulate vertices in
    let g = create () in
    Array.iter (G.add_vertex g) vertices;
    let add_edge v1 v2 =
      let e = E.create v1 (Point.distance v1 v2) v2 in G.add_edge_e g e
    in
    Triangulation.iter (fun v1 v2 -> add_edge v1 v2; add_edge v2 v1) t;
    g

(* a random digraph with n vertices *)
let () = Random.self_init ()
module R = Rand.Planar.I(G)
let new_graph () = R.graph ~xrange:(20,780) ~yrange:(20,580) ~prob n
let g = ref (new_graph ())

let () = printf "nb edges : %d\n" (G.nb_edges !g); flush stdout

let dump_graph () =
  G.iter_edges (fun v1 v2 ->
    let x1, y1 = G.V.label v1 in
    let x2, y2 = G.V.label v2 in
    Format.printf "%d,%d,%d,%d@\n" x1 y1 x2 y2) !g;
  Format.printf "@?"

(* let () = g := read_graph "tmp/carron.txt" *)

open Graphics
let () = open_graph " 800x600"

let vertex_radius = 5

let draw_arrow ?(color=black) ?(width=1) (xu,yu) (xv,yv) =
  set_color color;
  set_line_width width;
  let dx = float (xv - xu) in
  let dy = float (yv - yu) in
  let alpha = atan2 dy dx in
  let r = sqrt (dx *. dx +. dy *. dy) in
  let ra = float vertex_radius *. 1.5 in
  let d = float vertex_radius +. 3. in
  let xs, ys = float xu +. d *. dx /. r, float yu +. d *. dy /. r in
  let xd, yd = float xv -. d *. dx /. r, float yv -. d *. dy /. r in
  let coords theta =
    round (xd +. ra *. cos (pi +. alpha +. theta)),
    round (yd +. ra *. sin (pi +. alpha +. theta))
  in
  moveto (round xs) (round ys);
  lineto (round xd) (round yd);
  let x1,y1 = coords (pi /. 6.) in
  moveto (round xd) (round yd); lineto x1 y1;
  let x2,y2 = coords (-. pi /. 6.) in
  moveto (round xd) (round yd); lineto x2 y2

let color_vertex v color =
  let x,y = G.V.label v in
  set_color color;
  fill_circle x y vertex_radius

type selection =
  | No
  | One of G.V.t
  | Two of G.V.t * G.V.t

let selection = ref No

let draw_selection () = match !selection with
  | No -> ()
  | One v1 -> color_vertex v1 blue
  | Two (v1, v2) -> color_vertex v1 blue; color_vertex v2 green

let draw_graph () =
  clear_graph ();
  set_color red;
  set_line_width 1;
  G.iter_vertex
    (fun v ->
       let (x,y) = G.V.label v in
       draw_circle x y vertex_radius)
    !g;
  set_color black;
  G.iter_edges
    (fun v1 v2 -> draw_arrow (G.V.label v1) (G.V.label v2))
    !g;
  draw_selection ()

let distance (x1,y1) (x2,y2) =
  let dx = float (x1 - x2) in
  let dy = float (y1 - y2) in
  round (sqrt (dx *. dx +. dy *. dy))

let select () =
  let select_vertex v = match !selection with
    | No -> selection := One v
    | One v1 -> selection := Two (v1, v)
    | Two (_, v2) -> selection := Two (v2, v)
  in
  let p = mouse_pos () in
  try
    G.iter_vertex
      (fun v ->
	 if distance p (G.V.label v) <= vertex_radius then begin
	   select_vertex v; draw_graph (); raise Exit
	 end)
      !g
  with Exit ->
    ()

module W = struct
  type edge = G.E.t
  type t = int
  let weight x = G.E.label x
  let zero = 0
  let add = (+)
  let sub = (-)
  let compare = compare
end
module Dij = Path.Dijkstra(G)(W)

let dijkstra () = match !selection with
  | Two (v1, v2) ->
      printf "running Dijkstra... "; flush stdout;
      let t_ = ref 0.0 in
      begin try
	let (p,l),t = utime (Dij.shortest_path !g v1) v2 in
	t_ := t;
	printf "path of length %d (%d nodes) (%2.2f s)\n" l (List.length p) t;
	flush stdout;
	List.iter
	  (fun e ->
	     let v1 = G.E.src e in
	     let v2 = G.E.dst e in
	     draw_arrow ~color:red ~width:3 (G.V.label v1) (G.V.label v2))
	  p;
	ignore (Graphics.wait_next_event [ Key_pressed; Button_down ]);
	draw_graph ()
      with Not_found ->
	printf "no path (%2.2f s)\n" !t_; flush stdout
      end
  | _ ->
     ()

module J = Path.Johnson(G)(W)

let johnson () =
  match !selection with
  | Two (v1, v2) ->
     printf "running Johnson... "; flush stdout;
     let t_ = ref 0.0 in
     begin try
	 let paths, t = utime (J.all_pairs_shortest_paths) !g in
	 t_ := t;
	 printf "path of length %d (%2.2f s)\n" (J.HVV.find paths (v1, v2)) t;
	 flush stdout
       with Not_found ->
	 printf "no path (%2.2f s)\n" !t_; flush stdout
     end
  | _ -> ()

let draw_iteration f =
  let pause () = for i = 1 to 10000000 do () done in
  f (fun v -> color_vertex v red; pause ()) !g;
  ignore (Graphics.wait_next_event [ Key_pressed; Button_down ]);
  draw_graph ()

module Dfs = Traverse.Dfs(G)
let dfs () = draw_iteration Dfs.prefix
module Bfs = Traverse.Bfs(G)
let bfs () = draw_iteration Bfs.iter

let golden_ratio = 0.618033988749895

let hsv_to_rgb h s v =
  let c = v *. s in
  let h = int_of_float h in
  let hh = (h mod 360)/60 in
  let hhf = (mod_float (float_of_int h) 360.) /. 60. in
  let x = c *. (1. -. (abs_float (mod_float hhf 2. -. 1.))) in
  let m = v -. c in
  let cc = int_of_float ((c +. m) *. 255.) in
  let xx = int_of_float ((x +. m) *. 255.) in
  let mm = int_of_float (m *. 255.) in
  match hh with
  | 0 -> cc, xx, mm
  | 1 -> xx, cc, mm
  | 2 -> mm, cc, xx
  | 3 -> mm, xx, cc
  | 4 -> xx, mm, cc
  | 5 -> cc, mm, xx
  | _ -> mm, mm, mm

module Scc = Components.Make(G)
let scc () =
  printf "running scc ... "; flush stdout;
  let (n_scc, map_scc) = Scc.scc !g in
  printf "number of components: %d\n" n_scc; flush stdout;
  let colors = Hashtbl.create n_scc in
  let inc = golden_ratio *. 360. in
  Random.self_init ();
  let h = ref (Random.float 360.) in
  G.iter_vertex (
      fun v -> try let color = Hashtbl.find colors (map_scc v) in
		   color_vertex v color
	       with Not_found ->
		 let color = hsv_to_rgb !h 0.7 0.95 in
		 h := !h +. inc;
		 let rgb (r, g, b) = rgb r g b in
		 let c = rgb color in
		 Hashtbl.add colors (map_scc v) c;
		 color_vertex v c) !g

(* brute-force coloring *)
let four_colors () =
  (* vertices still to be colored are queued in [q] *)
  let q = Queue.create () in
  let rec loop () =
    if not (Queue.is_empty q) then begin
      let v = Queue.pop q in
      assert (Mark.get v == 0);
      try_color v 1 ||
      try_color v 2 ||
      try_color v 3 ||
      try_color v 4 ||
      (Mark.set v 0; Queue.add v q; false)
    end else
      true
  and try_color v c =
    (try
       G.iter_succ (fun w -> if Mark.get w == c then raise Exit) !g v; true
     with Exit ->
       false) &&
    (Mark.set v c; loop ())
  in
  G.iter_vertex (fun v -> Queue.add v q) !g;
  Mark.clear !g;
  assert (loop ());
  let color = [| black; red; green; blue; yellow |] in
  G.iter_vertex (fun v -> color_vertex v (color.(Mark.get v))) !g;
  ignore (Graphics.wait_next_event [ Key_pressed; Button_down ])

let () =
  try
    let () = draw_graph () in
    while true do
      let st = Graphics.wait_next_event [ Key_pressed; Button_down ] in
      if st.keypressed then match st.key with
	| 'q' -> raise Exit
	| 'r' -> g := new_graph (); selection := No; draw_graph ()
	| 'p' -> dijkstra ()
	| 'd' -> dfs ()
	| 'b' -> bfs ()
	| 'x' -> dump_graph ()
	| 'c' -> scc ()
	| 'j' -> johnson ()
	(* | 'c' -> four_colors () *)
	| _ -> ()
      else if st.button then
	select ()
    done
  with Exit ->
    close_graph ()

(*
Local Variables:
compile-command: "make -C .. bin/demo_planar.opt"
End:
*)
