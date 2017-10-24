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

open Sys
open Format
open Graph

let v_ = ref 30
let prob_ = ref 0.5
let seed_ = ref None
let interactive_ = ref false

type algo = TransitiveClosure | TransitiveReduction | Prim | Kruskal |
    Dijkstra | Bfs | Dfs
let algo = ref None

let arg_spec =
  ["-v", Arg.Int (fun i -> v_ := i),
   " <int>  number of vertices";
   "--prob", Arg.Float (fun f -> prob_ := f),
   " <float>  probability to discrad an edge";
   "--seed", Arg.Int (fun n -> seed_ := Some n),
   " <int>  random seed";
   "--transitive-closure", Arg.Unit (fun () -> algo := Some TransitiveClosure),
   "  display transitive closure in blue";
   "--transitive-reduction", Arg.Unit (fun () ->
     algo := Some TransitiveReduction),
   "  display useless edges in blue";
   "--prim", Arg.Unit (fun () -> algo := Some Prim),
   "  Prim's algorithm";
   "--kruskal", Arg.Unit (fun () -> algo := Some Kruskal),
   "  Kruskal's algorithm";
   "--dijkstra", Arg.Unit (fun () -> algo := Some Dijkstra),
   "  Dijkstra's algorithm";
   "--dfs", Arg.Unit (fun () -> algo := Some Dfs),
   "  Depth-First Search's algorithm";
   "--bfs", Arg.Unit (fun () -> algo := Some Bfs),
   "  Breadth-First Search's algorithm";
   "-i", Arg.Set interactive_, " run algorithms interactively";
  ]
let () = Arg.parse arg_spec (fun _ -> ()) "usage: color <options>"

let v = !v_
let prob = !prob_
let interactive = !interactive_

let seed = match !seed_ with
  | None -> Random.self_init (); Random.int (1 lsl 29)
  | Some s -> s
let () = printf "seed = %d@." seed; Random.init seed

let () = if interactive then
    printf "interactive mode (press any key to step in algorithm, q to quit)@."

module G = struct

  module IntInt = struct
    type t = int * int
    let compare = Pervasives.compare
    let equal = (=)
    let hash = Hashtbl.hash
  end
  module Int = struct
    type t = int
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = (=)
    let default = 0
  end

  include Imperative.Digraph.ConcreteLabeled(IntInt)(Int)

end

(* a random graph with n vertices *)
module R = Rand.Planar.I(G)
let g = R.graph ~xrange:(20,780) ~yrange:(20,580)~prob v

module Draw = struct

  open Graphics
  let () = open_graph " 800x600"

  let vertex_radius = 5
  let round f = truncate (f +. 0.5)
  let pi = 4.0 *. atan 1.0

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

  let draw_edge ?color ?width v1 v2 =
    draw_arrow ?color ?width (G.V.label v1) (G.V.label v2)

  let draw_vertex ?(color=red) ?(width=1) v =
    let (x,y) = G.V.label v in
    set_line_width width;
    set_color color;
    draw_circle x y vertex_radius

  let color_vertex v color =
    let x,y = G.V.label v in
    set_color color;
    fill_circle x y vertex_radius

  let draw_graph ?color ?width g =
    clear_graph ();
    G.iter_vertex draw_vertex g;
    G.iter_edges (draw_edge ?color ?width) g

  let draw_edges ?(color=blue) ?(width=2) el =
    List.iter
      (fun e ->
    	draw_edge ~color ~width (G.E.src e) (G.E.dst e);
    	draw_vertex ~color ~width (G.E.src e);
    	draw_vertex ~color ~width (G.E.dst e)
      ) el

  let pause () =
    let st = wait_next_event [Key_pressed] in
    if st.key = 'q' then begin close_graph (); exit 0 end

  let draw_iteration ?(interactive=false) f g =
    f (fun v -> color_vertex v Graphics.red; if interactive then pause ()) g

end

module W = struct
  type edge = G.E.t
  type t = int
  let weight = G.E.label
  let zero = 0
  let add = (+)
  let sub = (-)
  let compare = compare
end

module PathWeight = struct
  type edge = G.E.t
  type t = int
  let weight x = G.E.label x
  let zero = 0
  let add = (+)
  let sub = (-)
  let compare = compare
end

module Selection = struct

  type selection =
    | No
    | One of G.V.t
    | Two of G.V.t * G.V.t

  let selection = ref No

  let draw_selection () = match !selection with
    | No -> ()
    | One v1 -> Draw.color_vertex v1 Graphics.blue
    | Two (v1, v2) -> Draw.color_vertex v1 Graphics.blue;
      Draw.color_vertex v2 Graphics.green

  let distance (x1,y1) (x2,y2) =
    let dx = float (x1 - x2) in
    let dy = float (y1 - y2) in
    Draw.round (sqrt (dx *. dx +. dy *. dy))

  let select g =
    let select_vertex v = match !selection with
      | No -> selection := One v
      | One v1 -> selection := Two (v1, v)
      | Two (_, v2) -> selection := Two (v2, v)
    in
    let p = Graphics.mouse_pos () in
    try
      G.iter_vertex
	(fun v ->
	  if distance p (G.V.label v) <= Draw.vertex_radius then begin
	    select_vertex v; Draw.draw_graph g; draw_selection (); raise Exit
	  end)
	g
    with Exit -> ()

  let select2 g =
    printf "please select two vertices...@.";
    printf "press r to run...@.";
    printf "press q to quit...@.";
    let continue = ref true in
    while !continue do
      let st = Graphics.wait_next_event [ Graphics.Key_pressed;
					  Graphics.Button_down ] in
      if st.Graphics.keypressed then match st.Graphics.key with
	| 'r' ->
	  begin match !selection with
	    | Two (_,_) -> continue := false
	    | _ -> printf "please select two vertices...@."
	  end
	| 'q' -> raise Exit
	| _ -> ()
      else if st.Graphics.button then
	select g
    done;
    match !selection with
      | Two (v1,v2) -> (v1,v2)
      | _ -> assert false
end

let () = Draw.draw_graph g

let () = match !algo with
  | Some Dijkstra -> ()
  | _ ->  ignore (Graphics.wait_next_event [Graphics.Key_pressed ])

let () = match !algo with
  | Some TransitiveClosure ->
    let module O = Oper.I(G) in
    let tg = O.transitive_closure g in
    G.iter_edges
      (fun v1 v2 ->
	if not (G.mem_edge g v1 v2) then
	  Draw.draw_edge ~color:Graphics.blue v1 v2) tg
  | Some TransitiveReduction ->
    Draw.draw_graph ~color:Graphics.blue g;
    let module O = Oper.I(G) in
    let tr = O.transitive_reduction g in
    G.iter_edges Draw.draw_edge tr;
    ignore (Graphics.wait_next_event [Graphics.Key_pressed ]);
    Draw.draw_graph tr
  | Some Prim ->
    let module P = Prim.Make(G)(W) in
    let el = P.spanningtree g in
    Draw.draw_edges el
  | Some Kruskal ->
    let module P = Kruskal.Make(G)(W) in
    let el = P.spanningtree g in
    Draw.draw_edges el
  | Some Dijkstra ->
    let module Dij = Path.Dijkstra(G)(PathWeight) in
    let rec recherche () =
      let (v1, v2) = Selection.select2 g in
      begin
	try
	  let (p, _) = Dij.shortest_path g v1 v2 in
	  Draw.draw_edges ~color:Graphics.red p
	with Not_found ->
	  printf "no path found...@.";
	  recherche ()
      end
    in
    recherche ()
  | Some Dfs ->
    let module Dfs = Traverse.Dfs(G) in
    Draw.draw_iteration ~interactive Dfs.prefix g
  | Some Bfs ->
    let module Bfs = Traverse.Bfs(G) in
    Draw.draw_iteration ~interactive Bfs.iter g
  | None -> ()

let () =
  ignore (Graphics.wait_next_event [Graphics.Key_pressed ]);
  Graphics.close_graph ()

(*
Local Variables:
compile-command: "make -C .. bin/demo.opt"
End:
*)
