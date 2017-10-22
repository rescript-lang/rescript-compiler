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

(* 4-coloring planar graphs *)

open Printf
open Graph

(* command line *)
let n_ = ref 30
let prob_ = ref 0.5
let seed_ = ref None

let arg_spec =
  ["-v", Arg.Int (fun i -> n_ := i),
   " <int>  number of vertices";
   "-prob", Arg.Float (fun f -> prob_ := f),
   " <float>  probability to discrad an edge";
   "-seed", Arg.Int (fun n -> seed_ := Some n),
   " <int>  random seed"
  ]
let () = Arg.parse arg_spec (fun _ -> ()) "usage: color <options>"

let n = !n_
let prob = !prob_

let seed = match !seed_ with
  | None -> Random.self_init (); Random.int (1 lsl 29)
  | Some s -> s
let () = Format.printf "seed = %d@." seed; Random.init seed

(* undirected graphs with integer coordinates and integer labels on edges *)

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
module G = Imperative.Graph.AbstractLabeled(IntInt)(Int)
open G

(* a random graph with n vertices *)
module R = Rand.Planar.I(G)
let g0 = R.graph ~xrange:(20,780) ~yrange:(20,580) ~prob n

(* drawing *)
let round f = truncate (f +. 0.5)
let pi = 4.0 *. atan 1.0

open Graphics
let () = open_graph " 800x600"

let vertex_radius = 5

let draw_edge v1 v2 =
  let (xu,yu) = G.V.label v1 in
  let (xv,yv) = G.V.label v2 in
  set_color black;
  let dx = float (xv - xu) in
  let dy = float (yv - yu) in
  let r = sqrt (dx *. dx +. dy *. dy) in
  let d = float vertex_radius +. 3. in
  let xs, ys = float xu +. d *. dx /. r, float yu +. d *. dy /. r in
  let xd, yd = float xv -. d *. dx /. r, float yv -. d *. dy /. r in
  moveto (round xs) (round ys);
  lineto (round xd) (round yd)

let draw_vertex v =
  let (x,y) = G.V.label v in
  set_color red;
  draw_circle x y vertex_radius

let color_vertex v color =
  let x,y = G.V.label v in
  set_color color;
  fill_circle x y vertex_radius

let draw_graph () =
  clear_graph ();
  set_color red;
  set_line_width 1;
  G.iter_vertex draw_vertex g0;
  G.iter_edges draw_edge g0

module Dfs = Traverse.Dfs(G)
module Bfs = Traverse.Bfs(G)

let test_bfs () =
  let rec loop i =
    let v = Bfs.get i in
    color_vertex v red;
    ignore (Graphics.wait_next_event [ Key_pressed ]);
    loop (Bfs.step i)
  in
  try loop (Bfs.start g0) with Exit -> ()

let test_dfs () =
  let rec loop i =
    let v = Dfs.get i in
    color_vertex v red;
    ignore (Graphics.wait_next_event [ Key_pressed ]);
    loop (Dfs.step i)
  in
  try loop (Dfs.start g0) with Exit -> ()

let cols = [| white; red; green; blue; yellow; black |]
exception NoColor

(* Algo I. Brute force. *)

module C = Coloring.Mark(G)

let coloring_a k =
  Mark.clear g0;
  C.coloring g0 4;
  iter_vertex (fun v -> color_vertex v cols.(Mark.get v)) g0

(* Algo II.

   we use marks to color; bits are used as follows:
     0: set if node is discarded at step 1
   1-4: available colors
   5-7: the color (0 = not colored, else color in 1..4
 *)

let print_8_bits x =
  for i = 7 downto 0 do
    if (x lsr i) land 1 = 1 then printf "1" else printf "0"
  done

let dump () =
  let dump_mark v = printf "["; print_8_bits (Mark.get v); printf "]" in
  iter_vertex dump_mark g0;
  printf "\n"; flush stdout

let mask_color = [| 0; 0b11101; 0b11011; 0b10111; 0b01111 |]

let coloring_b () =
  (* initially all 4 colors available and every vertex to be colored *)
  iter_vertex (fun v -> Mark.set v 0b11110) g0;
  (* first step: we eliminate vertices with less than 4 successors *)
  let stack = Stack.create () in
  let finish = ref false in
  let round = ref 1 in
  let nb_to_color = ref n in
  while not !finish do
    let c = ref 0 in
    finish := true;
    let erase v =
      incr c; finish := false; Mark.set v 0b11111; Stack.push v stack
    in
    G.iter_vertex
      (fun v -> if Mark.get v = 0 && out_degree g0 v < 4 then erase v)
      g0;
    printf "round %d: removed %d vertices\n" !round !c;
    incr round;
    nb_to_color := !nb_to_color - !c
  done;
  flush stdout;
  (* second step: we 4-color the remaining of the graph *)
  (* [try_color v i] tries to assigne color [i] to vertex [v] *)
  let try_color v i =
    assert (1 <= i && i <= 4);
    let m = Mark.get v in
    assert (m lsr 5 = 0);
    if (m lsr i) land 1 = 0 then raise NoColor; (* color [i] not available *)
    let remove_color w =
      (* make color [i] unavailable for [w] *)
      let m = Mark.get w in
      if m lsr 5 > 0 then
	assert (m lsr 5 <> i) (* [w] already colored *)
      else begin
	let m' = m land mask_color.(i) in
	if m' = 0 then raise NoColor; (* no more color available for [w] *)
	Mark.set w m'
      end
    in
    iter_succ remove_color g0 v;
    Mark.set v (m lor (i lsl 5))
  in
  let uncolor v =
    let m = Mark.get v in
    let c = m lsr 5 in
    assert (0 <= c && c <= 4);
    if c > 0 then begin
      Mark.set v (m land 0b11111);
      let update w =
	(* give back color [c] to [w] only when no more succ. has color [c] *)
	try
	  iter_succ (fun u -> if Mark.get u lsr 5 = c then raise Exit) g0 w;
	  Mark.set w ((Mark.get w) lor (1 lsl c))
	with Exit ->
	  ()
      in
      iter_succ update g0 v
    end
  in
  if !nb_to_color > 0 then begin
    let rec iterate iter =
      let v = Bfs.get iter in
      if Mark.get v land 1 = 1 then
	(* no need to color this vertex *)
	iterate (Bfs.step iter)
      else begin
	for i = 1 to 4 do
	  try try_color v i; iterate (Bfs.step iter); assert false
	  with NoColor -> uncolor v
	done;
	raise NoColor
      end
    in
    try iterate (Bfs.start g0) with Exit -> ()
  end;
  (* third step: we color the eliminated vertices, in reverse order *)
  Stack.iter
    (fun v ->
       assert (Mark.get v land 1 = 1);
       try
	 for i = 1 to 4 do
	   try try_color v i; raise Exit with NoColor -> uncolor v
	 done;
	 assert false (* we must succeed *)
       with Exit -> ())
    stack;
  (* finally we display the coloring *)
  iter_vertex
    (fun v ->
       let c = (Mark.get v) lsr 5 in
       assert (1 <= c && c <= 4);
       color_vertex v cols.(c))
    g0

open Unix

let utime f x =
  let u = (times()).tms_utime in
  let y = f x in
  let ut = (times()).tms_utime -. u in
  (y,ut)

let print_utime f x =
  let (y,ut) = utime f x in
  Format.printf "user time: %2.2f@." ut;
  y

let () =
  draw_graph ();
  (* test_bfs (); *)
  (* test_dfs (); *)
  print_utime coloring_a 4;
  (*ignore (Graphics.wait_next_event [ Key_pressed ]);*)
  (*draw_graph ();*)
  print_utime coloring_b ();
  ignore (Graphics.wait_next_event [ Key_pressed ]);
  close_graph ()


(*
Local Variables:
compile-command: "make -C .. bin/color.opt"
End:
*)
