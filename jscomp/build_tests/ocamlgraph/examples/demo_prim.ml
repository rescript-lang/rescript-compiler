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

(* Demo of Prim's algorithm *)

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

let draw_edge ?(color=black) v1 v2 =
  let (xu,yu) = G.V.label v1 in
  let (xv,yv) = G.V.label v2 in
  set_color color;
  let dx = float (xv - xu) in
  let dy = float (yv - yu) in
  let r = sqrt (dx *. dx +. dy *. dy) in
  let d = float vertex_radius +. 3. in
  let xs, ys = float xu +. d *. dx /. r, float yu +. d *. dy /. r in
  let xd, yd = float xv -. d *. dx /. r, float yv -. d *. dy /. r in
  moveto (round xs) (round ys);
  lineto (round xd) (round yd)

let draw_vertex ?(color=red) v =
  let (x,y) = G.V.label v in
  set_color color;
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
module W = struct
  type label = G.E.label
  type t = int
  let weight x = x
  let zero = 0
  let add = (+)
  let compare = compare
end
module P = Prim.Make(G)(W)

let () =
  draw_graph ();
  ignore (Graphics.wait_next_event [ Key_pressed ]);
  let el = P.spanningtree g0 in
  set_line_width 2;
  List.iter
    (fun e ->
      draw_edge ~color:blue (G.E.src e) (G.E.dst e);
      draw_vertex ~color:blue (G.E.src e);
      draw_vertex ~color:blue (G.E.dst e)
    ) el;
  ignore (Graphics.wait_next_event [ Key_pressed ]);
  close_graph ()


(*
Local Variables:
compile-command: "make -C .. bin/demo_prim.opt"
End:
*)
