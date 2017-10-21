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

(*** Signatures ***)

module type TREE = sig
  type t
  type label
  val children : t -> t list
  val label : t -> label
end ;;

module type HTREE = sig
  type t
  type label
  val children : t -> t list
  val label : t -> label

  type coord = float * float

  type driver = {
    rlimit : float ;
    moveto : coord -> unit ;
    lineto : coord -> unit ;
    curveto : coord -> coord -> coord -> unit ;
    draw_label : label -> coord -> float -> unit ;
    init_edge_pass : unit -> unit ;
    init_label_pass : unit -> unit ;
    finalize : unit -> unit ;
  }

  val shrink_factor : coord -> float
  val drag_origin : coord -> coord -> coord -> coord

  val draw_linear_tree : driver -> t -> coord -> float -> unit
  val draw_curved_tree : driver -> t -> coord -> float -> unit
end ;;

(*** Complex operators ***)

let ( ~-& ) (x, y) = (-.x, -.y) ;;
let ( ~& ) (x, y) = (x, -.y) ;;

let ( +& ) (x1, y1) (x2, y2) =
  (x1 +. x2, y1 +. y2) ;;

let ( -& ) (x1, y1) (x2, y2) =
  (x1 +. x2, y1 +. y2) ;;

let ( *& ) (x1, y1) (x2, y2) =
  (x1*.x2 -. y1*.y2, x1*.y2 +. y1*.x2) ;;

let ( /& ) (x1, y1) (x2, y2) =
  let n2 = x2*.x2 +. y2*.y2 in
  ((x1*.x2 +. y1*.y2)/.n2, (-.x1*.y2 +. y1*.x2)/.n2) ;;

let ( *.& ) f (x, y) =
  (f*.x, f*.y) ;;

let norm_sqr (x, y) =
  x*.x +. y*.y ;;

let norm (x, y) =
  sqrt(x*.x +. y*.y) ;;

let normalize (x, y) =
  let n = sqrt(x*.x +. y*.y) in
  (x/.n, y/.n) ;;

let expi t =
  (cos t, sin t) ;;

(*** Hyperbolic functions ***)

let th t =
  let ept = exp t
  and emt = exp (-.t) in
  (ept -. emt)/.(ept +. emt) ;;

let ath x =
  0.5*.log((1.0 +. x)/.(1.0 -. x)) ;;

let pi = 3.14159265358979323846 ;;
let pi_over_2 = pi/.2.0 ;;
let pi_over_4 = pi/.4.0 ;;

let one = (1.0, 0.0) ;;

let translate a z =
  (a +& z)/&(one +& (~&a) *& z) ;;

let gamma a u t =
  let utht = th t *.& u in
  (a +& utht) /& (one +& (~&a) *& utht) ;;

let delta a u t =
  let atht = th t *.& a
  and utht = th t *.& u in
  normalize ((u +& atht) /& (one +& (~&a) *& utht)) ;;

(*** The hyperbolic turtle ***)

type coord = float * float ;;

type turtle = {
  pos : coord ;  (* with |pos| < 1 *)
  dir : coord    (* with |dir| = 1 *)
} ;;

let make_turtle pos angle =
  { pos = pos ;
    dir = expi angle } ;;

let advance turt step =
  { pos = gamma turt.pos turt.dir step ;
    dir = delta turt.pos turt.dir step } ;;

let turn turtle u =
  { pos = turtle.pos ;
    dir = turtle.dir *& u } ;;

let turn_left turtle angle =
  turn turtle (expi angle) ;;

let turn_right turtle angle =
  turn turtle (expi (-.angle)) ;;

(*** Tree-independent functions ***)

let shrink_factor (x, y) =
  1.0 -. (x*.x +. y*.y)

(* solving a Cramer system *)
let cramer a1 a2 b1 b2 c1 c2 =
  let cdet = a1*.b2 -. a2*.b1
  and xdet = c1*.b2 -. c2*.b1
  and ydet = a1*.c2 -. a2*.c1 in
  (xdet/.cdet, ydet/.cdet) ;;

let drag_origin (x0, y0) (x1, y1) (x2, y2) =
  let (x1, y1) = translate (-.x0, -.y0) (x1, y1) in
  let x3 = x1*.x2 -. y1*.y2 in
  let y3 = x1*.y2 +. y1*.x2 in
  cramer (1.0 -. x3) (-.y3) (-.y3) (1.0 +. x3) (x2 -. x1) (y2 -. y1)

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

(*** The functor ***)

module Make(T : TREE) = struct
  type t = T.t
  type label = T.label
  let children = T.children
  let label = T.label

  type coord = float * float

  type driver = {
    rlimit : float ;
    moveto : coord -> unit ;
    lineto : coord -> unit ;
    curveto : coord -> coord -> coord -> unit ;
    draw_label : label -> coord -> float -> unit ;
    init_edge_pass : unit -> unit ;
    init_label_pass : unit -> unit ;
    finalize : unit -> unit ;
  }

  let shrink_factor = shrink_factor
  let drag_origin = drag_origin

  let draw_linear_edges drv tree turtle =
    let rlimit_sqr = drv.rlimit*.drv.rlimit in
    let rec do_tree tur t =
      if hspace_dist_sqr tur <= rlimit_sqr then
        begin
          let list = children t in
          let num = List.length list in
          if num > 0 then
            begin
              let step = step_from (2*(max 3 num))
              and angle = pi/.float(num) in
              let tur' = turn_left tur (angle/.2.0 -. pi_over_2) in
              do_list tur' step (expi angle) list
            end
        end
    and do_list tur step u = function
      |  [] -> ()
      |  t :: rest ->
        drv.moveto tur.pos ;
        let tur' = advance tur step in
        drv.lineto tur'.pos ;
        do_tree tur' t ;
        do_list (turn tur u) step u rest in
    do_tree turtle tree

  let draw_curved_edges drv tree turtle =
    let rlimit_sqr = drv.rlimit*.drv.rlimit in
    let rec do_tree tur t =
      if hspace_dist_sqr tur <= rlimit_sqr then
        begin
          let list = children t in
          let num = List.length list in
          if num > 0 then
            begin
              let step = step_from (2*(max 3 num))
              and angle = pi/.float(num) in
              let tur' = turn_left tur (angle/.2.0 -. pi_over_2) in
              do_list tur' step (expi angle) list
            end
        end
    and do_list tur step u = function
      |  [] -> ()
      |  t :: rest ->
        drv.moveto tur.pos ;
        let tur' = advance tur step in
        let (a0x, a0y) = tur.pos
        and (u0x, u0y) = tur.dir
        and (a3x, a3y) = tur'.pos
        and (u3x, u3y) = tur'.dir in
        let dx = a3x -. a0x
        and dy = a3y -. a0y in
        let k = sqrt(dx*.dx +. dy*.dy)/.3.0 in
        let a1 = (a0x +. k*.u0x, a0y +. k*.u0y)
        and a2 = (a3x -. k*.u3x, a3y -. k*.u3y) in
        drv.moveto tur.pos ;
        drv.curveto a1 a2 tur'.pos ;
        do_tree tur' t ;
        do_list (turn tur u) step u rest in
    do_tree turtle tree

  let draw_labels drv tree turtle =
    let rlimit_sqr = drv.rlimit*.drv.rlimit in
    let rec do_tree tur t =
      if hspace_dist_sqr tur <= rlimit_sqr then
        begin
          drv.draw_label (label t) tur.pos (shrink_factor tur.pos) ;
          let list = children t in
          let num = List.length list in
          if num > 0 then
            begin
              let step = step_from (2*(max 3 num))
              and angle = pi/.float(num) in
              let tur' = turn_left tur (angle/.2.0 -. pi_over_2) in
              do_list tur' step (expi angle) list
            end
        end
    and do_list tur step u = function
      |  [] -> ()
      |  t :: rest ->
        let tur' = advance tur step in
        do_tree tur' t ;
        do_list (turn tur u) step u rest in
    do_tree turtle tree

  let draw_linear_tree drv tree start angle =
    let turt = make_turtle start angle in
    drv.init_edge_pass () ;
    draw_linear_edges drv tree turt ;
    drv.init_label_pass () ;
    draw_labels drv tree turt ;
    drv.finalize ()

  let draw_curved_tree drv tree start angle =
    let turt = make_turtle start angle in
    drv.init_edge_pass () ;
    draw_curved_edges drv tree turt ;
    drv.init_label_pass () ;
    draw_labels drv tree turt ;
    drv.finalize ()
end
