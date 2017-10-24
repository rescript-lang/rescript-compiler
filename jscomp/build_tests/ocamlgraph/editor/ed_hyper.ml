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

(* This code is from Alexandre Miquel's Htree
   (http://www.pps.jussieu.fr/~miquel/soft.html) *)

(*** Complex numbers *)

let ( ~-& ) (x, y) = (-.x, -.y) 
let ( ~& ) (x, y) = (x, -.y) 

let ( +& ) (x1, y1) (x2, y2) =
  (x1 +. x2, y1 +. y2) 

let ( -& ) (x1, y1) (x2, y2) =
  (x1 +. x2, y1 +. y2) 

let ( *& ) (x1, y1) (x2, y2) =
  (x1*.x2 -. y1*.y2, x1*.y2 +. y1*.x2) 

let ( /& ) (x1, y1) (x2, y2) =
  let n2 = x2*.x2 +. y2*.y2 in
  ((x1*.x2 +. y1*.y2)/.n2, (-.x1*.y2 +. y1*.x2)/.n2) 

let ( *.& ) f (x, y) =
  (f*.x, f*.y) 

let norm_sqr (x, y) =
  x*.x +. y*.y 

let norm (x, y) =
  sqrt(x*.x +. y*.y) 

let normalize (x, y) =
  let n = sqrt(x*.x +. y*.y) in
  (x/.n, y/.n) 

let expi t =
  (cos t, sin t) 

(*** Hyperbolic geometry ***)

let th t =
  let ept = exp t
  and emt = exp (-.t) in
  (ept -. emt)/.(ept +. emt)

let ath x =
  0.5*.log((1.0 +. x)/.(1.0 -. x)) 

let pi = 3.14159265358979323846 
let pi_over_2 = pi/.2.0 
let pi_over_4 = pi/.4.0 

let one = (1.0, 0.0) 

let translate a z =
  (a +& z)/&(one +& (~&a) *& z) 

let gamma a u t =
  let utht = th t *.& u in
  (a +& utht) /& (one +& (~&a) *& utht) 

let delta a u t =
  let atht = th t *.& a
  and utht = th t *.& u in
  (u +& atht) /& (one +& (~&a) *& utht)

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

let shrink_factor (x, y) =
  1.0 -. (x*.x +. y*.y)




(*** Hyperbolic turtle ***)

type coord = float * float 

type turtle =
  {
    pos : coord ;  (* with |pos| < 1 *)
    dir : coord    (* with |dir| = 1 *)
  } 

let make_turtle pos angle =
  { 
    pos = pos ;
    dir = expi angle 
  }

let make_turtle_dir pos dir =
  { 
    pos = pos ;
    dir = dir 
  }


let dist tdep tdest = 
  let a = tdep.pos in
  let b = tdest.pos in
  ath (norm ( (a -& b) /& (one -& (~&a) *& b)))

let dir_to tdep tdest t =
  let a = tdep.pos in
  let d = tdest.pos in
  ((d -& a) /& (th t *.&( one -& (~&a) *& d))) 



(* return a turtle for a distance from original *)
let advance turtle step =
  { pos = gamma turtle.pos turtle.dir step ;
    dir = delta turtle.pos turtle.dir step }

(* return a turtle for a distance d from original with steps  *)
let advance_many turtle d steps =
  let d = d /. (float steps) in
  let rec adv t = function 
    | 0 -> t
    | n -> adv (advance t d) (n-1)
  in
  adv turtle steps

(* return a list of turtle along distance d from original turtle with steps  *)
let list_advance_many turtle d steps =
  let d = d /. (float steps) in
  let rec adv t = function 
    | 0 -> []
    | n -> t :: adv (advance t d) (n-1)
  in
  adv turtle steps

let turn turtle u =
  { turtle with dir = turtle.dir *& u }

let turn_left turtle angle =
  turn turtle (expi angle)       (*** a comprendre pourquoi je dois inverser + et - de l'angle ***)

let turn_right turtle angle =
  turn turtle (expi (-.angle))           (*** a comprendre pourquoi je dois inverser + et - de l'angle ***) 

let dummy_turtle = { pos = (0., 0.); dir = (0., 0.) }




(* [step_from n] computes the best `distance' for solving the
   dictator's problem in the complex hyperbolic plane for [n]
   dictators.  In a half-plane, we have to use the distance
   given by [step_from (2*n)] or, better, the distance given
   by [step_from (2*max(3 n))]. *)
let step_from n =
  ath (tan (pi_over_4 -. pi/.float(2*n)))


(* [hspace_dist_sqr turtle] computes the square of the distance
   between the origin and the half-space in front of [turtle]. *)
let hspace_dist_sqr turtle  =
  let (ax, ay) = turtle.pos
  and (dx, dy) = turtle.dir in
  if ax*.dx +. ay*.dy < 0.0
  then begin 
    0.0 
  end else begin
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
        let res = zx*.zx +. zy*.zy in
        res
      end
  end



(* Limit of visibility for nodes *)
let rlimit =  0.98
let rlimit_sqr = rlimit *. rlimit


