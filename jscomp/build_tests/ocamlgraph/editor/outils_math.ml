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

(*** Operateurs Complex ***)

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

(*** Fonctions Hyperboliques ***)

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
  normalize ((u +& atht) /& (one +& (~&a) *& utht)) 

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

