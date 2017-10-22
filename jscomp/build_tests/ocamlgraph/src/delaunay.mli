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

(* $Id: delaunay.mli,v 1.8 2004-02-20 14:37:40 signoles Exp $ *)

(** Delaunay triangulation. *)

(** Delaunay triangulation is available for any CCC system in the sense
    of Knuth's ``Axioms and Hulls'' *)
module type CCC = sig

  type point

  val ccw : point -> point -> point -> bool
  (** The counterclockwise relation [ccw p q r] states that the
      circle through points [(p,q,r)] is traversed counterclockwise
      when we encounter the points in cyclic order [p,q,r,p,...] **)

  val in_circle : point -> point -> point -> point -> bool
  (** The relation [in_circle p q r s] states that [s] lies
      inside the circle [(p,q,r)] if [ccw p q r] is true, or outside that
      circle if [ccw p q r] is false. *)

end

(** The result of triangulation is an abstract value of type [triangulation].
    Then one can iterate over all edges of the triangulation. *)
module type Triangulation = sig

  module S : CCC

  type triangulation

  val triangulate : S.point array -> triangulation
  (** [triangulate a] computes the Delaunay triangulation of a set of
      points, given as an array [a]. If [N] is the number of points
      (that is [Array.length a]), then the running time is $O(N \log N)$
      on the average and $O(N^2)$ on the worst-case. The space used is
      always $O(N)$. *)

  val iter : (S.point -> S.point -> unit) -> triangulation -> unit
  (** [iter f t] iterates over all edges of the triangulation [t].
      [f u v] is called once for each undirected edge [(u,v)]. *)

  val fold : (S.point -> S.point -> 'a -> 'a) -> triangulation -> 'a -> 'a

  val iter_triangles :
    (S.point -> S.point -> S.point -> unit) -> triangulation -> unit

end

(** Generic Delaunay triangulation *)
module Make(S : CCC) : Triangulation with module S = S

(** Points with integer coordinates *)
module IntPoints : CCC with type point = int * int

(** Delaunay triangulation with integer coordinates *)
module Int : Triangulation with module S = IntPoints

(** Points with floating point coordinates *)
module FloatPoints : CCC with type point = float * float

(** Delaunay triangulation with floating point coordinates *)
module Float : Triangulation with module S = FloatPoints
