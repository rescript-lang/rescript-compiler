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

(* $Id: rand.ml,v 1.18 2005-03-31 13:32:51 filliatr Exp $ *)

module type S = sig
  type graph
  type vertex
  type edge_label
  val graph : ?loops:bool -> v:int -> e:int -> unit -> graph
  val labeled :
    (vertex -> vertex -> edge_label) ->
    ?loops:bool -> v:int -> e:int -> unit -> graph
  (* DEBUG *)
  val random_few_edges : loops:bool -> v:int -> e:int -> graph
  val random_many_edges : loops:bool -> v:int -> e:int -> graph
  val gnp : ?loops:bool -> v:int -> prob:float -> unit -> graph
  val gnp_labeled :
    (vertex -> vertex -> edge_label) ->
    ?loops:bool -> v:int -> prob:float -> unit -> graph
end

module Make(B : Builder.INT) = struct

  open B
  type graph = G.t
  type vertex = G.V.t
  type edge_label = G.E.label

  open Int64

  let max_edges ~loops ~v ~e =
    if v <= 0 || e < 0 then invalid_arg "random";
    let v64 = of_int v in
    let max_e = mul v64 (pred v64) in
    let max_e = if G.is_directed then max_e else div max_e (of_int 2) in
    let max_e = if loops then add max_e v64 else max_e in
    if of_int e > max_e then invalid_arg "random: too many edges";
    max_e

  let fold_for i0 i1 f =
    let rec loop i v = if i > i1 then v else loop (i + 1) (f v i) in
    loop i0

  (* naive implementation: we randomly chose edges up to [e] different edges *)
  let random_few_edges add_edge ~loops ~v ~e =
    let _ = max_edges ~loops ~v ~e in
    let a = Array.init v G.V.create in
    let g = Array.fold_left add_vertex (empty ()) a in
    let rec random_edge g =
      let i = Random.int v in
      let j = Random.int v in
      if (i = j && not loops) || G.mem_edge g a.(i) a.(j) then
        random_edge g
      else
        add_edge g a.(i) a.(j)
    in
    fold_for 1 e (fun g _ -> random_edge g) g

  (* other implementation in O(v * v); faster when [e] is large *)
  let random_many_edges add_edge ~loops ~v ~e =
    let v64 = of_int v in
    let max_e = max_edges ~loops ~v ~e in
    let a = Array.init v G.V.create in
    let g = Array.fold_left add_vertex (empty ()) a in
    let rec add_edges i j max nb g =
      assert
        (max >= 0L &&
         max_e =
         add max (add (mul (of_int i) v64)
                    (of_int
                       (j -
                        (match G.is_directed, loops with
                         | true, true -> 0
                         | true, false -> if j > i then i + 1 else i
                         | false, true -> i * (i - 1) / 2 + if j > i then i else j
                         | false, false -> i*(i+1)/2 + if j > i then i+1 else j)))));
      if nb = 0 then
        g
      else
        let add_edges =
          let i, j = if j = v - 1 then i + 1, 0 else i, j + 1 in
          add_edges i j
        in
        if (i = j && not loops) || (not G.is_directed && i > j) then
          add_edges max nb g
        else
          let add_edges = add_edges (pred max) in
          if Random.int64 max < of_int nb then
            add_edges (nb - 1) (add_edge g a.(i) a.(j))
          else
            add_edges nb g
    in
    add_edges 0 0 max_e e g

  let random ~loops ~v ~e =
    let r = float e /. (float v *. float v) in
    (if r < 0.4 then random_few_edges else random_many_edges) ~loops ~v ~e

  let graph ?(loops=false) ~v ~e () = random B.add_edge ~loops ~v ~e

  let labeled f ?(loops=false) ~v ~e () =
    random
      (fun g v1 v2 -> B.add_edge_e g (G.E.create v1 (f v1 v2) v2))
      ~loops ~v ~e

  (* DEBUG *)
  let random_few_edges = random_few_edges B.add_edge
  let random_many_edges = random_many_edges B.add_edge

  (** G(n,p) graphs
      See https://en.wikipedia.org/wiki/Random_graph *)


  let gnp_generic add_edge ?(loops=false) ~v ~prob () =
    if not (0.0 <= prob && prob <= 1.0) then invalid_arg "gnp";
    let vertices = Array.init v (fun i -> B.G.V.create i) in
    let g = Array.fold_left B.add_vertex (B.empty ()) vertices in
    let g = ref g in
    for i = 0 to v-1 do
      for j = 0 to (if G.is_directed then v-1 else i) do
        if (loops || j <> i) && (prob = 1.0 || Random.float 1.0 < prob) then
          g := add_edge !g vertices.(i) vertices.(j)
      done
    done;
    !g

  let gnp ?(loops=false) ~v ~prob () =
    gnp_generic B.add_edge ~loops ~v ~prob ()

  let gnp_labeled f ?(loops=false) ~v ~prob () =
    gnp_generic (fun g v1 v2 -> B.add_edge_e g (G.E.create v1 (f v1 v2) v2))
      ~loops ~v ~prob ()

end

module P (G : Sig.P with type V.label = int) = Make(Builder.P(G))

module I (G : Sig.I with type V.label = int) = Make(Builder.I(G))


(** Random planar graphs *)

module Planar = struct

  module type S = sig
    type graph
    val graph :
      ?loops:bool -> xrange:int*int -> yrange:int*int ->
      prob:float -> int -> graph
  end

  module Make
      (B : Builder.S with type G.V.label = int * int and type G.E.label = int) =
  struct

    type graph = B.G.t
    open B.G

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

    let graph ?(loops=false) ~xrange:(xmin,xmax) ~yrange:(ymin,ymax) ~prob v =
      if not (0.0 <= prob && prob <= 1.0) then invalid_arg "Planar.graph";
      if v < 2 then invalid_arg "Planar.graph";
      (* [v] random points and their Delaunay triangulation *)
      let random_point () =
        xmin + Random.int (1 + xmax - xmin),
        ymin + Random.int (1 + ymax - ymin)
      in
      let vertices = Array.init v (fun _ -> V.create (random_point ())) in
      let t = Triangulation.triangulate vertices in
      (* a graph with [v] vertices and random loops if any *)
      let g = Array.fold_left B.add_vertex (B.empty ()) vertices in
      let g =
        if loops then
          Array.fold_left
            (fun g v ->
               if Random.float 1.0 < prob then
                 g
               else
                 let e = E.create v 0 v in B.add_edge_e g e)
            g vertices
        else
          g
      in
      (* we keep some edges from the triangulation according to [prob] *)
      let add_edge v1 v2 g =
        if Random.float 1.0 < prob then
          g
        else
          let e = E.create v1 (Point.distance v1 v2) v2 in B.add_edge_e g e
      in
      Triangulation.fold
        (fun v1 v2 g ->
           let g = add_edge v1 v2 g in
           if is_directed then add_edge v2 v1 g else g)
        t g

  end

  module P (G : Sig.P with type V.label = int * int and type E.label = int) =
    Make(Builder.P(G))

  module I (G : Sig.I with type V.label = int * int and type E.label = int) =
    Make(Builder.I(G))

end
(*
  Local Variables:
  compile-command: "make -C .. src/rand.cmo"
  End:

*)
