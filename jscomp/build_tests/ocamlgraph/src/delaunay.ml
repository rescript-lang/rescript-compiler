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

(* $Id: delaunay.ml,v 1.12 2005-11-02 13:43:35 filliatr Exp $ *)

(** Code follows Don Knuth's algorithm
    from ``Axioms and hulls'' (LNCS 606, Springer-Verlag, 1992), pp. 73-77.

    Some code and comments are taken from the Stanford Graph Base,
    file [gb_plane].
*)

module type CCC = sig
  type point
  val ccw : point -> point -> point -> bool
  val in_circle : point -> point -> point -> point -> bool
end

module type Triangulation = sig
  module S : CCC
  type triangulation
  val triangulate : S.point array -> triangulation
  val iter : (S.point -> S.point -> unit) -> triangulation -> unit
  val fold : (S.point -> S.point -> 'a -> 'a) -> triangulation -> 'a -> 'a
  val iter_triangles :
    (S.point -> S.point -> S.point -> unit) -> triangulation -> unit
end

module Make (S : CCC) = struct

  module S = S

  type point = Point of int | Infinity

  (* Each edge of the current triangulation is represented by two arcs
     pointing in opposite directions; the two arcs are called mates. Each
     arc conceptually has a triangle on its left and a mate on its right. *)

  type arc = {
    mutable vert : point;
    (* v, if this arc goes from u to v *)
    mutable next : arc;
    (* the arc from v that shares a triangle with this one *)
    mutable inst : node ref;
    (* instruction to change when the triangle is modified *)
    mate : int
  }
  and node =
    | Branch of int * int * node ref * node ref
    | Terminal of arc

  type triangulation = {
    points : S.point array;
    arcs : arc array;
    last_used_arc : int
  }

  let rec dummy_arc =
    { vert = Infinity; next = dummy_arc;
      inst = ref (Terminal dummy_arc); mate = -1 }

  let make_arc n i =
    { vert = Infinity; next = dummy_arc;
      inst = ref (Terminal dummy_arc); mate = 6 * n - 7 - i }

  let finite = function Point p -> p | Infinity -> assert false

  (* [flip] will be used in both steps T4 and T5 *)
  let flip c d e t'' p n n' =
    let e' = e.next in
    let c' = c.next in
    let c'' = c'.next in
    e.next <- c;
    c.next <- c'';
    c''.next <- e;
    c''.inst <- n; c.inst <- n; e.inst <- n;
    c.vert <- Point p;
    d.next <- e';
    e'.next <- c';
    c'.next <- d;
    c'.inst <- n'; e'.inst <- n'; d.inst <- n';
    d.vert <- Point t''

  let triangulate points =
    let ccw p q r = S.ccw points.(p) points.(q) points.(r) in
    let in_circle p q r s =
      S.in_circle points.(p) points.(q) points.(r) points.(s)
    in
    let n = Array.length points in
    if n < 2 then invalid_arg "triangulate";
    let arcs = Array.init (6 * n - 6) (make_arc n) in
    let mate i = 6 * n - 7 - i in

    (*i DEBUG
      let rec dump d l =
      eprintf "%s" (String.make (2*d) ' ');
      match !l with
        | Terminal a ->
      eprintf "T %d\n" (mate a.mate)
        | Branch (u, v, l, r) ->
      eprintf "N %d %d\n" u v;
      dump (d+1) l;
      dump (d+1) r
      in
      i*)

    (* initialization:
       create a trivial triangulation for the first 2 vertices *)
    let u = 0 in
    let v = 1 in
    let a1 = arcs.(0) in
    let a2 = arcs.(1) in
    let a3 = arcs.(2) in
    let b1 = arcs.(mate 0) in
    let b2 = arcs.(mate 1) in
    let b3 = arcs.(mate 2) in
    let l1 = ref (Terminal a2) in
    let l2 = ref (Terminal b3) in
    a1.vert <- Point v;  a1.next <- a2; a1.inst <- l1;
    a2.vert <- Infinity; a2.next <- a3; a2.inst <- l1;
    a3.vert <- Point u;  a3.next <- a1; a3.inst <- l1;
    b1.vert <- Point u;  b1.next <- b3; b1.inst <- l2;
    b2.vert <- Point v;  b2.next <- b1; b2.inst <- l2;
    b3.vert <- Infinity; b3.next <- b2; b3.inst <- l2;
    let l0 = ref (Branch (u, v, l1, l2)) in
    let j = ref 2 in (* last used arc *)

    (* then for each new vertex [p] *)
    for p = 2 to n - 1 do
      (* Step T1 *)
      let rec step_T1 l p = match !l with
        | Terminal al ->
          l, al
        | Branch (pl, ql, al, bl) ->
          step_T1 (if ccw pl ql p then al else bl) p
      in
      let l, al = step_T1 l0 p in

      (* Step T2 *)
      let a = al in
      let b = a.next in
      let c = b.next in
      let q = a.vert in
      let r = b.vert in
      let s = c.vert in
      j := !j + 3;
      let aj = arcs.(!j) in
      let aj_1 = arcs.(!j - 1) in
      let aj_2 = arcs.(!j - 2) in
      let bj = arcs.(aj.mate) in
      let bj_1 = arcs.(aj_1.mate) in
      let bj_2 = arcs.(aj_2.mate) in
      let l' = ref (Terminal a) in
      let l'' = ref (Terminal aj) in
      let l''' = ref (Terminal c) in
      aj.vert   <- q;         aj.next <- b;      aj.inst <- l'';
      aj_1.vert <- r;       aj_1.next <- c;    aj_1.inst <- l''';
      aj_2.vert <- s;       aj_2.next <- a;    aj_2.inst <- l';
      bj.vert   <- Point p;   bj.next <- aj_2;   bj.inst <- l';
      bj_1.vert <- Point p; bj_1.next <- aj;   bj_1.inst <- l'';
      bj_2.vert <- Point p; bj_2.next <- aj_1; bj_2.inst <- l''';
      a.next <- bj;   a.inst <- l';
      b.next <- bj_1; b.inst <- l'';
      c.next <- bj_2; c.inst <- l''';
      let r = finite r in
      let s = finite s in

      (* steps T3 or T4 depending on [q] *)
      let r = match q with
        | Point q -> (* Step T3 *)
          let n = ref (Branch (q, p, l', l'')) in
          let n' = ref (Branch (s, p, l''', l')) in
          l := Branch (r, p, n, n');
          r
        | Infinity -> (* Step T4 *)
          let n = ref (Branch (s, p, l''', l')) in
          l := Branch (r, p, l'', n);
          let rec loop m a d s t =
            if t <> r && ccw p s t then begin
              let n = ref (Terminal d) in
              match !m with
              | Branch (mu, mv, ml, is_l') ->
                assert (is_l' == l');
                m := Branch (mu, mv, ml, d.inst);
                d.inst := Branch (t, p, n, l');
                let m = d.inst in
                flip a arcs.(a.mate) d t p n l';
                let a = arcs.(a.mate).next in
                let d = arcs.(a.mate).next in
                let s = t in
                let t = finite d.vert in
                l' := Terminal a;
                loop m a d s t
              | Terminal _ ->
                assert false
            end else begin
              (* at exit of while loop *)
              let n = ref (Terminal d.next) in
              d.inst := Branch (s, p, n, l');
              d.inst <- n; d.next.inst <- n; d.next.next.inst <- n;
              s
            end
          in
          let d = arcs.(a.mate).next in
          loop n a d s (finite d.vert)
      in

      (* Step T5 *)
      let rec loop c =
        let d = arcs.(c.mate) in
        let e = d.next in
        let t = finite d.vert in
        let t' = finite c.vert in
        let t'' = e.vert in
        if t'' <> Infinity && in_circle (finite t'') t' t p then begin
          let t'' = finite t'' in
          let n = ref (Terminal e) in
          let n' = ref (Terminal d) in
          c.inst := Branch (t'', p, n, n');
          d.inst := Branch (t'', p, n, n');
          flip c d e t'' p n n';
          loop e
        end else if t' <> r then
          loop arcs.(c.next.mate).next
        else
          () (* break *)
      in
      loop c

    done;
    { points = points; arcs = arcs; last_used_arc = !j }

  let iter f t =
    let points = t.points in
    let n = Array.length t.arcs in
    for i = 0 to t.last_used_arc do
      match t.arcs.(i).vert, t.arcs.(n - 1 - i).vert with
      | Point u, Point v -> f points.(u) points.(v)
      | _ -> ()
    done

  let iter_triangles f t =
    let n = Array.length t.arcs in
    let seen_arc = Array.make n false in
    let mate i = n - 1 - i in
    let index a = mate a.mate in
    for i = 0 to n-1 do
      if not seen_arc.(i) then begin
        let a1 = t.arcs.(i) in
        let a2 = a1.next in
        let a3 = a2.next in
        seen_arc.(i) <- true;
        seen_arc.(index a2) <- true;
        seen_arc.(index a3) <- true;
        match a1.vert, a2.vert, a3.vert with
        | Point i1, Point i2, Point i3 ->
          f t.points.(i1) t.points.(i2) t.points.(i3)
        | _ ->
          ()
      end
    done

  let fold f t a =
    let points = t.points in
    let n = Array.length t.arcs in
    let rec loop i a =
      if i <= t.last_used_arc then
        match t.arcs.(i).vert, t.arcs.(n - 1 - i).vert with
        | Point u, Point v -> loop (succ i) (f points.(u) points.(v) a)
        | _ -> loop (succ i) a
      else
        a
    in
    loop 0 a

end

(** Points with floating point coordinates *)

module FloatPoints = struct

  type point = float * float

  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( * ) = ( *. )

  let det = function
    | [| [| a00; a01 |];
         [| a10; a11 |] |] ->
      a00 * a11 - a01 * a10
    | [| [| a00; a01; a02 |];
         [| a10; a11; a12 |];
         [| a20; a21; a22 |] |] ->
      a00*a11*a22 - a00*a12*a21 - a10*a01*a22 +
      a10*a02*a21 + a20*a01*a12 - a20*a02*a11
    | [| [| a00; a01; a02; a03 |];
         [| a10; a11; a12; a13 |];
         [| a20; a21; a22; a23 |];
         [| a30; a31; a32; a33 |] |] ->
      a00*a11*a22*a33 - a00*a11*a23*a32 - a00*a21*a12*a33 +
      a00*a21*a13*a32 + a00*a31*a12*a23 - a00*a31*a13*a22 -
      a10*a01*a22*a33 + a10*a01*a23*a32 + a10*a21*a02*a33 -
      a10*a21*a03*a32 - a10*a31*a02*a23 + a10*a31*a03*a22 +
      a20*a01*a12*a33 - a20*a01*a13*a32 - a20*a11*a02*a33 +
      a20*a11*a03*a32 + a20*a31*a02*a13 - a20*a31*a03*a12 -
      a30*a01*a12*a23 + a30*a01*a13*a22 + a30*a11*a02*a23 -
      a30*a11*a03*a22 - a30*a21*a02*a13 + a30*a21*a03*a12
    | _ -> assert false

  let ccw (xu,yu) (xv,yv) (xw,yw) =
    det [| [| xu; yu; 1.0 |];
           [| xv; yv; 1.0 |];
           [| xw; yw; 1.0 |] |] > 0.0

  (*i DEBUG
    let ccw (xu,yu) (xv,yv) (xw,yw) =
    eprintf "ccw((%.0f,%.0f),(%.0f,%.0f),(%.0f,%.0f)) -> "
      xu yu xv yv xw yw;
    let r = ccw (xu,yu) (xv,yv) (xw,yw) in
    eprintf "%b\n" r; flush stderr;
    r
    i*)

  let in_circle (xt,yt) (xu,yu) (xv,yv) (xw,yw) =
    det [| [| xt; yt; (xt * xt + yt * yt); 1.0 |];
           [| xu; yu; (xu * xu + yu * yu); 1.0 |];
           [| xv; yv; (xv * xv + yv * yv); 1.0 |];
           [| xw; yw; (xw * xw + yw * yw); 1.0 |]; |] > 0.0

  (*i DEBUG
    let in_circle (xt,yt) (xu,yu) (xv,yv) (xw,yw) =
    eprintf "in_circle((%.0f,%.0f),(%.0f,%.0f),(%.0f,%.0f),(%.0f,%.0f)) -> "
      xt yt xu yu xv yv xw yw;
    let r = in_circle (xt,yt) (xu,yu) (xv,yv) (xw,yw) in
    eprintf "%b\n" r; flush stderr;
    r
    i*)

end

module Float = Make(FloatPoints)

(** Points with integer coordinates.
    We approximate using module [FloatPoints] but this could be made exact
    following Knuth's code in Axioms and Hulls *)

module IntPoints = struct

  type point = int * int

  let ccw (xu,yu) (xv,yv) (xw,yw) =
    FloatPoints.ccw
      (float xu, float yu) (float xv, float yv) (float xw, float yw)

  let in_circle (xt,yt) (xu,yu) (xv,yv) (xw,yw) =
    FloatPoints.in_circle
      (float xt, float yt)
      (float xu, float yu) (float xv, float yv) (float xw, float yw)

end

module Int = Make(IntPoints)

