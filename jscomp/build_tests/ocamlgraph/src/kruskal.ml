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

(* $Id: kruskal.ml,v 1.5 2005-06-30 10:48:55 filliatr Exp $ *)

module type UNIONFIND = sig
  type elt
  type t
  val init : elt list -> t
  val find : elt -> t -> elt
  val union : elt -> elt -> t -> unit
end

module type G = sig
  type t
  module V : Sig.COMPARABLE
  module E : sig
    type t
    type label
    val label : t -> label
    val dst : t -> V.t
    val src : t -> V.t
  end
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_edges_e : (E.t -> unit) -> t ->  unit
end

module Generic
    (G: G)
    (W : Sig.ORDERED_TYPE with type t = G.E.label)
    (UF: UNIONFIND with type elt = G.V.t) =
struct

  let spanningtree g =
    let vertices = G.fold_vertex (fun v a -> v :: a) g [] in
    let uf = UF.init vertices in
    let edges =
      let l = ref [] in
      G.iter_edges_e (fun e -> l := e :: !l) g;
      List.sort (fun e e'-> W.compare (G.E.label e) (G.E.label e')) !l
    in
    let s = ref [] in
    let cover e =
      let u, v = G.E.src e, G.E.dst e in
      if G.V.compare (UF.find u uf) (UF.find v uf) <> 0 then begin
        UF.union u v uf;
        s := e :: !s
      end
    in
    List.iter cover edges;
    !s

end

module Make(G: G)(W : Sig.ORDERED_TYPE with type t=G.E.label) =
  Generic(G)(W)(Unionfind.Make(G.V))

