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

(** {2 Mapping of vertices} *)

module type V_SRC = sig
  type t
  module V : Sig.HASHABLE
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module type V_DST = sig
  type t
  type vertex
  val empty : unit -> t
  val add_vertex : t -> vertex -> t
end

module Vertex(G_Src : V_SRC)(G_Dst : V_DST ) = struct

  module H = Hashtbl.Make(G_Src.V)
  let vertices = H.create 97

  let convert_vertex f x =
    try
      H.find vertices x
    with Not_found ->
      let x' = f x in
      H.add vertices x x';
      x'

  let map f g =
    H.clear vertices;
    G_Src.fold_vertex
      (fun x g -> G_Dst.add_vertex g (convert_vertex f x))
      g (G_Dst.empty ())

  let filter_map f g =
    G_Src.fold_vertex
      (fun x g -> match f x with
         | Some e -> G_Dst.add_vertex g e
         | None -> g
      ) g (G_Dst.empty ())
end

(** {2 Mapping of edges} *)

module type E_SRC = sig
  type t
  module E : Sig.ORDERED_TYPE
  val fold_edges_e : (E.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module type E_DST = sig
  type t
  type edge
  val empty : unit -> t
  val add_edge_e : t -> edge -> t
end

module Edge(G_Src: E_SRC)(G_Dst: E_DST) = struct
  module M = Map.Make(G_Src.E)
  let edges = ref M.empty

  let convert_edge f x =
    try
      M.find x !edges
    with Not_found ->
      let x' = f x in
      edges := M.add x x' !edges;
      x'

  let map f g =
    edges := M.empty;
    G_Src.fold_edges_e
      (fun x g -> G_Dst.add_edge_e g (convert_edge f x))
      g (G_Dst.empty ())

  let filter_map f g =
    G_Src.fold_edges_e
      (fun x g -> match f x with
         | Some e -> G_Dst.add_edge_e g e
         | None -> g
      ) g (G_Dst.empty ())
end

(*
  Vertex
  (struct include G_Src module V = E let fold_vertex = fold_edges_e end)
  (struct include G_Dst type vertex = edge let add_vertex = add_edge_e end)
*)
