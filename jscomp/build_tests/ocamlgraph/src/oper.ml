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

(* Basic operations over graphs *)

module type S = sig
  type g
  val transitive_closure : ?reflexive:bool -> g -> g
  val add_transitive_closure : ?reflexive:bool -> g -> g
  val transitive_reduction : ?reflexive:bool -> g -> g
  val replace_by_transitive_reduction : ?reflexive:bool -> g -> g
  val mirror : g -> g
  val complement : g -> g
  val intersect : g -> g -> g
  val union : g -> g -> g
end

module Make(B : Builder.S) = struct

  open B

  (* Roy-Warshall's algorithm *)

  type g = G.t

  let add_transitive_closure ?(reflexive=false) g0 =
    let phi v g =
      let g = if reflexive then B.add_edge g v v else g in
      G.fold_succ
        (fun sv g -> G.fold_pred (fun pv g -> B.add_edge g pv sv) g v g)
        g v g
    in
    G.fold_vertex phi g0 g0

  let transitive_closure ?(reflexive=false) g0 =
    add_transitive_closure ~reflexive (B.copy g0)

  module H = Hashtbl.Make(G.V)

  let mirror g =
    if G.is_directed then begin
      let g' =
        G.fold_vertex
          (fun v g' -> B.add_vertex g' v)
          g (B.empty ())
      in
      G.fold_edges_e
        (fun e g' ->
           let v1 = G.E.src e in
           let v2 = G.E.dst e in
           B.add_edge_e g' (G.E.create v2 (G.E.label e) v1))
        g g'
    end else
      g

  let complement g =
    G.fold_vertex
      (fun v g' ->
         G.fold_vertex
           (fun w g' ->
              if G.mem_edge g v w then g'
              else B.add_edge g' v w)
           g g')
      g (B.empty ())

  let intersect g1 g2 =
    G.fold_vertex
      (fun v g ->
         try
           let succ = G.succ_e g2 v in
           G.fold_succ_e
             (fun e g ->
                if List.exists (fun e' -> G.E.compare e e' = 0) succ
                then B.add_edge_e g e
                else g)
             g1 v (B.add_vertex g v)
         with Invalid_argument _ ->
           (* [v] not in [g2] *)
           g)
      g1 (B.empty ())

  let union g1 g2 =
    let add g1 g2 =
      (* add the graph [g1] in [g2] *)
      G.fold_vertex
        (fun v g ->
           G.fold_succ_e (fun e g -> B.add_edge_e g e) g1 v (B.add_vertex g v))
        g1 g2
    in
    add g1 (B.copy g2)

  let replace_by_transitive_reduction ?(reflexive=false) g0 =
    let phi v g =
      let g = if reflexive then B.remove_edge g v v else g in
      G.fold_succ
        (fun sv g ->
           G.fold_pred
             (fun pv g ->
                if G.V.equal pv v || G.V.equal sv v then g
                else B.remove_edge g pv sv)
             g v g)
        g v g
    in
    G.fold_vertex phi g0 g0

  let transitive_reduction ?(reflexive=false) g0 =
    replace_by_transitive_reduction ~reflexive (B.copy g0)

end

module P(G : Sig.P) = Make(Builder.P(G))
module I(G : Sig.I) = Make(Builder.I(G))

module Choose(G : sig
    type t
    type vertex
    type edge
    val iter_vertex : (vertex -> unit) -> t -> unit
    val iter_edges_e : (edge -> unit) -> t -> unit
  end) =
struct

  exception Found_Vertex of G.vertex
  let choose_vertex g =
    try
      G.iter_vertex (fun v -> raise (Found_Vertex v)) g;
      invalid_arg "choose_vertex"
    with Found_Vertex v ->
      v

  exception Found_Edge of G.edge
  let choose_edge g =
    try
      G.iter_edges_e (fun v -> raise (Found_Edge v)) g;
      invalid_arg "choose_vertex"
    with Found_Edge v ->
      v

end

module Neighbourhood(G : sig
    type t
    module V : Sig.COMPARABLE
    val fold_succ: (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
    val succ: t -> V.t -> V.t list
  end) =
struct

  module Vertex_Set = Set.Make(G.V)

  let set_from_vertex g v =
    G.fold_succ
      (fun v' s -> if G.V.equal v v' then s else Vertex_Set.add v' s)
      g v Vertex_Set.empty

  let list_from_vertex g v =
    let rec aux = function
      | [] -> []
      | v' :: l ->
        if G.V.equal v v' then begin
          assert (not (List.exists (G.V.equal v) l));
          l
        end else
          v' :: aux l
    in
    aux (G.succ g v)

  let set_from_vertices g l =
    let fold_left f = List.fold_left f Vertex_Set.empty l in
    let env_init = fold_left (fun s v -> Vertex_Set.add v s) in
    let add x s =
      if Vertex_Set.mem x env_init then s else Vertex_Set.add x s
    in
    fold_left (fun s v -> G.fold_succ add g v s)

  let list_from_vertices g l = Vertex_Set.elements (set_from_vertices g l)

end
