(**************************************************************************)
(*                     *)
(* Ocamlgraph: a generic graph library for OCaml         *)
(* Copyright (C) 2014-2015               *)
(* Giselle Reis                 *)
(*                     *)
(* This software is free software; you can redistribute it and/or     *)
(* modify it under the terms of the GNU Library General Public       *)
(* License version 2.1, with the special exception on linking       *)
(* described in file LICENSE.               *)
(*                     *)
(* This software is distributed in the hope that it will be useful,     *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       *)
(*                     *)
(**************************************************************************)

module type G = sig
  type t
  module V : Sig.COMPARABLE
  val succ : t -> V.t -> V.t list
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module Bron_Kerbosch(G : G) = struct

  let rec bron_kerbosch cliquelst graph clique candidates used = match (candidates, used) with
    | ([], []) -> clique :: cliquelst
    | ([], _) -> cliquelst
    | (c, u) ->
      let (_, _, cliques) = List.fold_left ( fun (c, u, acc) v ->
          (* Get the neighbors ignoring self-loops *)
          let n = List.filter (fun nb -> not (G.V.equal nb v)) (G.succ graph v) in
          let c' = List.filter (fun cv -> List.exists (fun v -> G.V.equal v cv) n) c in
          let u' = List.filter (fun cv -> List.exists (fun v -> G.V.equal v cv) n) u in
          let c_minus_v = List.filter (fun cv -> not (G.V.equal cv v)) c in

          ( c_minus_v, (v :: u), bron_kerbosch acc graph (v :: clique) c' u')
        ) (c, u, []) c in
      cliques @ cliquelst

  let maximalcliques g =
    let vertices = G.fold_vertex (fun v acc -> v :: acc) g [] in
    bron_kerbosch [] g [] vertices []

end


