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

(** Graph cliques *)

(** {1 Clique algorithms} *)

(** {2 Bron-Kerbosch Algorithm}

    This algorithm will find and return all maximal cliques in an undirected graph. *)

(** Minimal graph signature for Bron-Kerbosch.
    Sub-signature of {!Sig.G}. *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  val succ : t -> V.t -> V.t list
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module Bron_Kerbosch(G : G) : sig
  val maximalcliques : G.t -> G.V.t list list
  (** [maximalcliques g] computes all the maximal cliques of [g] using the
      Bron-Kerbosch algorithm. It returns the sets of vertices belonging to the
      same maximal clique. *)
end
