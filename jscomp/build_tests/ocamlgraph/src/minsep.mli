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

(**
   Minimal separators of a graph

   Based on the article:
   Generating all the minimal separators of a graph.
   by A. Berry, J.-P. Bordat and O.Cogis
   http://www.isima.fr/berry/generating.html

   A set [S] of vertices is a minimal separator if it exists 2 distinct
   connected components [C] and [D] in [G \ S] such that each vertex of [S] has
   a successor in [C] and [D]. *)

(** Minimal signature for computing the minimal separators *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  val succ: t -> V.t -> V.t list
  val iter_succ: (V.t -> unit) -> t -> V.t -> unit
  val fold_succ: (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val iter_vertex: (V.t -> unit) -> t -> unit
  val fold_vertex: (V.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module type MINSEP = sig
  module G : G
  (** Implementation of a graph *)
  module Vertex_Set : Set.S with type elt = G.V.t
  (** Implementation of a set of vertex *)
  module VSetset : Set.S with type elt = Vertex_Set.t
  (** Implementation of a set of [Vertex_Set] *)

  val allminsep : G.t -> Vertex_Set.t list
  (** [allminsep g] computes the list of all minimal separators of g. *)
  val list_of_allminsep : G.t -> G.V.t list list
  (** Less efficient that [allminsep] *)
  val set_of_allminsep : G.t -> VSetset.t
  (** Less efficient that [allminsep] *)
end

(** Implementation for a persistent graph *)
module P(G : sig include G val remove_vertex : t -> V.t -> t end) :
  MINSEP with module G = G

(** Implementation for an imperative graph.
    Less efficient that the implementation for a persistent graph *)
module I(G : sig
    include G
    module Mark : Sig.MARK with type graph = t and type vertex = V.t
  end) :
  MINSEP with module G = G

