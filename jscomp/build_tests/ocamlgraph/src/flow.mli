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

(** Algorithms on flows

    The following flow algorithms only apply to networks, that are
    directed graphs together with a source (a 0 in-degree vertex) and a
    terminal (a 0 out-degree vertex). *)

(** {1 Maximum flow algorithms} *)

(** Signature for edges' flow. *)
module type FLOW = sig

  type t
  (** Type of edges. *)

  type label
  (** Type of labels on edges. *)

  (** Maximum and minimum capacities for a label on an edge. *)

  val max_capacity : label -> t
  val min_capacity : label -> t

  (** Current flow for a label on an edge. *)

  val flow : label -> t

  (** [+] and [-] on flows. *)

  val add : t -> t -> t
  val sub : t -> t -> t

  (** Neutral element for [add] and [sub]. *)

  val zero : t

  (** A total ordering over flows. *)

  val compare : t -> t -> int

end

(**  {2 Goldberg maximal flow algorithm} *)

(** Minimal graph signature for Goldberg.
    Sub-signature of {!Sig.G}. *)
module type G_GOLDBERG = sig
  type t
  module V : Sig.COMPARABLE
  module E : Sig.EDGE with type vertex = V.t
  val nb_vertex : t -> int
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges_e : (E.t -> unit) -> t -> unit
  val fold_succ_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val fold_pred_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

module Goldberg(G: G_GOLDBERG)(F: FLOW with type label = G.E.label) : sig

  val maxflow : G.t -> G.V.t -> G.V.t -> (G.E.t -> F.t) * F.t
  (** [maxflow g v1 v2] searchs the maximal flow from source [v1] to
      terminal [v2] using the Goldberg algorithm. It returns the new
      flows on each edges and the growth of the flow. *)

end

(**  {2 Ford-Fulkerson maximal flow algorithm} *)

(** Minimal digraph signature for Ford-Fulkerson.
    Sub-signature of {!Sig.G}. *)
module type G_FORD_FULKERSON = sig
  type t
  module V : Sig.HASHABLE
  module E : sig
    type t
    type label
    val src : t -> V.t
    val dst : t -> V.t
    val label : t -> label
  end
  val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
  val iter_pred_e : (E.t -> unit) -> t -> V.t -> unit
end

module Ford_Fulkerson
    (G: G_FORD_FULKERSON)
    (F: FLOW with type label = G.E.label) :
sig

  val maxflow : G.t -> G.V.t -> G.V.t -> (G.E.t -> F.t) * F.t
  (** [maxflow g v1 v2] searchs the maximal flow from source [v1]
      to terminal [v2] using the Ford-Fulkerson algorithm. It
      returns the new flows on each edges and the growth of the
      flow. *)

end
