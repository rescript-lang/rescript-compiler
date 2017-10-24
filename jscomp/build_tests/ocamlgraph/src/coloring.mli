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

(** [k]-coloring of undirected graphs.

    A [k]-coloring of a graph [g] is a mapping [c] from nodes to [\{1,...,k\}]
    such that [c(u) <> c(v)] for any edge [u-v] in [g]. *)

(** {2 Graph coloring for graphs without marks} *)

(** Minimal graph signature for {!Make}.
    Sub-signature of {!Sig.G}. *)
module type G = sig
  val is_directed : bool
  type t
  val nb_vertex : t -> int
  module V : Sig.COMPARABLE
  val out_degree : t -> V.t -> int
  val iter_vertex : (V.t -> unit) -> t -> unit
  val fold_vertex : (V.t -> 'a -> 'a) -> t  -> 'a -> 'a
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

(** Provide a function for [k]-coloring a graph. *)
module Make(G: G) : sig

  module H : Hashtbl.S with type key = G.V.t
  (** Hash tables used to store the coloring *)

  val coloring : G.t -> int -> int H.t
  (** [coloring g k] colors the graph [g] with [k] colors and returns the
      coloring as a hash table mapping nodes to their colors. *)

end

(** {2 Graph coloring for graph with integer marks} *)

(** Minimal graph signature for {!Mark}.
    Sub-signature of {!Sig.IM}. *)
module type GM = sig
  val is_directed : bool
  type t
  val nb_vertex : t -> int
  module V : Sig.COMPARABLE
  val out_degree : t -> V.t -> int
  val iter_vertex : (V.t -> unit) -> t -> unit
  val fold_vertex : (V.t -> 'a -> 'a) -> t  -> 'a -> 'a
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  module Mark : sig
    val get : V.t -> int
    val set : V.t -> int -> unit
  end
end

(** Provide a function for [k]-coloring a graph with integer marks. 
    The provided function is more efficient that the one provided by functor
    {!Make} above. *)
module Mark(G : GM) : sig

  exception NoColoring

  val coloring : G.t -> int -> unit
  (** [coloring g k] colors the nodes of graph [g] using k colors,
      assigning the marks integer values between 1 and k.
      raises [NoColoring] when there is no possible coloring.

      The graph marks may be partially set before starting; the meaning of
      initial values is as follows:
      - 0: a node to be colored
      - any value between 1 and k: a color already assigned
      - any value greater than k: a node to be ignored 

      @raise NoColoring if [g] cannot be [k]-colored. *)

end
