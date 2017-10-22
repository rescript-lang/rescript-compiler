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

(** Graph traversal. *)

(** {2 Dfs and Bfs} *)

(** Minimal graph signature for {!Dfs} and {!Bfs}.
    Sub-signature of {!Sig.G}. *)
module type G = sig
  val is_directed : bool
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  (** It is enough to iter over all the roots (vertices without predecessor) of
      the graph, even if iterating over the other vertices is correct. *)

  val fold_vertex : (V.t -> 'a -> 'a) -> t  -> 'a -> 'a
  (** It is enough to fold over all the roots (vertices without predecessor) of
      the graph, even if folding over the other vertices is correct. *)

  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
end

(** Depth-first search *)
module Dfs(G : G) : sig

  (** {2 Classical big-step iterators} *)

  val iter : ?pre:(G.V.t -> unit) ->
    ?post:(G.V.t -> unit) -> G.t -> unit
  (** [iter pre post g] visits all nodes of [g] in depth-first search,
      applying [pre] to each visited node before its successors,
      and [post] after them. Each node is visited exactly once.
      Not tail-recursive. *)

  val prefix : (G.V.t -> unit) -> G.t -> unit
  (** applies only a prefix function; note that this function is more
      efficient than [iter] and is tail-recursive. *)

  val postfix : (G.V.t -> unit) -> G.t -> unit
  (** applies only a postfix function. Not tail-recursive. *)

  (** Same thing, but for a single connected component
      (only [prefix_component] is tail-recursive) *)

  val iter_component : ?pre:(G.V.t -> unit) ->
    ?post:(G.V.t -> unit) -> G.t -> G.V.t -> unit
  val prefix_component : (G.V.t -> unit) -> G.t -> G.V.t -> unit
  val postfix_component : (G.V.t -> unit) -> G.t -> G.V.t -> unit

  (** {2 Step-by-step iterator}

      This is a variant of the iterators above where you can move on
      step by step. The abstract type [iterator] represents the current
      state of the iteration. The [step] function returns the next state.
      In each state, function [get] returns the currently visited vertex.
      On the final state both [get] and [step] raises exception [Exit].

      Note: the iterator type is persistent (i.e. is not modified by the
      [step] function) and thus can be used in backtracking algorithms. *)

  type iterator
  val start : G.t -> iterator
  val step : iterator -> iterator
  val get : iterator -> G.V.t

  (** {2 Cycle detection} *)

  val has_cycle : G.t -> bool
  (** [has_cycle g] checks for a cycle in [g]. Linear in time and space. *)

end

(** Breadth-first search *)
module Bfs(G : G) : sig

  (** {2 Classical big-step iterators} *)

  val iter : (G.V.t -> unit) -> G.t -> unit
  val iter_component : (G.V.t -> unit) -> G.t -> G.V.t -> unit

  (** {2 Classical folds} *)
  val fold : (G.V.t -> 'a -> 'a) -> 'a -> G.t -> 'a
  val fold_component : (G.V.t -> 'a -> 'a) -> 'a -> G.t -> G.V.t -> 'a

  (** {2 Step-by-step iterator}
      See module [Dfs] *)

  type iterator
  val start : G.t -> iterator
  val step : iterator -> iterator
  val get : iterator -> G.V.t

end

(** {2 Traversal with marking}

    Provide a more efficient version of depth-first algorithm when graph
    vertices are marked. *)

(** Minimal graph signature for graph traversal with marking.
    Sub-signature of {!Sig.IM}. *)
module type GM = sig
  type t
  module V : sig type t end
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  module Mark : sig
    val clear : t -> unit
    val get : V.t -> int
    val set : V.t -> int -> unit
  end
end

(** Graph traversal with marking.
    Only applies to imperative graphs with marks. *)
module Mark(G : GM) : sig

  val dfs : G.t -> unit
  (** [dfs g] traverses [g] in depth-first search, marking all nodes. *)

  val has_cycle : G.t -> bool
  (** [has_cycle g] checks for a cycle in [g]. Modifies the marks.
      Linear time, constant space. *)

end

