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

module type G = sig
  type t
  module V : Sig.COMPARABLE
  module E : sig
    type t
    type label
    val label : t -> label
    val dst : t -> V.t
    val src : t -> V.t
    val compare : t -> t -> int
  end
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges_e : (E.t -> unit) -> t ->  unit
  val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
end

(** Functor providing an implementation of Prim's minimum-spanning-tree
    algorithm.
    Parameter [W] ensures that label on edges are comparable. *)
module Make(G: G)(W: Sig.WEIGHT with type edge = G.E.t) : sig
  val spanningtree : G.t -> G.E.t list
  val spanningtree_from : G.t -> G.V.t -> G.E.t list
end

