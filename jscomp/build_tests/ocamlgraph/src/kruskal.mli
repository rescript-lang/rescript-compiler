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

(* $Id: kruskal.mli,v 1.5 2005-06-30 10:48:55 filliatr Exp $ *)

(** Kruskal's minimum-spanning-tree algorithm. *)

(** Minimal graph signature for Kruskal.
    Sub-signature of {!Sig.G}. *)
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

(** Functor providing an implementation of Kruskal's minimum-spanning-tree
    algorithm.
    Parameter [W] ensures that label on edges are comparable. *)
module Make(G: G)(W: Sig.ORDERED_TYPE with type t = G.E.label) : sig
  val spanningtree : G.t -> G.E.t list
end

(** {2 Generic version where union-find implementation is provided} *)

(** Signature of union-find. *)
module type UNIONFIND = sig
  type elt
  type t
  val init : elt list -> t
  val find : elt -> t -> elt
  val union : elt -> elt -> t -> unit
end

(** Functor providing an implementation of Kruskal's minimum-spanning-tree
    algorithm using a user-defined union-find algorithm.
    Parameter [W] ensures that label on edges are comparable. *)
module Generic
    (G: G)
    (W: Sig.ORDERED_TYPE with type t = G.E.label)
    (UF: UNIONFIND with type elt = G.V.t) :
sig
  val spanningtree : G.t -> G.E.t list
end
