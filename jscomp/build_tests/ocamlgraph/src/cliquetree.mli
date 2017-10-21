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

(** Construction of the clique tree of a graph and recognition
    of chordal graphs.

    Based on the article:
    Chordal graphs and their clique graph
    by P. Galinier, M. Habib and C. Paul.

    @author Matthieu Sozeau *)

module CliqueTree(G : Sig.G) : sig

  (** Original graph vertex *)
  module CliqueV :
  sig
    type t
    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool
    val label : t -> t
    val create : G.V.t -> t
    val vertex : t -> G.V.t
  end

  (** Set of original vertices *)
  module CVS : Set.S with type elt = CliqueV.t

  (** Clique tree vertex type *)
  module CliqueTreeV : sig
    (** Trace of the algorithm as a list of markers Clique vertices *)
    type data = CliqueV.t list * CVS.t
    type label
    type t

    val compare : t -> t -> int
    val hash : t -> int
    val equal : t -> t -> bool

    val create : data -> label -> t
    val label : t -> label
    val data : t -> data
  end

  module CliqueTreeE : sig
    type t = int * CVS.t
    val compare : t -> t -> int
    val default : t
    val create : int -> CVS.t -> t

    (** Vertices in the clique tree edge
        (intersection of the two clique extremities). *)
    val vertices : t -> CVS.t
  end

  (** The clique tree graph type *)
  module CliqueTree : Sig.G with type V.t = CliqueTreeV.t
                             and type E.label = CliqueTreeE.t

  (** [mcs_clique g] return an perfect elimination order of [g]
      (if it is chordal), the clique tree of [g] and its root.  *)
  val mcs_clique : G.t -> G.V.t list * CliqueTree.t * CliqueTree.V.t

  (** [is_chordal g] uses the clique tree construction to test if a graph is
      chordal or not. *)
  val is_chordal : G.t -> bool

  (** [maxwidth g tri tree] returns the maxwidth characteristic of the
      triangulation [tri] of graph [g] given the clique tree [tree] of [tri]. *)
  val maxwidth : G.t -> G.t -> CliqueTree.t -> int
end
