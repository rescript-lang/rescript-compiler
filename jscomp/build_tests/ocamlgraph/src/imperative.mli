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

(** Imperative Graph Implementations. *)

open Sig

(** Signature of imperative graphs. *)
module type S = sig

  (** <b>Edges may be labeled or not</b>:
      - Unlabeled: there is no label on edges
      - Labeled: you have to provide a label implementation as a functor
      parameter.

      <b>Vertices may be concrete or abstract</b>:
      - Concrete: type of vertex labels and type of vertices are identified.
      - Abstract: type of vertices is abstract (in particular it is not equal
      to type of vertex labels

      <b>How to choose between concrete and abstract vertices for my graph
      implementation</b>?

      Usually, if you fall into one of the following cases, use abstract
      vertices:
      - you cannot provide efficient comparison/hash functions for vertices; or
      - you wish to get two different vertices with the same label.

      In other cases, it is certainly easier to use concrete vertices.  *)

  (** Imperative Unlabeled Graphs. *)
  module Concrete (V: COMPARABLE) :
    Sig.I with type V.t = V.t and type V.label = V.t and type E.t = V.t * V.t
                                                     and type E.label = unit

  (** Abstract Imperative Unlabeled Graphs. *)
  module Abstract(V: ANY_TYPE) :
    Sig.IM with type V.label = V.t and type E.label = unit

  (** Imperative Labeled Graphs. *)
  module ConcreteLabeled (V: COMPARABLE)(E: ORDERED_TYPE_DFT) :
    Sig.I with type V.t = V.t and type V.label = V.t
                              and type E.t = V.t * E.t * V.t and type E.label = E.t

  (** Abstract Imperative Labeled Graphs. *)
  module AbstractLabeled (V: ANY_TYPE)(E: ORDERED_TYPE_DFT) :
    Sig.IM with type V.label = V.t and type E.label = E.t

end

(** Imperative Directed Graphs. *)
module Digraph : sig

  include S

  (** {2 Bidirectional graphs}

      Bidirectional graphs use more memory space (at worse the double) that
      standard concrete directional graphs. But accessing predecessors is in
      O(1) amortized instead of O(max(|V|,|E|)) and removing a vertex is in
      O(D*ln(D)) instead of O(|V|*ln(D)). D is the maximal degree of the
      graph. *)

  (** Imperative Unlabeled, bidirectional graph. *)
  module ConcreteBidirectional (V: COMPARABLE) :
    Sig.I with type V.t = V.t and type V.label = V.t and type E.t = V.t * V.t
                                                     and type E.label = unit

  (** Imperative Labeled and bidirectional graph. *)
  module ConcreteBidirectionalLabeled(V:COMPARABLE)(E:ORDERED_TYPE_DFT) :
    Sig.I with type V.t = V.t and type V.label = V.t
                              and type E.t = V.t * E.t * V.t and type E.label = E.t

end

(** Imperative Undirected Graphs. *)
module Graph : S

(** Imperative graphs implemented as adjacency matrices. *)
module Matrix : sig

  module type S = sig

    (** Vertices are integers in [0..n-1].
        A vertex label is the vertex itself.
        Edges are unlabeled. *)

    include Sig.I with type V.t = int and type V.label = int
                                      and type E.t = int * int

    (** Creation. graphs are not resizeable: size is given at creation time.
        Thus [make] must be used instead of [create]. *)
    val make : int -> t

    (** Note: [add_vertex] and [remove_vertex] have no effect.
        [clear] only removes edges, not vertices. *)

  end

  module Digraph : S
  (** Imperative Directed Graphs implemented with adjacency matrices. *)

  module Graph : S
  (** Imperative Undirected Graphs implemented with adjacency matrices. *)

end

(****
   (** Faster implementations for abstract (un)labeled (di)graphs
    when vertices are _not shared_ between different graphs.
    This means that, when using the following implementations, two different
    graphs (created with two calls to [create]) must have disjoint sets of
    vertices. *)
   module UV : sig

   (** directed graphs *)
   module Digraph : sig

    module Abstract(V: ANY_TYPE) :
      Sig.IM with type V.label = V.t and type E.label = unit

    module AbstractLabeled (V: ANY_TYPE)(E: ORDERED_TYPE_DFT) :
      Sig.IM with type V.label = V.t and type E.label = E.t

   end

   (** undirected graphs *)
   module Graph : sig

    module Abstract(V: ANY_TYPE) :
      Sig.IM with type V.label = V.t and type E.label = unit

    module AbstractLabeled (V: ANY_TYPE)(E: ORDERED_TYPE_DFT) :
      Sig.IM with type V.label = V.t and type E.label = E.t

   end

   end
 ****)

(*
Local Variables:
compile-command: "make -C .."
End:
*)
