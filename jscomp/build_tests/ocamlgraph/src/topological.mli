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

(** Topological order.

    This functor provides functions which allow iterating over a graph in
    topological order. Cycles in graphs are allowed. Specification is the
    following:
      if vertex [x] is visited before vertex [y]
      then either there is a path from [x] to [y],
           or there is no path from [y] to [x].
    In the particular case of a DAG, this simplifies to:
      if there is an edge from [x] to [y], then [x] is visited before [y]. *)

(** Minimal graph signature to provide.
    Sub-signature of {!Sig.G}. *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
end

(** Functor providing topological iterators over a graph. *)
module Make(G: G) : sig

  val fold : (G.V.t -> 'a -> 'a) -> G.t -> 'a -> 'a
  (** [fold action g seed] allows iterating over the graph [g]
      in topological order. [action node accu] is called repeatedly,
      where [node] is the node being visited, and [accu] is the result of
      the [action]'s previous invocation, if any, and [seed] otherwise.
      If [g] contains cycles, the order is unspecified inside the cycles and
      every node in the cycles will be presented exactly once.

      Not tail-recursive. Complexity: O(V+E) *)

  val iter : (G.V.t -> unit) -> G.t -> unit
  (** [iter action] calls [action node] repeatedly. Nodes are (again)
      presented to [action] in topological order.
      The order is the same as for [fold]. *)

end

(** Provide the same features than {!Make}, except that the resulting
    topological ordering is stable according to vertices comparison: if two
    vertices [v1] and [v2] are topologically equal, [v1] is presented first to
    the iterator if and only if [G.V.compare v1 v2 <= 0]. In particular, the
    resulting order is not dependent on the provided hash function. This
    property is not guaranteed by the functor {!Make}. The counterpart is a less
    efficient implementation: worst time complexity is O(E*V*ln(V)) instead of
    O(E*V) (with E = number of edges and V = number of vertices. *)
module Make_stable(G: sig include G  val in_degree : t -> V.t -> int end): sig
  val fold : (G.V.t -> 'a -> 'a) -> G.t -> 'a -> 'a
  val iter : (G.V.t -> unit) -> G.t -> unit
end

(*
Local Variables:
compile-command: "make -C .."
End:
*)
