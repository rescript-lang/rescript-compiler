(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2013-2014                                               *)
(*  David Monniaux, Gabriel Radanne                                       *)
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

(** Minimal cutset of a graph

    Compute a minimal vertex cutset of a reducible oriented graph.
    The set [S] of vertices is a cutset of [G] if [G \ S] doesn't
    contain any cycle.

    Based on the article: A linear time algorithm for finding minimum
    cutsets in reducible graphs by A. Shamir (1979).
*)

(** Minimal signature for computing the minimal separators *)
module type G = sig
  type t
  module V : Sig.VERTEX
  val succ : t -> V.t -> V.t list
end

module Make (G : G) : sig

  (** Find a minimal cutset.
      @raise Invalid_argument if the graph is not reducible.
  *)
  val min_cutset : G.t -> G.V.t -> G.V.t list

end
