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

(** Graph builders in order to persistent/imperative graphs sharing a same
    signature. *)

(** {2 Common interface for graph builders}.

    Note: the following functions always return graphs but this is meaningless
    for imperative implementations (the graph is modified in-place).
    This is just to provide a common interface. *)

module type S = sig
  module G : Sig.G
  val empty : unit -> G.t
  val copy : G.t -> G.t
  val add_vertex : G.t -> G.V.t -> G.t
  val add_edge : G.t -> G.V.t -> G.V.t -> G.t
  val add_edge_e : G.t -> G.E.t -> G.t
  val remove_vertex : G.t -> G.V.t -> G.t
  val remove_edge : G.t -> G.V.t -> G.V.t -> G.t
  val remove_edge_e : G.t -> G.E.t -> G.t
end

module type INT = S with type G.V.label = int

(** {1 Builders for the various graph implementations} *)

module P(G : Sig.P) : S with module G = G
(** Persistent Graphs Builders. *)

module I(G : Sig.I) : S with module G = G
(** Imperative Graphs Builders. *)

(*
Local Variables:
compile-command: "make -C .."
End:
*)
