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

(* Copyright (c) 2010 - 2011 Technische Universitaet Muenchen
 * Markus W. Weissmann <markus.weissmann@in.tum.de>
 * Florian Pichlmeier <florian.pichlmeier@in.tum.de>
 * All rights reserved. *)

(** The leader list algorithm; it generates a list of basic blocks from
    a directed graph. A basic block is a forward path of nodes that requires
    neither branching from nor into.
*)

(** Minimal graph signature for leader list algorithm *)
module type G = sig
  type t
  module V : Sig.COMPARABLE
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val succ : t -> V.t -> V.t list
  val pred : t -> V.t -> V.t list
end

module Make
    (G : G) :
sig
  val leader_lists : G.t -> G.V.t -> G.V.t list list
  (** [leader_lists graph root] computes the leader lists or basic blocks
      of the given graph. The node [root] is always a leader of a basic block. *)
end
