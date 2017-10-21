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

(* $Id: classic.mli,v 1.12 2005-02-25 13:54:33 signoles Exp $ *)

(** Some classic graphs *)

module type S = sig

  type graph

  val divisors : int -> graph
  (** [divisors n] builds the graph of divisors.
      Vertices are integers from [2] to [n]. [i] is connected to [j] if
      and only if [i] divides [j].
      @raise Invalid_argument is [n < 2]. *)

  val de_bruijn : int -> graph
  (** [de_bruijn n] builds the de Bruijn graph of order [n].
      Vertices are bit sequences of length [n] (encoded as their
      interpretation as binary integers). The sequence [xw] is connected
      to the sequence [wy] for any bits [x] and [y] and any bit sequence
      [w] of length [n-1].
      @raise Invalid_argument is [n < 1] or [n > Sys.word_size-1]. *)

  val vertex_only : int -> graph
  (** [vertex_only n] builds a graph with [n] vertices and no edge. *)

  val full : ?self:bool -> int -> graph
  (** [full n] builds a graph with [n] vertices and all possible edges.
      The optional argument [self] indicates if loop edges should be added
      (default value is [true]). *)

end

module P (G : Sig.P with type V.label = int) : S with type graph = G.t
(** Classic Persistent Graphs *)

module I (G : Sig.I with type V.label = int) : S with type graph = G.t
(** Classic Imperative Graphs *)
