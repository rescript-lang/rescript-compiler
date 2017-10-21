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

(* $Id: gml.mli,v 1.3 2005-07-06 13:20:31 conchon Exp $ *)

(** Parser and pretty-printer for GML file format. *)

type value =
  | Int of int
  | Float of float
  | String of string
  | List of value_list

and value_list = (string * value) list

(** {2 Parser} *)

(** Provide a parser for GML file format. *)
module Parse
    (B : Builder.S)
    (L : sig
       val node : value_list -> B.G.V.label
       (** How to build the node label out of the set of GML attributes.
           For example {v node [ id 12 label "foo" ] v}  will call this
           function with [["id", Int 12; "label", String "foo"]] *)
       val edge : value_list -> B.G.E.label
       (** How to build the edge label out of the set of GML attributes *)
     end) :
sig
  val parse : string -> B.G.t
end

(** {2 Pretty-printer} *)

(** Signature for graph required by {!Print}.
    Sub-signature of {!Sig.G}. *)
module type G = sig
  module V : sig
    type t
    val hash : t -> int
    val equal : t -> t -> bool
    type label
    val label : t -> label
  end
  module E : sig
    type t
    type label
    val src : t -> V.t
    val dst : t -> V.t
    val label : t -> label
  end
  type t
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges_e : (E.t -> unit) -> t -> unit
end

(** Provide a pretty-printer for GML file format. *)
module Print
    (G: G)
    (L: sig
       val node : G.V.label -> value_list
       val edge : G.E.label -> value_list
     end) :
sig
  val print : Format.formatter -> G.t -> unit
end


