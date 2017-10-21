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

(** Parser for DOT file format. *)

open Dot_ast

val parse_dot_ast : string -> Dot_ast.file

type clusters_hash = (string, attr list) Hashtbl.t

(** Provide a parser for DOT file format. *)
module Parse
    (B : Builder.S)
    (L : sig
       val node : node_id -> attr list -> B.G.V.label
       (** How to build the node label out of the set of attributes *)
       val edge : attr list -> B.G.E.label
       (** How to build the edge label out of the set of attributes *)
     end) :
sig

  (** Parses a dot file *)
  val parse : string -> B.G.t

  (** Parses a dot file and returns the graph, its bounding box and
      a hash table from clusters to dot attributes *)
  val parse_bounding_box_and_clusters :
    string -> B.G.t * string * clusters_hash

end
