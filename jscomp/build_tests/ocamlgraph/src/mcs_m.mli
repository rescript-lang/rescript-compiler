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

(* $Id: mcs_m.mli,v 1.2 2004-10-19 15:21:44 signoles Exp $ *)

(** Maximal Cardinality Search (MCS-M) algorithm

    Based on the article:
    Maximal Cardinality Search for Computing Minimal Triangulations of Graphs.
    by A. Berry, Jean R. S. Blair, Pinar Heggernes & Barry W. Peyton.

    @author Matthieu Sozeau
    @author Pierre-Loic Garoche *)

module MaximalCardinalitySearch : sig
  module P(G : Sig.P) : sig
    type edgelist = (G.V.t * G.V.t) list

    (** [mcsm g] returns a tuple [(o, e)] where [o] is a perfect elimination
        order of [g'] where [g'] is the triangulation [e] applied to [g]. *)
    val mcsm : G.t -> (int * G.V.t) list * edgelist

    (** [triangulate g] computes a triangulation of [g]
        using the MCS-M algorithm *)
    val triangulate : G.t -> G.t
  end
  module I(Gr : Sig.I) : sig
    type edgelist = (Gr.V.t * Gr.V.t) list

    (** [mcsm g] return a tuple [(o, e)] where o is a perfect elimination order
        of [g'] where [g'] is the triangulation [e] applied to [g]. *)
    val mcsm : Gr.t -> (int * Gr.V.t) list * edgelist

    (** [triangulate g] triangulates [g] using the MCS-M algorithm *)
    val triangulate : Gr.t -> unit
  end
end
