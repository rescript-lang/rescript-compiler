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

(** Minimum Degree algorithm

    Based on the article:
    The Minimum Degree Heuristic and the Minimal Triangulation Process
    by A. Berry, Pinar Heggernes & Geneviève Simonet.

    @author Matthieu Sozeau
    @author Pierre-Loic Garoche *)

module P(G : Sig.P) : sig

  type edgeset = (G.V.t * G.V.t) list

  val md : G.t -> G.t * edgeset * G.V.t list
  (** [md g] return a tuple [(g', e, o)] where [g'] is
      a triangulated graph, [e] is the triangulation of [g] and
      [o] is a perfect elimination order of [g'] *)

  val triangulate : G.t -> G.t
  (** [triangulate g] return the graph [g'] produced by applying
      miminum degree to [g]. *)

end

module I(G : Sig.I) : sig

  type edgeset = (G.V.t * G.V.t) list

  val md : G.t -> G.t * edgeset * G.V.t list
  (** [md g] return a tuple [(g', e, o)] where [g'] is
      a triangulated graph, [e] is the triangulation of [g] and
      [o] is a perfect elimination order of [g'] *)

  val triangulate : G.t -> G.t
  (** [triangulate g] return the graph [g'] produced by applying
      miminum degree to [g]. *)

end
