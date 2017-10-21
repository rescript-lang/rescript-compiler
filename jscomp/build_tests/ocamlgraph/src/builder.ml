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

module P(G : Sig.P) = struct
  module G = G
  let empty () = G.empty
  let copy g = g
  let add_vertex = G.add_vertex
  let add_edge = G.add_edge
  let add_edge_e = G.add_edge_e
  let remove_vertex = G.remove_vertex
  let remove_edge = G.remove_edge
  let remove_edge_e = G.remove_edge_e
end

module I(G : Sig.I) = struct
  module G = G
  let empty () = G.create ~size:97 ()
  let copy = G.copy
  let add_vertex g v = G.add_vertex g v; g
  let add_edge g v1 v2 = G.add_edge g v1 v2; g
  let add_edge_e g e = G.add_edge_e g e; g
  let remove_vertex g v = G.remove_vertex g v; g
  let remove_edge g v1 v2 = G.remove_edge g v1 v2; g
  let remove_edge_e g e = G.remove_edge_e g e; g
end

(*
Local Variables:
compile-command: "make -C .."
End:
*)
