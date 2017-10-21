(******************************************************************************)
(*                                                                            *)
(*  Copyright (C) 2012 Pietro Abate <pietro.abate@pps.jussieu.fr>             *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(******************************************************************************)

(** Generic GraphMl Printer *)

(** Graph information required by Graphml *)
module type G = sig

  type t
  type vertex
  module E : sig
    type t
    val src: t -> vertex
    val dst : t -> vertex
  end
  val is_directed : bool
  val iter_vertex : (vertex -> unit) -> t -> unit
  val iter_edges_e : (E.t -> unit) -> t -> unit

end

(** Graphml Printer given a graph and required info *)
module Print
    (G: G)
    (L : sig
       val vertex_properties : (string * string * string option) list
       (** List of the type of the vertex proprierties.
           The format is (id,type,default). *)

       val edge_properties : (string * string * string option) list
       (** List of the type of the edge proprierties. *)

       val map_vertex : G.vertex -> (string * string) list
       (** Associates to each vertex a key/value list where
           the key is the id of a vertex attribute and the value is the value
           associated to this vertex *)

       val map_edge : G.E.t -> (string * string) list
       (** Associates to each edge a key/value list *)

       val vertex_uid : G.vertex -> int
       (** @return a unique integer identifier for the vertex *)

       val edge_uid : G.E.t -> int
       (** @return a unique integer identifier for the edge *)
     end) :
sig
  val print : Format.formatter -> G.t -> unit
  (** [print fmt graph] print the GraphMl representation of the given graph
      on the given formatter *)
end


(*
Local Variables:
compile-command: "make -C .."
End:
*)
