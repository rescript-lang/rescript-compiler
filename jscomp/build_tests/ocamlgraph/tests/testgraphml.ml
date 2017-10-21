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

module V = struct
  type t = int
  let compare = compare
  let hash i = i
  let equal = (=)
end

module G = Graph.Imperative.Digraph.ConcreteBidirectional(V)

module Gr = struct
  include G
  let vertex_properties = ["id1","string",None; "id2","string",Some "2"]
  let edge_properties = ["ed", "string",Some "3"]
  let map_edge e = ["ed", string_of_int (E.dst e)]
  let map_vertex v = [ "id1", string_of_int v ; "id2", string_of_int v]
  let vertex_uid = G.V.hash
  let edge_uid e =
    Hashtbl.hash (vertex_uid (G.E.src e), G.E.label e, vertex_uid (G.E.dst e))
end

module GraphPrinter = Graph.Graphml.Print(G)(Gr)

let print g = GraphPrinter.print Format.std_formatter g

let () =
  let g = G.create () in
  G.add_vertex g 1;
  G.add_vertex g 2;
  G.add_vertex g 3;
  G.add_edge g 1 2;
  G.add_edge g 1 3;
  print g;;
