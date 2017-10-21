(**************************************************************************)
(*                                                                        *)
(*  This file is part of OcamlGraph.                                      *)
(*                                                                        *)
(*  Copyright (C) 2009-2010                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1, with a linking exception.                    *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the file ../LICENSE for more details.                             *)
(*                                                                        *)
(*  Authors:                                                              *)
(*    - Julien Signoles  (Julien.Signoles@cea.fr)                         *)
(*    - Jean-Denis Koeck (jdkoeck@gmail.com)                              *)
(*    - Benoit Bataille  (benoit.bataille@gmail.com)                      *)
(*                                                                        *)
(**************************************************************************)

open Graph

let element = function
  | [] -> invalid_arg "empty list in element"
  | l -> 
    Random.self_init ();
    List.nth l (Random.int (List.length l))

let black   = 0x000000
and white   = 0xFFFFFF
and red     = 0xFF0000
and green   = 0x00FF00
and blue    = 0x0000FF
and yellow  = 0xFFFF00
and cyan    = 0x00FFFF
and magenta = 0xFF00FF  

module Vertex = struct
  type t = int
end

module Edge = struct
  type t = int
  let compare : int -> int -> int = Pervasives.compare
  let default = 0
end

module G = Imperative.Digraph.AbstractLabeled(Vertex)(Edge)
module R = Rand.I(G)

module GraphAttrs = struct
  include G
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_name v = string_of_int (G.V.label v)
  let vertex_attributes _ =
    let shape = element [`Ellipse; `Box; `Circle; `Doublecircle; `Diamond] in
    let color = element [black; white; red; green; blue; yellow; cyan; magenta] in
    [`Shape shape; `Color color]
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Model = DGraphModel.Make(GraphAttrs)

let create () =
  (* State *)
  Random.self_init ();
  let v = 100 in
  let e = Random.int (v*2) in
  let g = R.graph ~loops:true ~v ~e () in
  Model.from_graph g
