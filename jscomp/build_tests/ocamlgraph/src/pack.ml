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

(* $Id: pack.ml,v 1.13 2006-05-12 14:07:16 filliatr Exp $ *)

module Generic(G : Sig.IM with type V.label = int and type E.label = int) =
struct

  include G

  exception Found of V.t
  let find_vertex g i =
    try
      iter_vertex (fun v -> if V.label v = i then raise (Found v)) g;
      raise Not_found
    with Found v ->
      v

  module Builder = Builder.I(G)

  module Dfs = Traverse.Dfs(G)
  module Bfs = Traverse.Bfs(G)
  module Marking = Traverse.Mark(G)

  module Classic = Classic.I(G)

  module Rand = Rand.I(G)

  module Components = Components.Make(G)

  module W = struct
    type edge = G.E.t
    type t = int
    let weight e = G.E.label e
    let zero = 0
    let add = (+)
    let sub = (-)
    let compare : t -> t -> int = Pervasives.compare
  end

  include Path.Dijkstra(G)(W)
  include Path.Johnson(G)(W)

  module BF = Path.BellmanFord(G)(W)
  let bellman_ford = BF.find_negative_cycle_from

  module F = struct
    type label = int
    type t = int
    let max_capacity x = x
    let min_capacity _ = 0
    let flow _ = 0
    let add = (+)
    let sub = (-)
    let compare : t -> t -> int = Pervasives.compare
    let zero = 0
  end

  module FF = Flow.Ford_Fulkerson(G)(F)
  let ford_fulkerson g =
    if not G.is_directed then
      invalid_arg "ford_fulkerson: not a directed graph";
    FF.maxflow g

  module Goldberg = Flow.Goldberg(G)(F)
  let goldberg g =
    if not G.is_directed then invalid_arg "goldberg: not a directed graph";
    Goldberg.maxflow g

  include Oper.Make(Builder)

  module PathCheck = Path.Check(G)

  module Topological = struct
    include Topological.Make(G)
    module S = Topological.Make_stable(G)
    let fold_stable = S.fold
    let iter_stable = S.iter
  end

  module Int = struct
    type t = int
    let compare : t -> t -> int = Pervasives.compare
  end

  include Kruskal.Make(G)(Int)

  module Display = struct
    include G
    let vertex_name v = string_of_int (V.label v)
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes _ = []
    let default_edge_attributes _ = []
    let edge_attributes e = [ `Label (string_of_int (E.label e) ) ]
    let get_subgraph _ = None
  end
  module Dot_ = Graphviz.Dot(Display)
  module Neato = Graphviz.Neato(Display)

  let dot_output g f =
    let oc = open_out f in
    if is_directed then Dot_.output_graph oc g else Neato.output_graph oc g;
    close_out oc

  let display_with_gv g =
    let tmp = Filename.temp_file "graph" ".dot" in
    dot_output g tmp;
    ignore (Sys.command ("dot -Tps " ^ tmp ^ " | gv -"));
    Sys.remove tmp

  module GmlParser =
    Gml.Parse
      (Builder)
      (struct
        let node l =
          try match List.assoc "id" l with Gml.Int n -> n | _ -> -1
          with Not_found -> -1
        let edge _ =
          0
      end)

  let parse_gml_file = GmlParser.parse

  module DotParser =
    Dot.Parse
      (Builder)
      (struct
        let nodes = Hashtbl.create 97
        let new_node = ref 0
        let node (id,_) _ =
          try
            Hashtbl.find nodes id
          with Not_found ->
            incr new_node;
            Hashtbl.add nodes id !new_node;
            !new_node
        let edge _ =
          0
      end)

  let parse_dot_file = DotParser.parse

  open Format

  module GmlPrinter =
    Gml.Print
      (G)
      (struct
        let node n = ["label", Gml.Int n]
        let edge n = ["label", Gml.Int n]
      end)

  let print_gml = GmlPrinter.print
  let print_gml_file g f =
    let c = open_out f in
    let fmt = formatter_of_out_channel c in
    fprintf fmt "%a@." GmlPrinter.print g;
    close_out c

(*
  module GraphmlPrinter =
    Graphml.Print
      (G)
      (struct
   let node n = ["label", Gml.Int n]
   let edge n = ["label", Gml.Int n]
         module Vhash = Hashtbl.Make(G.V)
         let vertex_uid = uid (Vhash.create 17) Vhash.find Vhash.add
         module Ehash = Hashtbl.Make(G.E)
         let edge_uid = uid (Ehash.create 17) Ehash.find Ehash.add
       end)

  let print_gml = GmlPrinter.print
  let print_gml_file g f =
    let c = open_out f in
    let fmt = formatter_of_out_channel c in
    fprintf fmt "%a@." GmlPrinter.print g;
    close_out c
*)

end

module I = struct
  type t = int
  let compare : t -> t -> int = Pervasives.compare
  let default = 0
end

module Digraph = Generic(Imperative.Digraph.AbstractLabeled(I)(I))

module Graph = Generic(Imperative.Graph.AbstractLabeled(I)(I))

