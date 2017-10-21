let v = int_of_string Sys.argv.(1)
let e = int_of_string Sys.argv.(2)

module Int = struct
  type t = int
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash
end

open Graph

module G = Imperative.Digraph.Abstract(Int)
module R = Rand.I(G)

module Display = struct
  include G
  let vertex_name v = string_of_int (V.label v)
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Gv = Graphviz.Dot(Display)

let () = Gv.output_graph stdout (R.graph ~v ~e ())
