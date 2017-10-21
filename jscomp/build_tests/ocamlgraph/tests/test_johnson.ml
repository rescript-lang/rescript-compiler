
(* Test file for Johnson *)

open Printf
open Graph

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
  end


module G = Imperative.Digraph.ConcreteLabeled(Int)(Int)


module W = struct
  type edge = G.E.t
  type t = int
  let weight e = G.E.label e
  let zero = 0
  let add = (+)
  let sub = (-)
  let compare = compare
  end

module J = Path.Johnson(G)(W)

let g = G.create ()

let () =
  G.add_edge_e g (G.E.create 1 3  2);
  G.add_edge_e g (G.E.create 1 (-4)  5);
  G.add_edge_e g (G.E.create 1 8  3);
  G.add_edge_e g (G.E.create 2 7  5);
  G.add_edge_e g (G.E.create  2 1  4);
  G.add_edge_e g (G.E.create  3 4  2);
  G.add_edge_e g (G.E.create  4 (-5)  3);
  G.add_edge_e g (G.E.create  4 2  1);
  G.add_edge_e g (G.E.create  5 6  4)

let () = let test = J.all_pairs_shortest_paths g in
	 J.HVV.iter (fun (v, u) d -> Printf.printf "[%d -> %d : %d]\n" v u d) test
