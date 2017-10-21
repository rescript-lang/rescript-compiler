type file = { name : string ; mutable deps : file list }

module G = struct
  
  type t = file list

  module V = struct 
    type t = file 
    let compare n1 n2 = compare n1.name n2.name
    let hash n = Hashtbl.hash n.name 
    let equal n1 n2 = n1.name = n2.name 
  end

(*  module E = struct
    type t = file * file
    type label = unit
    let label _ = ()
    let dst (_,n) = n
  end
*)

  let iter_vertex = List.iter
  let fold_vertex = List.fold_right
  let iter_succ f _ v = List.iter f v.deps
(*  let iter_succ_e f _ v = List.iter (fun w -> f (v,w)) v.deps*)
  let fold_succ f _ v = List.fold_right f v.deps
end

module Dfs = Graph.Traverse.Dfs(G)
(*
module W = struct
  type label = unit
  type t = int
  let weight _ = 1
  let zero = 0
  let add = (+)
  let compare = compare 
end

module Dij = Graph.Path.Dijkstra(G)(W)

let n = 10000
let e = 1000000
let depart = 0
let arrivee = 765

let files = Array.init n (fun i -> {name=string_of_int i;deps=[]} )
let g = Array.to_list files
let _ = 
  for i=1 to e do 
    let n1 = files.(Random.int n) in
    let n2 = files.(Random.int n) in
    n1.deps <- n2::n1.deps
  done

open Unix
  
let utime f x =                                                   
  let u = (times()).tms_utime in                                  
  let y = f x in
  let ut = (times()).tms_utime -. u in
  (y,ut)

let print_utime f x = 
  let (y,ut) = utime f x in
  Format.printf "user time: %2.2f@." ut;
  y

(*let () = print_utime (Dfs.prefix (fun _ -> ())) g*)
let _ = print_utime (Dij.shortest_path g files.(depart)) files.(arrivee)

module G' = Graph.Pack.Digraph
let g' = G'.create ()
let files' = Array.map 
  (fun n -> let v = G'.V.create (int_of_string n.name) in G'.add_vertex g' v; v) files
let () = 
  List.iter 
    (fun n1 ->
       List.iter (fun n2 -> G'.add_edge g' files'.(int_of_string n1.name) files'.(int_of_string n2.name)) 
	 n1.deps)
    g

module Dfs' = Graph.Traverse.Dfs(G')

module W' = struct
  type label = G'.E.label
  type t = int
  let weight _ = 1
  let zero = 0
  let add = (+)
  let compare = compare 
end
module Dij' = Graph.Path.Dijkstra(G')(W')

(*let () = print_utime (Dfs'.prefix (fun _ -> ())) g'*)
let _ = print_utime (Dij'.shortest_path g' files'.(depart)) files'.(arrivee)

module G'' = Graph.Persistent.Digraph.Abstract(struct type t = int end)
let g'' = G''.empty
let files'' = Array.map (fun n -> G''.V.create (int_of_string n.name)) files
let g'' = Array.fold_right (fun v g'' -> G''.add_vertex g'' v) files'' g''
let g'' = 
  List.fold_right 
    (fun n1 g'' ->
       List.fold_right 
	 (fun n2 g'' -> G''.add_edge g'' files''.(int_of_string n1.name) files''.(int_of_string n2.name)) 
	 n1.deps g'')
    g g''

module Dfs'' = Graph.Traverse.Dfs(G'')

module W'' = struct

  type label = G''.E.label
  type t = int
  let weight _ = 1
  let zero = 0
  let add = (+)
  let compare = compare 
end
module Dij'' = Graph.Path.Dijkstra(G'')(W'')

(*let () = print_utime (Dfs''.prefix (fun _ -> ())) g''*)
let _ = print_utime (Dij''.shortest_path g'' files''.(depart)) files''.(arrivee)



*)
