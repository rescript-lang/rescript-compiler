(* Test file for Brom-Kerbosch *)

open Graph

module G = Persistent.Graph.Concrete (struct 
  type t = int 
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end)

module BK = Clique.Bron_Kerbosch(G)

let () = 
  let vertices = [1;2;3;4;5;6;7] in
  let edges = [(1,2);(1,5);(2,5);(2,3);(4,5);(3,4);(4,6)] in
  let g = List.fold_left (fun graph v -> G.add_vertex graph v) G.empty vertices in
  let g = List.fold_left (fun graph (v1, v2) -> G.add_edge graph v1 v2) g edges in
  let cliques = BK.maximalcliques g in
  (* The cliques of this graph should be: [2, 3], [3, 4], [1, 2, 5], [4, 5], [4, 6], [7] *)
  assert (List.length cliques == 6);
  assert (List.exists (fun cl -> List.length cl == 2 && List.mem 2 cl && List.mem 3 cl) cliques);
  assert (List.exists (fun cl -> List.length cl == 2 && List.mem 3 cl && List.mem 4 cl) cliques);
  assert (List.exists (fun cl -> List.length cl == 3 && List.mem 1 cl && List.mem 2 cl && List.mem 5 cl) cliques);
  assert (List.exists (fun cl -> List.length cl == 2 && List.mem 4 cl && List.mem 5 cl) cliques);
  assert (List.exists (fun cl -> List.length cl == 2 && List.mem 4 cl && List.mem 6 cl) cliques);
  assert (List.exists (fun cl -> List.length cl == 1 && List.mem 7 cl) cliques);
;;

