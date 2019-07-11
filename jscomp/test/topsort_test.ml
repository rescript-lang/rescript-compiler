type graph = (string * string) list

let graph : graph =
  [ ("a", "b"); ("a", "c"); ("a", "d"); ("b", "e"); ("c", "f"); ("d", "e")
  ; ("e", "f"); ("e", "g") ]

let nexts (x : string) (g : graph) : string list =
  List.fold_left (fun acc (a, b) -> if a = x then b :: acc else acc) [] g

let rec dfs1 nodes graph visited =
  match nodes with
  | [] -> List.rev visited
  | x :: xs ->
      if List.mem x visited then dfs1 xs graph visited
      else (
        print_endline x ;
        dfs1 (nexts x graph @ xs) graph (x :: visited) )

let () =
  assert (dfs1 ["a"] graph [] = ["a"; "d"; "e"; "g"; "f"; "c"; "b"]) ;
  print_newline () ;
  assert (dfs1 ["b"] (("f", "d") :: graph) [] = ["b"; "e"; "g"; "f"; "d"])

let rec dfs2 nodes graph visited =
  let rec aux nodes graph visited =
    match nodes with
    | [] -> visited
    | x :: xs ->
        if List.mem x visited then aux xs graph visited
        else aux xs graph (aux (nexts x graph) graph (x :: visited)) in
  List.rev @@ aux nodes graph visited

let () =
  let dfs1 = dfs2 in
  assert (dfs1 ["a"] graph [] = ["a"; "d"; "e"; "g"; "f"; "c"; "b"]) ;
  assert (dfs1 ["b"] (("f", "d") :: graph) [] = ["b"; "e"; "g"; "f"; "d"])

let dfs3 nodes graph =
  let visited = ref [] in
  let rec aux node graph =
    if not @@ List.mem node !visited then (
      visited := node :: !visited ;
      List.iter (fun x -> aux x graph) (nexts node graph) ) in
  List.iter (fun node -> aux node graph) nodes ;
  List.rev !visited

let () =
  let dfs1 = dfs3 in
  assert (dfs1 ["a"] graph = ["a"; "d"; "e"; "g"; "f"; "c"; "b"]) ;
  assert (dfs1 ["b"] (("f", "d") :: graph) = ["b"; "e"; "g"; "f"; "d"])

(** since [x] is recorded before visiting its successors, so even with cycles,
    it is still terminating *)

let grwork =
  [ ("wake", "shower"); ("shower", "dress"); ("dress", "go"); ("wake", "eat")
  ; ("eat", "washup"); ("washup", "go") ]

let unsafe_topsort graph =
  let visited = ref [] in
  let rec sort_nodes nodes = List.iter (fun node -> sort_node node) nodes
  and sort_node node =
    if
      not @@ List.mem node !visited
      (* This check does not prevent cycle , but it is still necessary? yes!
         since a node can have multiple parents *)
    then (
      sort_nodes (nexts node graph) ;
      (* different from dfs, recorded after its successors have been searched
         for toplogoical sort, downside: can not detect cycles *)
      visited := node :: !visited ) in
  List.iter (fun (x, _) -> sort_node x) graph ;
  !visited

let () =
  assert (
    unsafe_topsort grwork = ["wake"; "shower"; "dress"; "eat"; "washup"; "go"]
  )

module String_set = Set.Make (String)

exception Cycle of string list

let pathsort graph =
  let visited = ref [] in
  let empty_path = (String_set.empty, []) in
  let ( +> ) node (set, stack) =
    if String_set.mem node set then raise (Cycle (node :: stack))
    else (String_set.add node set, node :: stack) in
  (*let check node (set,stack) = if String_set.mem node set then raise (Cycle
    (node::stack)) in *)
  let rec sort_nodes path nodes =
    List.iter (fun node -> sort_node path node) nodes
  and sort_node path node =
    if not @@ List.mem node !visited then (
      (* check node path ; *)
      sort_nodes (node +> path) (nexts node graph) ;
      (* different from dfs, recorded after its successors have been searched
         for toplogoical sort, downside: can not detect cycles *)
      visited := node :: !visited ) in
  List.iter (fun (x, _) -> sort_node empty_path x) graph ;
  !visited

let () =
  assert (pathsort grwork = ["wake"; "shower"; "dress"; "eat"; "washup"; "go"])

let () =
  try
    ignore @@ pathsort (("go", "eat") :: grwork) ;
    assert false
  with
  | Cycle ["go"; "washup"; "eat"; "go"] -> ()
  | _ -> assert false
