type graph = list<(string, string)>

let graph: graph = list{
  ("a", "b"),
  ("a", "c"),
  ("a", "d"),
  ("b", "e"),
  ("c", "f"),
  ("d", "e"),
  ("e", "f"),
  ("e", "g"),
}

let nexts = (x: string, g: graph): list<string> => List.fold_left((acc, (a, b)) =>
    if a == x {
      list{b, ...acc}
    } else {
      acc
    }
  , list{}, g)

let rec dfs1 = (nodes, graph, visited) =>
  switch nodes {
  | list{} => List.rev(visited)
  | list{x, ...xs} =>
    if List.mem(x, visited) {
      dfs1(xs, graph, visited)
    } else {
      print_endline(x)
      dfs1(\"@"(nexts(x, graph), xs), graph, list{x, ...visited})
    }
  }

let () = {
  assert (dfs1(list{"a"}, graph, list{}) == list{"a", "d", "e", "g", "f", "c", "b"})

  print_newline()
  assert (dfs1(list{"b"}, list{("f", "d"), ...graph}, list{}) == list{"b", "e", "g", "f", "d"})
}

let rec dfs2 = (nodes, graph, visited) => {
  let rec aux = (nodes, graph, visited) =>
    switch nodes {
    | list{} => visited
    | list{x, ...xs} =>
      if List.mem(x, visited) {
        aux(xs, graph, visited)
      } else {
        aux(xs, graph, aux(nexts(x, graph), graph, list{x, ...visited}))
      }
    }
  \"@@"(List.rev, aux(nodes, graph, visited))
}

let () = {
  let dfs1 = dfs2
  assert (dfs1(list{"a"}, graph, list{}) == list{"a", "d", "e", "g", "f", "c", "b"})
  assert (dfs1(list{"b"}, list{("f", "d"), ...graph}, list{}) == list{"b", "e", "g", "f", "d"})
}

let dfs3 = (nodes, graph) => {
  let visited = ref(list{})
  let rec aux = (node, graph) =>
    if \"@@"(not, List.mem(node, visited.contents)) {
      visited := list{node, ...visited.contents}
      List.iter(x => aux(x, graph), nexts(node, graph))
    }
  List.iter(node => aux(node, graph), nodes)
  List.rev(visited.contents)
}

let () = {
  let dfs1 = dfs3
  assert (dfs1(list{"a"}, graph) == list{"a", "d", "e", "g", "f", "c", "b"})
  assert (dfs1(list{"b"}, list{("f", "d"), ...graph}) == list{"b", "e", "g", "f", "d"})
}

/* since [x] is recorded before visiting its successors, so even with 
    cycles, it is still terminating
*/

let grwork = list{
  ("wake", "shower"),
  ("shower", "dress"),
  ("dress", "go"),
  ("wake", "eat"),
  ("eat", "washup"),
  ("washup", "go"),
}

let unsafe_topsort = graph => {
  let visited = ref(list{})
  let rec sort_nodes = nodes => List.iter(node => sort_node(node), nodes)
  and sort_node = node =>
    if \"@@"(not, List.mem(node, visited.contents)) {
      /* This check does not prevent cycle ,
           but it is still necessary? yes! 
           since a node can have multiple parents
 */

      sort_nodes(nexts(node, graph))
      /* different from dfs, recorded after its 
            successors have been searched for toplogoical
            sort, downside: can not detect cycles
 */
      visited := list{node, ...visited.contents}
    }
  List.iter(((x, _)) => sort_node(x), graph)
  visited.contents
}

let () = assert (unsafe_topsort(grwork) == list{"wake", "shower", "dress", "eat", "washup", "go"})

module String_set = Set.Make(String)
exception Cycle(list<string>)
let pathsort = graph => {
  let visited = ref(list{})
  let empty_path = (String_set.empty, list{})
  let \"+>" = (node, (set, stack)) =>
    if String_set.mem(node, set) {
      raise(Cycle(list{node, ...stack}))
    } else {
      (String_set.add(node, set), list{node, ...stack})
    }

  /* let check node (set,stack) = 
      if String_set.mem node set then 
        raise (Cycle (node::stack))  in */
  let rec sort_nodes = (path, nodes) => List.iter(node => sort_node(path, node), nodes)
  and sort_node = (path, node) =>
    if \"@@"(not, List.mem(node, visited.contents)) {
      /* check node path ; */
      sort_nodes(\"+>"(node, path), nexts(node, graph))
      /* different from dfs, recorded after its 
            successors have been searched for toplogoical
            sort, downside: can not detect cycles
 */
      visited := list{node, ...visited.contents}
    }
  List.iter(((x, _)) => sort_node(empty_path, x), graph)
  visited.contents
}

let () = assert (pathsort(grwork) == list{"wake", "shower", "dress", "eat", "washup", "go"})

let () = try {
  \"@@"(ignore, pathsort(list{("go", "eat"), ...grwork}))
  assert(false)
} catch {
| Cycle(list{"go", "washup", "eat", "go"}) => ()
| _ => assert(false)
}
