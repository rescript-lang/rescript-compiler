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

let g = R.graph ~v ~e ()

(* module D = Traverse.Dfs(G) *)

(* let () = D.prefix (fun _ -> ()) g *)
