
module type G = sig
  type t
  module V : Sig.HASHABLE
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
end

module Dfs(G : G) = struct

  module H = Hashtbl.Make(G.V)

  let dfs f g = 
    let h = H.create 65537 in
    let stack = Stack.create () in
    (* invariant: [h] contains exactly the vertices which have been pushed *)
    let push v = 
      if not (H.mem h v) then begin H.add h v (); Stack.push v stack end
    in
    let loop () =
      while not (Stack.is_empty stack) do
	let v = Stack.pop stack in
	f v;
	G.iter_succ push g v
      done
    in
    G.iter_vertex (fun v -> push v; loop ()) g

end

