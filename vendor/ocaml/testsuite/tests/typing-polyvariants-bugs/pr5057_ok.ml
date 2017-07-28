(* PR5057 *)

module TT = struct
  module IntSet = Set.Make(struct type t = int let compare = compare end)
end

let () =
  let f flag =
    let module T = TT in
    let _ = match flag with `A -> 0 | `B r -> r in
    let _ = match flag with `A -> T.IntSet.mem | `B r -> r in
    ()
  in
  f `A
