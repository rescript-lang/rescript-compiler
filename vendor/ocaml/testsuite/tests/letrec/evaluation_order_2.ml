(* A variant of evaluation_order_1.ml where the side-effects
   are inside the blocks.
   Effect are not named to allow different evaluation orders (flambda
   and clambda differ on this point).
*)
type tree = Tree of tree list

let test =
  let rec x = (Tree [(print_endline "effect"; y); z])
  and y = Tree (print_endline "effect"; [])
  and z = Tree (print_endline "effect"; [x])
  in
  match (x, y, z) with
    | (Tree [y1; z1], Tree[], Tree[x1]) ->
      assert (y1 == y);
      assert (z1 == z);
      assert (x1 == x)
    | _ ->
      assert false
