(* test evaluation order

   'y' is translated into a constant, and is therefore considered
   non-recursive. With the current letrec compilation method,
   it should be evaluated before x and z.
*)
type tree = Tree of tree list

let test =
  let rec x = (print_endline "effect"; Tree [y; z])
  and y = (print_endline "effect"; Tree [])
  and z = (print_endline "effect"; Tree [x])
  in
  match (x, y, z) with
    | (Tree [y1; z1], Tree[], Tree[x1]) ->
      assert (y1 == y);
      assert (z1 == z);
      assert (x1 == x)
    | _ ->
      assert false
