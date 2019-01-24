(* Effect are not named to allow different evaluation orders (flambda
   and clambda differ on this point).
 *)
let test =
  let rec x = print_endline "effect"; [| 1; 2; 3 |]
      and y = print_endline "effect"; [| 1.; 2.; 3. |]
  in
  assert (x = [| 1; 2; 3 |]);
  assert (y = [| 1.; 2.; 3. |]);
  ()
