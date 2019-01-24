let test =
  let rec x = [| y; y |] and y = 1. in
  assert (x = [| 1.; 1. |]);
  assert (y = 1.);
  ()
;;
