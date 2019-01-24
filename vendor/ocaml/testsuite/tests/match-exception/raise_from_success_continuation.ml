(**
  Test raising exceptions from a value-matching branch.
*)
let test_raise_from_val_handler =
  let () = print_endline "test raise from val handler" in
  let g () = List.find ((=)2) [1;2;4] in
  let h () =
    match
      g ()
    with exception _ -> 10
    | _ -> raise Not_found
  in
  assert ((try h () with Not_found -> 20) = 20);
  print_endline "raise from val handler succeeded"
;;
