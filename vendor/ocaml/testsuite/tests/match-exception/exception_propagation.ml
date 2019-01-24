(**
   Test that match allows exceptions to propagate.
*)
let () =
  try
    match
      (let _ = raise Not_found in
       assert false)
    with
    | _ -> assert false
    | exception Invalid_argument _ -> assert false
  with
    Not_found ->
      print_endline "caught expected exception (Not_found)"
  | _ ->
    assert false
;;
