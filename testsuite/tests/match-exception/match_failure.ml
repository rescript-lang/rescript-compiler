(**
   Test that value match failure in a match block raises Match_failure.
*)
let return_some_3 () = Some (1 + 2)
;;

let test_match_partial_match =
  try 
    let _ = (match return_some_3 () with 
    | Some x when x < 3 -> "Some x"
    | exception Failure _ -> "failure"
    | exception Invalid_argument _ -> "invalid argument"
    | None -> "None"
    ) in
    assert false
  with
    Match_failure _ ->
      print_endline "match failure, as expected"
;;
