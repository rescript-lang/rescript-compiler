(**
    The success continuation expression is in tail position.
*)

let count_to_tr_match n =
  let rec loop i =
    match
      i < n
    with exception Not_found -> ()
    | false -> ()
    | true -> loop (i + 1)
  in loop 0
;;

let test_tail_recursion =
  try
    count_to_tr_match 10000000;
    print_endline "handler-case (match) is tail recursive"
  with _ ->
    assert false
;;
