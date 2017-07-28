let edit_distance = Misc.edit_distance

let show_cutoff n =
  if n = max_int then "max_int" else Printf.sprintf "%d" n
;;

let test =
  let counter = ref 0 in
  fun a b cutoff expected ->
    let show_result = function
      | None -> "None"
      | Some d -> "Some " ^ string_of_int d in
    incr counter;
    Printf.printf "[%02d] (edit_distance %S %S %s), expected %s\n"
      !counter a b (show_cutoff cutoff) (show_result expected);
    let result = edit_distance a b cutoff in
    if result = expected
    then print_endline "OK"
    else Printf.printf "FAIL: got %s\n%!" (show_result result)

let () =
  test "a" "a" 1 (Some 0);
  test "a" "a" 0 (Some 0);
  test "a" "b" 1 (Some 1);
  test "a" "b" 0 None;
  test "add" "adad" 3 (Some 1);
  test "delete" "delte" 3 (Some 1);
  test "subst" "sabst" 3 (Some 1);
  test "swap" "sawp" 3 (Some 1);
  test "abbb" "bbba" 3 (Some 2);
  test "abbb" "bbba" 1 None;

  (* check for bugs where a small common suffix, or common prefix, is
     enough to make the distance goes down *)
  test "xyzwabc" "mnpqrabc" 10 (Some 5);
  test "abcxyzw" "abcmnpqr" 10 (Some 5);

  (* check that using "max_int" as cutoff works *)
  test "a" "a" max_int (Some 0);
  test "a" "b" max_int (Some 1);
  test "abc" "ade" max_int (Some 2);

  (* check empty strings*)
  test "" "" 3 (Some 0);
  test "" "abc" 3 (Some 3);
  test "abcd" "" 3 None;
  
  ()

