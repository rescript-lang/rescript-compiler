(* Test that two Random.self_init() in close succession will not result
   in the same PRNG state.
   Note that even when the code is correct this test is expected to fail
   once in 10000 runs.
*)

let () =
  Random.self_init ();
  let x = Random.int 10000 in
  Random.self_init ();
  let y = Random.int 10000 in
  if x = y then print_endline "FAILED" else print_endline "PASSED"
