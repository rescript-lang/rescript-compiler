let () =
  Random.self_init ();
  let x = Random.int 10000 in
  Random.self_init ();
  let y = Random.int 1000 in
  if x = y then print_endline "FAILED" else print_endline "PASSED"
