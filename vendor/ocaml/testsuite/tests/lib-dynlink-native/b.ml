let () =
  print_endline "B is running";
  incr A.x;
  Printf.printf "A.x = %i\n" !A.x
