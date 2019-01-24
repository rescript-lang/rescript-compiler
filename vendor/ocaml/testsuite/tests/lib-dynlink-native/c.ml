let () =
  print_endline "C is running";
  incr A.x;
  Printf.printf "A.x = %i\n" !A.x
