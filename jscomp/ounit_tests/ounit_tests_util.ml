let time description f  =
  let start = Unix.gettimeofday () in 
  f ();
  let finish = Unix.gettimeofday () in
  Printf.printf "%s elapsed %f\n" description (finish -. start)  
