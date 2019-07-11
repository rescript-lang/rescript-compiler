let time ?nums description f =
  match nums with
  | None ->
      let start = Unix.gettimeofday () in
      ignore @@ f () ;
      let finish = Unix.gettimeofday () in
      Printf.printf "\n%s elapsed %f\n" description (finish -. start) ;
      flush stdout
  | Some nums ->
      let start = Unix.gettimeofday () in
      for i = 0 to nums - 1 do
        ignore @@ f ()
      done ;
      let finish = Unix.gettimeofday () in
      Printf.printf "\n%s elapsed %f\n" description (finish -. start) ;
      flush stdout
