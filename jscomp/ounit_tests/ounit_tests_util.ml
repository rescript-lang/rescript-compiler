


let time ?nums description  f  =
  match nums with 
  | None -> 
    begin 
      let start = Unix.gettimeofday () in 
      ignore @@ f ();
      let finish = Unix.gettimeofday () in
      Printf.printf "\n%s elapsed %f\n" description (finish -. start) ;
      flush stdout; 
    end

  | Some nums -> 
    begin 
        let start = Unix.gettimeofday () in 
        for _i = 0 to nums - 1 do 
          ignore @@ f ();
        done  ;
      let finish = Unix.gettimeofday () in
      Printf.printf "\n%s elapsed %f\n" description (finish -. start)  ;
      flush stdout;
    end
