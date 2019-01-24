(* PR#7531 *)

let f =
  (let _i = print_endline "first"
   in fun q -> fun i -> "") (print_endline "x")

let _ =
  let k = 
    (let _i = print_int 1 
     in fun q -> fun i -> "") () 
  in k (print_int 0)

let () =
  print_endline "foo";
  ignore ((f ()) : string);
  ignore ((f ()) : string);
  print_endline "bar"
