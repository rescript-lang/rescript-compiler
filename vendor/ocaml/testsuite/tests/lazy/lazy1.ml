(* Mantis 7301, due to A. Frisch *)

let foo () =
  (fun xs0 () -> Lazy.force (List.hd xs0) ())
    (List.map (fun g -> lazy g)
       [Lazy.force (  lazy ( let _ = () in fun () -> ()  ) )]
    )

let () =
  let gen = foo () in
  gen ();
  Gc.compact ();
  print_char 'A'; flush stdout;
  gen ()
