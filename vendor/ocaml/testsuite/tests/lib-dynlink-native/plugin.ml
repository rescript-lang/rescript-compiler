let rec f x = ignore ([x]); f x

let rec fact n = if n = 0 then 1 else n * fact (n - 1)

let facts = [ fact 1; fact 2; fact 3; fact (Random.int 4) ]

let () =
  Api.reg_mod "Plugin";
  Api.add_cb (fun () -> print_endline "Callback from plugin");
  print_endline "COUCOU";
  ()
