let f (type t) () =
  let exception E of t in
  (fun x -> E x), (function E _ -> print_endline "OK" | _ -> print_endline "KO")

let inj1, proj1 = f ()
let inj2, proj2 = f ()

let () = proj1 (inj1 42)
let () = proj1 (inj2 42)
