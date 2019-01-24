let rec c = lazy (0 + d) and d = 3;;

let () = Printf.printf "%d\n" (Lazy.force c)
