let f : string A.t -> unit = function
    A.X s -> print_endline s

(* It is important that the line below is the last line of the file (see Makefile) *)
let () = f A.y
