type t =
  { mutable x : int;
    y : int }

let f { x = c } =
    fun () -> c;;

let r = { x = 10; y = 20 };;

let h = f r;;

print_endline (string_of_int (h ()));;

r.x <- 20;;

print_endline (string_of_int (h ()));;

