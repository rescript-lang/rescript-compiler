(** Test that weak variables keep their names long enough *)

let f y = fun x -> x
let blah = f 0
let splash () = blah (failwith "coucou")
let blurp = f 0;;

blah 1;;

let g = f ();;

g (fun x -> x);;
let h = g (f ());;
