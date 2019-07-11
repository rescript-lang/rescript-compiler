[@@@bs.config {no_export}]

type t = A_list | A_hashtable

let string_of_associative_type = function
  | A_list -> "list"
  | A_hashtable -> "Hashtbl.t"

;;
Js.log (string_of_associative_type A_list)

type t2 = A_list | A_hashtable | A_bad

let string_of_associative_type = function
  | A_list -> "list"
  | A_hashtable -> "Hashtbl.t"
  | A_bad -> "bad"

;;
Js.log (string_of_associative_type A_list)

let v = Some 3
let f = function Some _ -> "Some" | None -> "None"
let u v = f (Some v)

;;
Js.log (f v, f None, f (Some 3))

type v =
  | A of int
  | B of int
  | C of int
  | D of int
  | E of int
  | F of int
  | G of int
  | H of int

let ff = function
  | A _ -> "A"
  | B _ -> "B"
  | C _ -> "C"
  | D _ -> "D"
  | E _ -> "E"
  | F _ -> "F"
  | G _ -> "G"
  | H _ -> "H"

;;
Js.log (ff (A 3), (function A _ -> "A" | B _ -> "B" | _ -> "?") (A 3))
