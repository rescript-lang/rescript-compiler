external new_uninitialized_array : int -> 'a array = "" [@@bs.new "Array"]

let caml_array_sub (x : 'a array) (offset : int) (len : int) =
  let result = new_uninitialized_array len in
  for j = 0 to len - 1 do
    result.(j) <- x.(offset + j)
  done ;
  result

(* let rec len l = *)
(* match l with *)
(* | [] -> 0 *)
(* | x::xs -> Array.length x + len xs *)

(* let caml_array_concat (l : 'a array list) : 'a array = *)
(* let v = len l in *)
(* let result = new_uninitialized_array v in *)

let caml_array_set xs index newval =
  if index < 0 || index >= Array.length xs then
    invalid_arg "index out of bounds"
  else xs.(index) <- newval

let caml_array_get xs index =
  if index < 0 || index >= Array.length xs then
    invalid_arg "index out of bounds"
  else xs.(index)

let caml_make_vect len init =
  let b = new_uninitialized_array len in
  for i = 0 to len - 1 do
    b.(i) <- init
  done ;
  b
