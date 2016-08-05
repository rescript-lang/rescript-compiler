external to_str : 'a -> string = "js_anything_to_string"
external to_json_string : 'a -> string = "js_json_stringify"
let debug x = print_endline (to_str x )
let pprint x = print_endline (to_json_string x)
let rec fib = function
  | 1 | 2 -> 1
  | n -> fib (n - 1 )  + fib (n - 2)
(** Imperative style *)
let sum n =
    let v  = ref 0 in
    for i = 0 to n do
       v := !v + i
    done;
    !v
let tail_sum n =
  let rec aux acc i =
    if i <= n then
      aux (acc + i) (i + 1)
    else acc
  in aux 0 0

(** List map *)
type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec map f = function
  | Nil -> Nil
  | Cons (x,xs) ->  Cons (f x [@bs], map f xs)

(** Test curry and uncurry calling convention *)
let test_curry x  y =  x + y
let f = test_curry 32

let () =
 let hello_ocaml = [|"h";"e";"y";"o";"c";"a";"m";"l"|] in
 hello_ocaml |> Array.to_list |> String.concat "," |> pprint              
        