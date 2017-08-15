

let f () =
  raise  Not_found



let assert_f x = 
  let ()  = assert (x > 3) in 
  3 


let hh () = 
  let v = raise Not_found in 
  v + 3 
(* TODO: comment for line column number *)

exception A of int 
exception B 
exception C of int * int 
let u = A 3 
let test_not_found f () = 
  try f () with 
  Not_found -> 2
  (** note in ocaml, if such exception is not caught,
  it will be re-raised *)



let test_js_error2 () = 
  try Js.Json.parseExn {| {"x" : }|} with
  |(Js.Exn.Error err ) as e ->
    Js.log @@ Js.Exn.stack err;
    raise e

let test_js_error3 () = 
  try ignore @@ Js.Json.parseExn {| {"x"}|} ; 1 with 
    e -> 0

