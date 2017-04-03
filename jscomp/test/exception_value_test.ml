

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

let u = A 3 
let test_not_found f () = 
  try f () with 
  Not_found -> 2
  (** note in ocaml, if such exception is not caught,
  it will be re-raised *)

let test_js_error () = 
  match Js.Json.parse {| {"x" : }|} with
  | (exception
    Js.Exn.Error err ) ->
    Js.log @@ Js.Exn.stack err;
    raise Not_found
  | e -> e 


let test_js_error2 () = 
  try Js.Json.parse {| {"x" : }|} with
  |(Js.Exn.Error err ) as e ->
    Js.log @@ Js.Exn.stack err;
    raise e

