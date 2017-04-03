



let test_js_error () = 
  match Js.Json.parse {| {"x" : }|} with
  | (exception
    Js.Exn.Error err ) ->
    Js.log @@ Js.Exn.stack err;
    None
  | e -> Some e 


let test_js_error2 () = 
  try Js.Json.parse {| {"x" : }|} with
  |(Js.Exn.Error err ) as e ->
    Js.log @@ Js.Exn.stack err;
    raise e

(*let () = 
  Js.log @@ test_js_error () 
*)