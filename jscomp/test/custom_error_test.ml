


let test_js_error () = 
  match Js.Json.exnParse {| {"x" : }|} with
  | (exception
    Js.Exn.Error err ) ->
    Js.log @@ Js.Exn.stack err;
    None
  | e -> Some e 


let test_js_error2 () = 
  try Js.Json.exnParse {| {"x" : }|} with
  |(Js.Exn.Error err ) as e ->
    Js.log @@ Js.Exn.stack err;
    raise e

let example1 () = 
    match Js.Json.exnParse {| {"x"  }|} with 
    | exception Js.Exn.Error err -> 
        Js.log @@ Js.Exn.stack err;
        None
    | v -> Some v 

let example2 () = 
    try Some (Js.Json.exnParse {| {"x"}|}) with 
    Js.Exn.Error _ -> None        

(*let () = 
  Js.log @@ test_js_error () 
*)
