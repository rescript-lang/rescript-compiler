

let asJsExn = Js.Exn.asJsExn 
let test_js_error () = 
  match Js.Json.parseExn {| {"x" : }|} with
  |  exception err  ->
    begin match asJsExn err with 
    | Some err ->
      Js.log @@ Js.Exn.stack err;
      None
    | None -> raise err end
  | e -> Some e 


let test_js_error2 () = 
  try Js.Json.parseExn {| {"x" : }|} with
  | e -> 
    (match asJsExn e with 
     | None -> raise e 
     | Some err ->
       Js.log @@ Js.Exn.stack err;
       raise e)

let example1 () = 
    match Js.Json.parseExn {| {"x"  }|} with 
    | exception err -> 
      begin match asJsExn err with 
      | Some err ->
        Js.log @@ Js.Exn.stack err;
        None
      | None -> raise err end  
    | v -> Some v 

let example2 () = 
    try Some (Js.Json.parseExn {| {"x"}|}) with 
    e -> 
    if asJsExn e <> None then None else raise e 

(*let () = 
  Js.log @@ test_js_error () 
*)
