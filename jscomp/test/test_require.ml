

let () =
  match [%node require] with
  | None ->   ()
  | Some u ->               
    Js.log @@ u#@resolve "./test_require.js";
    match [%node _module], Js.Undefined.toOption u##main with
    | Some x, Some y when x == y ->
      Js.log "is main"
    | Some _, Some _
    | None, Some _ 
    | None, None  
    | Some _, None -> 
      Js.log "not main"

