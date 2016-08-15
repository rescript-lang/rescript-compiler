

let () =
  match Js.Undefined.to_opt  [%bs.node require] with
  | None ->   ()
  | Some u ->               
    Js.log @@ u#@resolve "./test_require.js";
    if u##main == [%bs.node module_] && u##main != Js.Undefined.empty then
      Js.log "is main"
    else
      Js.log "not main"
