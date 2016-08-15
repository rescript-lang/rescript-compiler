

let u = [%bs.node __require]

let () =
  Js.log @@ u#@resolve "./test_require.js"
let () = 
  if [%bs.node __require]##main ==
     Js.Undefined.return
       [%bs.node __module] then
    Js.log "is main"
  else
    Js.log "not main"
