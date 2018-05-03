
let test path = 
  let open Node.Fs.Watch in 
  watch 
    path
    ~config:(config ~recursive:true ())
    ()
  |> on (`change (fun [@bs] event string_buffer -> Js.log (event, string_buffer)))  
  |> close 

