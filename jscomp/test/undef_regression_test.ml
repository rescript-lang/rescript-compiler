


let f obj = 
  if Js.typeof obj = "function" then 
    ()
  else 
    let size = Caml_obj_extern.size_of_t obj in
    match Js.Undefined.toOption size with
    | None -> ()
    | Some s -> Js.log s (* TODO: This case should be peepwholed .. *)
