


let f obj = 
  if Js.typeof obj = "function" then 
    ()
  else 
    let size = Bs_obj.size_of_any obj in
    match Js.Undefined.to_opt size with
    | None -> ()
    | Some s -> Js.log s (* TODO: This case should be peepwholed .. *)
