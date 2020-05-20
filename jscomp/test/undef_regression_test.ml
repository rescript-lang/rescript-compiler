

external size_of_t : Obj.t -> 'a Js.undefined = "length" [@@bs.get] 

let f obj = 
  if Js.typeof obj = "function" then 
    ()
  else 
    let size = size_of_t obj in
    match Js.Undefined.toOption size with
    | None -> ()
    | Some s -> Js.log s (* TODO: This case should be peepwholed ..*)
