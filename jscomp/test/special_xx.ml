let recover = function [@bs.exn]
  | Not_found -> `Not_found 
  | Invalid_argument _ -> `Invalid_argument
  | _ -> `Any 


let recover2 = fun e -> 
  if Js.Exn.isCamlExceptionOrOpenVariant e then
    match e with 
    | Not_found -> `Not_found
    | Invalid_argument _ -> `Invalid_argument
    | _ -> `Any 
  else 
    `Any
