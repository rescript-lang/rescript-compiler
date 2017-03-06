




let a = 
  match Js.Undefined.to_opt [%raw {|___undefined_value|}] with 
  | None -> 1 
  | Some _ -> 2 