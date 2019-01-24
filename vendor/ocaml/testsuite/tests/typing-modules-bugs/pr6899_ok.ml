type 'a t = 'a option
let is_some = function
  | None -> false
  | Some _ -> true

let should_accept ?x () = is_some x
