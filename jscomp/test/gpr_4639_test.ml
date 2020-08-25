let x url =
  match url with   
  | [] -> `start
  | ["login"] -> `login
  | ["start"] -> `start
  | _ -> `start