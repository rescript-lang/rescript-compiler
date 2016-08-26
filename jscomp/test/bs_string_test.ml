

let () =
  "ghso ghso g"
  |> Bs.String.split " "
  |> Bs.Array.reduce (fun [@bs] x y ->  x ^  "-" ^ y) ""
  |> Js.log 

