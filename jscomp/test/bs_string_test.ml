

let () =
  "ghso ghso g"
  |> Js.String.split " "
  |> Js.Array.reduce (fun [@bs] x y ->  x ^  "-" ^ y) ""
  |> Js.log 

