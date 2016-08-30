open Js
let () =
  [| 1; 2; 3; 4 |]
  |> Array.filter (fun [@bs] x -> x > 2)
  |> Array.mapi (fun [@bs] x i -> x + i)
  |> Array.reduce (fun [@bs] x y -> x + y) 0
  |> log
