open Js
let () =
  [| 1; 2; 3; 4 |]
  |> Array.filter (fun  x -> x > 2)
  |> Array.mapi (fun  x i -> x + i)
  |> Array.reduce (fun  x y -> x + y) 0
  |> log
