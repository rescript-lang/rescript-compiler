
type 'a t = 'a Js.Array.t
let () =
  [| 1; 2; 3; 4 |]
  |> Js.Array.filter (fun [@bs] x -> x > 2)
  |> Js.Array.mapi (fun [@bs] x i -> x + i)
  |> Js.Array.reduce (fun [@bs] x y -> x + y) 0 
  |> Js.log
