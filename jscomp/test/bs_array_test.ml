
type 'a t = 'a Bs.Array.t
let () =
  [| 1; 2; 3; 4 |]
  |> Bs.Array.filter (fun [@bs] x -> x > 2)
  |> Bs.Array.mapi (fun [@bs] x i -> x + i)
  |> Bs.Array.reduce (fun [@bs] x y -> x + y) 0 
  |> Js.log
