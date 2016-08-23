

let rec append l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (append tl  l2)

let rec sort ls = 
  match ls with 
  | [] -> []
  | x::xs -> 
    append 
    (sort (List.filter (fun u -> u <= x) xs))
    (x :: sort (List.filter (fun u -> u > x) xs))

let () = 
    [| 1;3 ; 2;4;5; 10; 23; 3|]
    |> Array.to_list 
    |> sort
    |> Array.of_list
    |> Js.log     