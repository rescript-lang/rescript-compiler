module Int_set = Set.Make(
    struct 
        type t = int
        let compare (x : int) (y: int) = Pervasives.compare x y
    end
)



let () = 
    [| 1; 5; 3; 4; 2|]
    |> Array.to_list
    |> Int_set.of_list
    |> Int_set.elements
    |> Array.of_list
    |> Js.log 
