



external get : int array -> int -> int option = "" 
[@@bs.get_index] [@@bs.return { undefined_to_opt }]


let f xs i = 
    match get xs i with 
    | None -> assert false 
    | Some k -> k 