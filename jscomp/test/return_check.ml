
type element
type dom
external getElementById : string -> element option = "" 
[@@bs.send.pipe:dom] [@@bs.return {null_to_opt}]

let test dom = 
    let elem = dom |> getElementById "haha"  in
    match elem with 
    | None -> 1 
    | Some ui -> Js.log ui ; 2   











external get : int array -> int -> int option = "" 
[@@bs.get_index] [@@bs.return { undefined_to_opt }]


let f xs i = 
    match get xs i with 
    | None -> assert false 
    | Some k -> k 



