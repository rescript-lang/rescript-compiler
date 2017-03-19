let x = [|1; 2|]
let y = try Array.get x 3 with
        Invalid_argument msg -> print_endline msg; 0
