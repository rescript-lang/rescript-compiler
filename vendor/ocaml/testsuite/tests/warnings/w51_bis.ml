let rec foldl op acc = function
    [] -> acc
    | x :: xs ->
        try (foldl [@tailcall]) op (op x acc) xs
        with Not_found -> assert false
