let ff x =
  let a =
    match x with
    | "0" | "1" | "2" -> 3
    | "3" -> 4
    | "4" -> 6
    | "7" -> 7
    | _ -> 8 in
  a + 3

let f2 x =
  ( match x with
  | "0" | "1" | "2" -> 3
  | "3" -> 4
  | "4" -> 6
  | "7" -> 7
  | _ -> 8 )
  + 3

type u = A of int * int | B of int

let f (x : u) =
  let y = match x with A _ -> 3 | B _ -> 4 in
  y + 32

let f2 (x : u) =
  let v = ref 0 in
  let y =
    match x with
    | A _ ->
        v := 1 ;
        let z = 1 in
        let z = z + 32 in
        z + 3
    | B _ ->
        v := 1 ;
        let z = 1 in
        let z = z + 32 in
        z + 4 in
  y + 32

let f3 (x : u) =
  let v = ref 0 in
  let y =
    match x with
    | A _ ->
        v := 1 ;
        3
    | B _ ->
        v := 1 ;
        4 in
  y + 32
