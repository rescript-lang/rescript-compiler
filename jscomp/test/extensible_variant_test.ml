type attr = ..
type attr += Str of string

module N = struct type attr += Int of int * int end

type attr += Int of int * int

let to_int (x : attr) =
  match x with
  | Str _ -> -1
  | N.Int (a, _) -> a
  | Int (_, b) -> b
  | _ -> assert false

let suites =
  Mt.
    [ ("test_int", fun _ -> Eq (3, to_int (N.Int (3, 0))))
    ; ("test_int2", fun _ -> Eq (0, to_int (Int (3, 0))))
    ; ("test_string", fun _ -> Eq (-1, to_int (Str "x"))) ]

;;
Mt.from_pair_suites __MODULE__ suites
