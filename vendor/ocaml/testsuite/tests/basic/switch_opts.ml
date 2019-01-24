(* Test for optimisation of jump tables to arrays of constants *)

let p = Printf.printf

type test =
  Test : 'b * 'a * ('b -> 'a) -> test

type t = A | B | C

(* These test functions need to have at least three cases.
   Functions with fewer cases don't trigger the optimisation,
   as they are compiled to if-then-else, not switch *)
let testcases = [
  Test (3, 3, function 1 -> 1 | 2 -> 2 | 3 -> 3 | _ -> 0);
  Test (3, -3, function 1 -> 1 | 2 -> 2 | 3 -> -3 | _ -> 0);
  Test (3, min_int, function 1 -> 1 | 2 -> 2 | 3 -> min_int | _ -> 0);
  Test (3, max_int, function 1 -> 1 | 2 -> 2 | 3 -> max_int | _ -> 0);
  Test (3, 3., function 1 -> 1. | 2 -> 2. | 3 -> 3. | _ -> 0.);
  Test (3, Sys.opaque_identity "c" ^ Sys.opaque_identity "c",
        function 1 -> "a" | 2 -> "b" | 3 -> "cc" | _ -> "");
  Test (3, List.rev [3;2;1], function 1 -> [] | 2 -> [42] | 3 -> [1;2;3] | _ -> [415]);

  Test (C, 3, function A -> 1 | B -> 2 | C -> 3);
  Test (C, -3, function A -> 1 | B -> 2 | C -> -3);
  Test (C, min_int, function A -> 1 | B -> 2 | C -> min_int);
  Test (C, max_int, function A -> 1 | B -> 2 | C -> max_int);
  Test (C, 3., function A -> 1. | B -> 2. | C -> 3.);
  Test (C, "c", function A -> "a" | B -> "b" | C -> "c");
  Test (C, List.rev [3;2;1], function A -> [] | B -> [42] | C -> [1;2;3]);

  Test (42, 42, function
  | 1 -> 1 | 2 -> 2 | 3 -> 3 | 4 -> 4 | 5 -> 5 | 6 -> 6 | 7 -> 7 | 8 -> 8
  | 9 -> 9 | 10 -> 10 | 11 -> 11 | 12 -> 12 | 13 -> 13 | 14 -> 14 | 15 -> 15
  | 16 -> 16 | 17 -> 17 | 18 -> 18 | 19 -> 19 | 20 -> 20 | 21 -> 21 | 22 -> 22
  | 23 -> 23 | 24 -> 24 | 25 -> 25 | 26 -> 26 | 27 -> 27 | 28 -> 28 | 29 -> 29
  | 30 -> 30 | 31 -> 31 | 32 -> 32 | 33 -> 33 | 34 -> 34 | 35 -> 35 | 36 -> 36
  | 37 -> 37 | 38 -> 38 | 39 -> 39 | 40 -> 40 | 41 -> 41 | 42 -> 42 | 43 -> 43
  | 44 -> 44 | 45 -> 45 | 46 -> 46 | 47 -> 47 | 48 -> 48 | 49 -> 49 | 50 -> 50
  | 51 -> 51 | 52 -> 52 | 53 -> 53 | 54 -> 54 | 55 -> 55 | 56 -> 56 | 57 -> 57
  | 58 -> 58 | 59 -> 59 | 60 -> 60 | 61 -> 61 | 62 -> 62 | 63 -> 63 | 64 -> 64
  | 65 -> 65 | 66 -> 66 | 67 -> 67 | 68 -> 68 | 69 -> 69 | 70 -> 70 | 71 -> 71
  | 72 -> 72 | 73 -> 73 | 74 -> 74 | 75 -> 75 | 76 -> 76 | 77 -> 77 | 78 -> 78
  | 79 -> 79 | 80 -> 80 | 81 -> 81 | 82 -> 82 | 83 -> 83 | 84 -> 84 | 85 -> 85
  | 86 -> 86 | 87 -> 87 | 88 -> 88 | 89 -> 89 | 90 -> 90 | 91 -> 91 | 92 -> 92
  | 93 -> 93 | 94 -> 94 | 95 -> 95 | 96 -> 96 | 97 -> 97 | 98 -> 98 | 99 -> 99
  | _ -> 0);

  Test (3, `Tertiary, function
  | 1 -> `Primary
  | 2 -> `Secondary
  | 3 -> `Tertiary
  | n -> invalid_arg "test")
  ]

let passes = ref 0
let run_test (Test (a, b, f)) =
  assert (f a = b);
  incr passes

let () =
  List.iter run_test testcases;
  Printf.printf "%d tests passed\n" !passes

