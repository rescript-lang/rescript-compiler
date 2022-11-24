open Js_obj


type  x = < say : int -> int >

let suites = Mt.[
  "empty", (fun _ ->
    Eq(0, empty () |> keys |> Array.length));
  "assign", (fun _ ->
    Eq([%obj { a = 1 }], assign (empty ()) [%obj { a = 1 }]));
]

;; Mt.from_pair_suites __MODULE__ suites

