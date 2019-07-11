(* Int64.bits_of_float 1.0*)
let one_float = 4607182418800017408L

let int32_pairs =
  [|(32l, 4.48415508583941463e-44); (3l, 4.20389539297445121e-45)|]

let from_pairs pair =
  int32_pairs
  |> Array.mapi (fun i (i32, f) ->
         [ ( Printf.sprintf "int32_float_of_bits %d" i
           , fun _ -> Mt.Eq (Int32.float_of_bits i32, f) )
         ; ( Printf.sprintf "int32_bits_of_float %d" i
           , fun _ -> Mt.Eq (Int32.bits_of_float f, i32) ) ])
  |> Array.to_list |> List.concat

let suites =
  Mt.
    [ ("one", fun _ -> Eq (Int64.bits_of_float 1.0, one_float))
    ; ("two", fun _ -> Eq (Int64.float_of_bits one_float, 1.0)) ]
  @ from_pairs int32_pairs

;;
Mt.from_pair_suites __MODULE__ suites
