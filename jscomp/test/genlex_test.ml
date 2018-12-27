
open Genlex 

let lexer = make_lexer ["+";"-";"*";"/";"let";"="; "("; ")"]  

let to_list s = 
  let rec aux acc = 
    match Stream.next s with 
    | exception Stream.Failure  -> List.rev acc 
    |   v -> aux (v :: acc) in
  aux []

let suites = Mt.[ "lexer_stream_genlex", (fun _ -> 
     Eq( [Int 3; Kwd "("; Int 3; Kwd "+"; Int 2; Int (-1); Kwd ")"], 
         "3(3 + 2 -1)" |> Stream.of_string |> lexer|> to_list))]

;; Mt.from_pair_suites __MODULE__ suites
