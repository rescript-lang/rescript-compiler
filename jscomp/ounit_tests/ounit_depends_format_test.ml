let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let ( =~ ) (xs : string list) (ys : string list) =
  OUnit.assert_equal xs ys ~printer:(fun xs -> String.concat "," xs)

let f (x : string) =
  let stru = Parse.implementation (Lexing.from_string x) in
  Ast_extract.String_set.elements
    (Ast_extract.read_parse_and_extract Ml_binary.Ml stru)

let suites =
  __FILE__
  >::: [ ( __LOC__
         >:: fun _ ->
         f {|module X = List|} =~ ["List"] ;
         f {|module X = List module X0 = List1|} =~ ["List"; "List1"] ) ]
