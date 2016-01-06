{ 
 
}
rule translate = parse
| "current_directory"{ "." ^ translate lexbuf }
| _ as c{  String.make 1 c ^ translate lexbuf }
| eof{ "" }


{
open Mt
let suites = [
  "translate", (fun _ -> 
    assert_equal 
      (translate
         (Lexing.from_string "-- current_directory --"))
      "-- . --")
]

;; from_suites "simple_lexer" suites
    (* 
       print_string @@ translate @@ 
       Lexing.from_string "-- current_directory --"  *)

}
