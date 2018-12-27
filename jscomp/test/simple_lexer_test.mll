{ 
 
}
rule translate = parse
| "current_directory"{ "." ^ translate lexbuf }
| _ as c{  String.make 1 c ^ translate lexbuf }
| eof{ "" }


{

let suites = Mt.[
  "translate", (fun _ -> 
    Eq
      (translate
         (Lexing.from_string "-- current_directory --"),
      "-- . --"))
]


;; Mt.from_pair_suites __MODULE__ suites
    (* 
       print_string @@ translate @@ 
       Lexing.from_string "-- current_directory --"  *)

}
