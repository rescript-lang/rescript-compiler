
let get_tokens lex str = 
  let  buf = Lexing.from_string str in
  let rec aux acc = 
    let v = lex buf in
    if v = Arith_parser.EOF then 
      List.rev acc 
    else aux (v :: acc) in
  aux []

let f = get_tokens Arith_lexer.lexeme 

let from_tokens lst = 
  let l = ref lst in
  let rec aux () = 
    match !l with 
    | [] -> raise End_of_file 
    | y::ys -> l := ys; y in
  aux 

(* let wrap   parse f =  *)
(* (Lexing.lexbuf -> Arith_parser.token) -> *)
(*   Lexing.lexbuf -> Arith_syntax.expression *)


let lexer_suites = 
  Mt.[
  "arith_token", (fun _ -> 
    Eq( f "x + 3 + 4 + y", [IDENT "x"; PLUS; NUMERAL 3.;
                            PLUS; NUMERAL 4.; PLUS;
                            IDENT "y"]));

  "simple token", (fun _ -> 
    Eq( Arith_lexer.lexeme (Lexing.from_string "10"), NUMERAL 10. )
                  );
  "number_lexer", (fun _ -> 
    let v = ref [] in
    let add t =  v:= t :: !v in
    Number_lexer.token add (Lexing.from_string "32 + 32 ( ) * / ");
    Eq (List.rev !v,  [
        "number";
        "32";
        "new line";
        "+";
        "new line";
        "number";
        "32";
        "new line";
        "(";
        "new line";
        ")";
        "new line";
        "*";
        "new line";
        "/";
        "new line";
        "eof"]));  
  "simple number", (fun _ ->
    Eq (
    Arith_syntax.str (
    Arith_parser.toplevel
      Arith_lexer.lexeme
      (Lexing.from_string "10")) , "10."
   ));
  "arith", (fun _ ->
    Eq ((Arith_syntax.str
           (Arith_parser.toplevel
              Arith_lexer.lexeme
              (Lexing.from_string "x + 3 + 4 + y")
           )
        ),  "x+3.+4.+y"));

  

]

;; Mt.from_pair_suites __MODULE__ lexer_suites
