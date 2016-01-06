{
  open Arith_parser
}

rule lexeme = parse
    [' ' '\t' '\r' '\n']  { lexeme lexbuf }
  | ['0'-'9']+  { NUMERAL (float (int_of_string (Lexing.lexeme lexbuf))) }
  | ['a'-'z''A'-'Z']+ {IDENT (Lexing.lexeme lexbuf)}
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { TIMES }
  | '/'         { DIVIDE }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | eof         { EOF }

{
open Arith_syntax
let rec str e = 
  match e with 
  | Numeral  f -> string_of_float f 
  | Plus (a,b) -> str a ^ "+" ^ str b 
  | Minus (a,b) -> str a ^ "-" ^ str b 
  | Times (a,b) -> str a ^ "*" ^ str b 
  | Divide (a,b) -> str a ^ "/" ^ str b 
  | Negate a -> "-" ^ str a 
  | Variable s -> s 

(* let _ = Parsing.set_trace true;; *)
}
