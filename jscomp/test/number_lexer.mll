{
external log : string -> unit = "caml_alloc_dummy" [@@bs.val "console.log"]
let l = 
#if BS then 
 log 
#else output_string stdout
#end

}

rule  token l = parse
    [' ' '\t' '\r' '\n']  { l "new line" ; token l lexbuf}
  | ['0'-'9']+  { l "number"; l (Lexing.lexeme lexbuf); token l lexbuf }
  | ['a'-'z''A'-'Z']+ {l "ident"; l (Lexing.lexeme lexbuf); token l lexbuf}
  | '+'         { l "+" ; token l lexbuf }
  | '-'         { l "-" ; token l lexbuf}
  | '*'         { l "*" ; token l lexbuf}
  | '/'         { l "/" ; token l lexbuf}
  | '('         { l "(" ; token l lexbuf }
  | ')'         { l ")"; token l lexbuf }
  | eof         { l "eof" }

{
(* token l (Lexing.from_string "32 + 32 ( ) * / ") *)
(* token (Lexing.from_string "32") *)
}

(* local variables: *)
(* compile-command: "ocamlbuild number_lexer.byte --" *)
(* end: *)
