# 1 "json_lexer.gen.mll"
 
type error =
  | Illegal_character of char
  | Unterminated_string
  | Illegal_escape of string
  | Unexpected_token 
  | Expect_comma_or_rbracket
  | Expect_comma_or_rbrace
  | Expect_colon
  | Expect_string_or_rbrace 
  | Expect_eof 
exception Error of error * Lexing.position * Lexing.position;;

type path = string list 



type token = 
  | Comma
  | Eof
  | False
  | Lbrace
  | Lbracket
  | Null
  | Colon
  | Number of string
  | Rbrace
  | Rbracket
  | String of string
  | True   
  

let error  (lexbuf : Lexing.lexbuf) e = 
  raise (Error (e, lexbuf.lex_start_p, lexbuf.lex_curr_p))

let lexeme_len (x : Lexing.lexbuf) =
  x.lex_curr_pos - x.lex_start_pos

let update_loc ({ lex_curr_p; _ } as lexbuf : Lexing.lexbuf) diff =
  lexbuf.lex_curr_p <-
    {
      lex_curr_p with
      pos_lnum = lex_curr_p.pos_lnum + 1;
      pos_bol = lex_curr_p.pos_cnum - diff;
    }

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let dec_code c1 c2 c3 =
  100 * (Char.code c1 - 48) + 10 * (Char.code c2 - 48) + (Char.code c3 - 48)

let hex_code c1 c2 =
  let d1 = Char.code c1 in
  let val1 =
    if d1 >= 97 then d1 - 87
    else if d1 >= 65 then d1 - 55
    else d1 - 48 in
  let d2 = Char.code c2 in
  let val2 =
    if d2 >= 97 then d2 - 87
    else if d2 >= 65 then d2 - 55
    else d2 - 48 in
  val1 * 16 + val2

let lf = '\010'

# 74 "json_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\240\255\241\255\242\255\000\000\025\000\011\000\023\000\
    \245\255\246\255\247\255\248\255\249\255\250\255\000\000\000\000\
    \000\000\001\000\254\255\005\000\001\000\002\000\253\255\000\000\
    \000\000\003\000\252\255\001\000\003\000\251\255\005\000\079\000\
    \089\000\099\000\121\000\131\000\141\000\153\000\163\000\006\000\
    \246\255\073\000\248\255\216\000\255\255\249\255\250\000\182\000\
    \252\255\009\000\011\000\063\000\226\000\251\255\033\001\250\255\
    ";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\012\000\012\000\015\000\015\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\015\000\015\000\
    \015\000\015\000\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\011\000\255\255\
    \255\255\012\000\255\255\012\000\255\255\012\000\255\255\255\255\
    \255\255\008\000\255\255\255\255\255\255\255\255\006\000\006\000\
    \255\255\006\000\001\000\002\000\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\000\000\255\255\255\255\000\000\030\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\041\000\
    \000\000\041\000\000\000\045\000\000\000\000\000\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\000\000\255\255\000\000\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\019\000\018\000\018\000\019\000\017\000\019\000\255\255\
    \042\000\019\000\255\255\051\000\050\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \019\000\000\000\003\000\000\000\000\000\019\000\000\000\000\000\
    \044\000\000\000\000\000\050\000\009\000\006\000\032\000\007\000\
    \004\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\008\000\004\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\031\000\030\000\032\000\
    \051\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\013\000\000\000\012\000\031\000\051\000\
    \000\000\023\000\043\000\000\000\000\000\031\000\015\000\022\000\
    \026\000\000\000\000\000\255\255\024\000\028\000\014\000\029\000\
    \000\000\000\000\020\000\025\000\016\000\027\000\021\000\000\000\
    \000\000\000\000\038\000\011\000\038\000\010\000\031\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\036\000\255\255\036\000\000\000\
    \034\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\000\000\
    \034\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\000\000\000\000\000\000\
    \000\000\000\000\050\000\000\000\000\000\049\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \048\000\000\000\048\000\000\000\000\000\000\000\000\000\048\000\
    \002\000\000\000\000\000\000\000\000\000\255\255\040\000\000\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\048\000\000\000\000\000\000\000\
    \000\000\000\000\048\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\000\000\000\000\000\000\000\000\000\000\048\000\000\000\
    \000\000\255\255\048\000\000\000\048\000\000\000\000\000\000\000\
    \046\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\000\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\017\000\000\000\000\000\019\000\030\000\
    \039\000\019\000\030\000\049\000\050\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\255\255\255\255\019\000\255\255\255\255\
    \039\000\255\255\255\255\050\000\000\000\000\000\004\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\004\000\007\000\005\000\
    \051\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\041\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\005\000\051\000\
    \255\255\015\000\039\000\255\255\255\255\004\000\000\000\021\000\
    \025\000\255\255\255\255\041\000\023\000\027\000\000\000\028\000\
    \255\255\255\255\016\000\024\000\000\000\014\000\020\000\255\255\
    \255\255\255\255\031\000\000\000\031\000\000\000\005\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\034\000\041\000\034\000\255\255\
    \033\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\255\255\
    \033\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\255\255\255\255\255\255\
    \255\255\255\255\043\000\255\255\255\255\043\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \043\000\255\255\043\000\255\255\255\255\255\255\255\255\043\000\
    \000\000\255\255\255\255\255\255\255\255\030\000\039\000\255\255\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\043\000\255\255\255\255\255\255\
    \255\255\255\255\043\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\255\255\255\255\255\255\255\255\255\255\043\000\255\255\
    \255\255\041\000\043\000\255\255\043\000\255\255\255\255\255\255\
    \043\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\255\255\054\000\054\000\054\000\054\000\054\000\054\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\054\000\054\000\054\000\054\000\054\000\054\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \043\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec lex_json buf lexbuf =
    __ocaml_lex_lex_json_rec buf lexbuf 0
and __ocaml_lex_lex_json_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 88 "json_lexer.gen.mll"
          ( lex_json buf lexbuf)
# 264 "json_lexer.ml"

  | 1 ->
# 89 "json_lexer.gen.mll"
                   ( 
    update_loc lexbuf 0;
    lex_json buf  lexbuf
  )
# 272 "json_lexer.ml"

  | 2 ->
# 94 "json_lexer.gen.mll"
         ( True)
# 277 "json_lexer.ml"

  | 3 ->
# 95 "json_lexer.gen.mll"
          (False)
# 282 "json_lexer.ml"

  | 4 ->
# 96 "json_lexer.gen.mll"
         (Null)
# 287 "json_lexer.ml"

  | 5 ->
# 97 "json_lexer.gen.mll"
       (Lbracket)
# 292 "json_lexer.ml"

  | 6 ->
# 98 "json_lexer.gen.mll"
       (Rbracket)
# 297 "json_lexer.ml"

  | 7 ->
# 99 "json_lexer.gen.mll"
       (Lbrace)
# 302 "json_lexer.ml"

  | 8 ->
# 100 "json_lexer.gen.mll"
       (Rbrace)
# 307 "json_lexer.ml"

  | 9 ->
# 101 "json_lexer.gen.mll"
       (Comma)
# 312 "json_lexer.ml"

  | 10 ->
# 102 "json_lexer.gen.mll"
        (Colon)
# 317 "json_lexer.ml"

  | 11 ->
# 103 "json_lexer.gen.mll"
                      (lex_json buf lexbuf)
# 322 "json_lexer.ml"

  | 12 ->
# 105 "json_lexer.gen.mll"
         ( Number (Lexing.lexeme lexbuf))
# 327 "json_lexer.ml"

  | 13 ->
# 107 "json_lexer.gen.mll"
      (
  let pos = Lexing.lexeme_start_p lexbuf in
  scan_string buf pos lexbuf;
  let content = (Buffer.contents  buf) in 
  Buffer.clear buf ;
  String content 
)
# 338 "json_lexer.ml"

  | 14 ->
# 114 "json_lexer.gen.mll"
       (Eof )
# 343 "json_lexer.ml"

  | 15 ->
let
# 115 "json_lexer.gen.mll"
       c
# 349 "json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 115 "json_lexer.gen.mll"
          ( error lexbuf (Illegal_character c ))
# 353 "json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_lex_json_rec buf lexbuf __ocaml_lex_state

and scan_string buf start lexbuf =
    __ocaml_lex_scan_string_rec buf start lexbuf 39
and __ocaml_lex_scan_string_rec buf start lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 120 "json_lexer.gen.mll"
      ( () )
# 365 "json_lexer.ml"

  | 1 ->
# 122 "json_lexer.gen.mll"
  (
        let len = lexeme_len lexbuf - 2 in
        update_loc lexbuf len;

        scan_string buf start lexbuf
      )
# 375 "json_lexer.ml"

  | 2 ->
# 129 "json_lexer.gen.mll"
      (
        let len = lexeme_len lexbuf - 3 in
        update_loc lexbuf len;
        scan_string buf start lexbuf
      )
# 384 "json_lexer.ml"

  | 3 ->
let
# 134 "json_lexer.gen.mll"
                                               c
# 390 "json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 135 "json_lexer.gen.mll"
      (
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf start lexbuf
      )
# 397 "json_lexer.ml"

  | 4 ->
let
# 139 "json_lexer.gen.mll"
                 c1
# 403 "json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 139 "json_lexer.gen.mll"
                               c2
# 408 "json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 139 "json_lexer.gen.mll"
                                             c3
# 413 "json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and
# 139 "json_lexer.gen.mll"
                                                    s
# 418 "json_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 4) in
# 140 "json_lexer.gen.mll"
      (
        let v = dec_code c1 c2 c3 in
        if v > 255 then
          error lexbuf (Illegal_escape s) ;
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      )
# 429 "json_lexer.ml"

  | 5 ->
let
# 148 "json_lexer.gen.mll"
                        c1
# 435 "json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 148 "json_lexer.gen.mll"
                                         c2
# 440 "json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3) in
# 149 "json_lexer.gen.mll"
      (
        let v = hex_code c1 c2 in
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      )
# 449 "json_lexer.ml"

  | 6 ->
let
# 155 "json_lexer.gen.mll"
             c
# 455 "json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 156 "json_lexer.gen.mll"
      (
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;

        scan_string buf start lexbuf
      )
# 464 "json_lexer.ml"

  | 7 ->
# 163 "json_lexer.gen.mll"
      (
        update_loc lexbuf 0;
        Buffer.add_char buf lf;

        scan_string buf start lexbuf
      )
# 474 "json_lexer.ml"

  | 8 ->
# 170 "json_lexer.gen.mll"
      (
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Buffer.add_substring buf lexbuf.lex_buffer ofs len;

        scan_string buf start lexbuf
      )
# 485 "json_lexer.ml"

  | 9 ->
# 178 "json_lexer.gen.mll"
      (
        error lexbuf Unterminated_string
      )
# 492 "json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_scan_string_rec buf start lexbuf __ocaml_lex_state

;;

# 182 "json_lexer.gen.mll"
 

type js_array =
  { content : t array ; 
    loc_start : Lexing.position ; 
    loc_finish : Lexing.position ; 
  }
and t = 
  [  
    `True
  | `False
  | `Null
  | `Flo of string 
  | `Str of string 
  | `Arr  of js_array
  | `Obj of t String_map.t 
   ]

type status = 
  | No_path
  | Found  of t 
  | Wrong_type of path 



let rec parse_json lexbuf =
  let buf = Buffer.create 64 in 
  let look_ahead = ref None in
  let token () : token = 
    match !look_ahead with 
    | None ->  
      lex_json buf lexbuf 
    | Some x -> 
      look_ahead := None ;
      x 
  in
  let push e = look_ahead := Some e in 
  let rec json (lexbuf : Lexing.lexbuf) = 
    match token () with 
    | True -> `True
    | False -> `False
    | Null -> `Null
    | Number s ->  `Flo s 
    | String s -> `Str s 
    | Lbracket -> parse_array lexbuf.lex_start_p lexbuf.lex_curr_p [] lexbuf
    | Lbrace -> parse_map String_map.empty lexbuf
    |  _ -> error lexbuf Unexpected_token
  and parse_array  loc_start loc_finish acc lexbuf =
    match token () with 
    | Rbracket -> `Arr {loc_start ; content = Ext_array.reverse_of_list acc ; 
                            loc_finish = lexbuf.lex_curr_p }
    | x -> 
      push x ;
      let new_one = json lexbuf in 
      begin match token ()  with 
      | Comma -> 
          parse_array loc_start loc_finish (new_one :: acc) lexbuf 
      | Rbracket 
        -> `Arr {content = (Ext_array.reverse_of_list (new_one::acc));
                     loc_start ; 
                     loc_finish = lexbuf.lex_curr_p }
      | _ -> 
        error lexbuf Expect_comma_or_rbracket
      end
  and parse_map acc lexbuf = 
    match token () with 
    | Rbrace -> `Obj acc 
    | String key -> 
      begin match token () with 
      | Colon ->
        let value = json lexbuf in
        begin match token () with 
        | Rbrace -> `Obj (String_map.add key value acc )
        | Comma -> 
          parse_map (String_map.add key value acc) lexbuf 
        | _ -> error lexbuf Expect_comma_or_rbrace
        end
      | _ -> error lexbuf Expect_colon
      end
    | _ -> error lexbuf Expect_string_or_rbrace
  in 
  let v = json lexbuf in 
  match token () with 
  | Eof -> v 
  | _ -> error lexbuf Expect_eof

let parse_json_from_string s = 
  parse_json (Lexing.from_string s )

let parse_json_from_file s = 
  let in_chan = open_in s in 
  let lexbuf = Lexing.from_channel in_chan in 
  match parse_json lexbuf with 
  | exception e -> close_in in_chan ; raise e
  | v  -> close_in in_chan;  v


type callback = 
  [
    `Str of (string -> unit) 
  | `Flo of (string -> unit )
  | `Bool of (bool -> unit )
  | `Obj of (t String_map.t -> unit)
  | `Arr of (t array -> unit )
  | `Null of (unit -> unit)
  ]

let test   ?(fail=(fun () -> ())) key 
    (cb : callback) m 
     =
     begin match String_map.find key m, cb with 
       | exception Not_found -> fail ()
       | `True, `Bool cb -> cb true
       | `False, `Bool cb  -> cb false 
       | `Flo s , `Flo cb  -> cb s 
       | `Obj b , `Obj cb -> cb b 
       | `Arr {content}, `Arr cb -> cb content 
       | `Null, `Null cb  -> cb ()
       | `Str s, `Str cb  -> cb s 
       | _, _ -> fail () 
     end;
     m
let query path (json : t ) =
  let rec aux acc paths json =
    match path with 
    | [] ->  Found json
    | p :: rest -> 
      begin match json with 
        | `Obj m -> 
          begin match String_map.find p m with 
            | m' -> aux (p::acc) rest m'
            | exception Not_found ->  No_path
          end
        | _ -> Wrong_type acc 
      end
  in aux [] path json

# 637 "json_lexer.ml"
