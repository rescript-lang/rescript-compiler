{

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unbalanced_paren 
  | Unterminated_paren
  | Unterminated_string
  | Non_sexp_outside
exception Error of error * Lexing.position * Lexing.position;;

let error  (lexbuf : Lexing.lexbuf) e = 
  raise (Error (e, lexbuf.lex_start_p, lexbuf.lex_curr_p))


let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c -> c

let lf = '\010'

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

let update_loc ({ lex_curr_p; _ } as lexbuf : Lexing.lexbuf) diff =
  lexbuf.lex_curr_p <-
    {
      lex_curr_p with
      pos_lnum = lex_curr_p.pos_lnum + 1;
      pos_bol = lex_curr_p.pos_cnum - diff;
    }

let lexeme_len ({ lex_start_pos; lex_curr_pos; _ } : Lexing.lexbuf) =
  lex_curr_pos - lex_start_pos



type t  =
  | Atom of string 
  | List of t list
  | Data of t list 
  | Lit of string 



type st = 
  { sexps : (t list * bool) Stack.t ; 
    mutable top : t list   ;
    mutable has_prime : bool ;
    buf : Buffer.t;
    mutable paren_depth : int
  }

let push_atom lexbuf atom (buf : st ) = 
  buf.top <- atom:: buf.top

(** entering the new stack *)
let new_lparen has_prime buf = 
  buf.paren_depth <- buf.paren_depth + 1 ;
  Stack.push (buf.top, buf.has_prime) buf.sexps ;
  buf.top <- [];
  buf.has_prime <- has_prime

(** exit the stack *)
let new_rparen  buf lexbuf = 
  buf.paren_depth <- buf.paren_depth - 1 ; 
  if buf.paren_depth < 0  then
    error lexbuf Unbalanced_paren
  else 
    let new_sexp =
      if buf.has_prime then 
        Data (List.rev   buf.top)
      else List (List.rev   buf.top) 
    in 
    let top, has_prime =  Stack.pop buf.sexps in
    buf.top<- top;
    buf.has_prime<-has_prime;
    push_atom lexbuf new_sexp buf 

let get_data buf = buf.top

}

let lf = '\010'
let lf_cr = ['\010' '\013']
let dos_newline = "\013\010"
let blank = [' ' '\009' '\012']

let digit = ['0'-'9']
let hexdigit = digit | ['a'-'f' 'A'-'F']
let identchar = ['A'-'Z' 'a'-'z' '-' '?'  '_'  '0'-'9' '.' '=']


rule main buf  = parse
  | lf | dos_newline { 
    update_loc lexbuf 0;
    main (buf : st ) lexbuf  }
  | blank+ { main buf lexbuf  }
  | (';' (_ # lf_cr)*) {  main buf lexbuf }
  | "'(" {
    new_lparen true buf; 
    main buf lexbuf
  }
  | '(' { 
    new_lparen false buf ; 
    main buf lexbuf 
  }
  | ')' { 
      new_rparen  buf lexbuf; 
      main buf lexbuf 
  }
  | '"'
      {
        let pos = Lexing.lexeme_start_p lexbuf in
        scan_string buf.buf pos lexbuf;
        push_atom lexbuf  ( Lit (Buffer.contents  buf.buf)) buf;
        Buffer.clear buf.buf;
        main buf lexbuf
      }
  | identchar +  as s 
    { push_atom lexbuf (Atom s) buf ; 
      main buf lexbuf
    }
  | _ as c 
      {  error  lexbuf (Illegal_character c)}

  | eof {
    if buf.paren_depth > 0 then 
      error lexbuf Unterminated_paren
    else 
      get_data buf }

and scan_string buf start = parse
  | '"' { () }
  | '\\' lf [' ' '\t']*
      {
        let len = lexeme_len lexbuf - 2 in
        update_loc lexbuf len;

        scan_string buf start lexbuf
      }
  | '\\' dos_newline [' ' '\t']*
      {
        let len = lexeme_len lexbuf - 3 in
        update_loc lexbuf len;
        scan_string buf start lexbuf
      }
  | '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c)
      {
        Buffer.add_char buf (char_for_backslash c);
        scan_string buf start lexbuf
      }
  | '\\' (digit as c1) (digit as c2) (digit as c3) as s 
      {
        let v = dec_code c1 c2 c3 in
        if v > 255 then
          error lexbuf (Illegal_escape s) ;
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      }
  | '\\' 'x' (hexdigit as c1) (hexdigit as c2)
      {
        let v = hex_code c1 c2 in
        Buffer.add_char buf (Char.chr v);

        scan_string buf start lexbuf
      }
  | '\\' (_ as c)
      {
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;

        scan_string buf start lexbuf
      }
  | lf
      {
        update_loc lexbuf 0;
        Buffer.add_char buf lf;

        scan_string buf start lexbuf
      }
  | ([^ '\\' '"'] # lf)+
      {
        let ofs = lexbuf.lex_start_pos in
        let len = lexbuf.lex_curr_pos - ofs in
        Buffer.add_substring buf lexbuf.lex_buffer ofs len;

        scan_string buf start lexbuf
      }
  | eof
      {
        error lexbuf Unterminated_string
      }

{ 

    let token  lexbuf  =
      List.rev @@ main { 
        buf = Buffer.create 256 ;
        sexps = Stack.create () ; 
        paren_depth = 0; 
        top = [];
        has_prime = false } lexbuf
    let from_string str = 
      token (Lexing.from_string str)    
    let from_file file = 
      let in_channel =  open_in file in 
      match  token (Lexing.from_channel in_channel) with 
      | exception  e -> close_in in_channel; raise e 
      | sexps -> close_in in_channel ; sexps
}
