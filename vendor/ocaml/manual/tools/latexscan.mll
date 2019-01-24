{
open Lexing;;
open Latexmacros;;

let delimiter = ref (char_of_int 0);;

let upto delim lexfun lexbuf =
  let old_delim = !delimiter in
  delimiter := delim;
  lexfun lexbuf;
  delimiter := old_delim;;

let verb_delim = ref (char_of_int 0);;

let brace_nesting = ref 0;;

let rindex c s =
  let rec find i =
    if i < 0 then raise Not_found else
    if s.[i] = c then i else find (i-1) in
  find (String.length s - 1);;

let first_caml_line = ref true;;
let in_caml = ref false;;
}

rule main = parse
(* Comments *)
    '%' [^ '\n'] * '\n' { main lexbuf }
(* Paragraphs *)
  | "\n\n" '\n' *
                { print_string "<P>\n"; main lexbuf }
(* Font changes *)
  | "{\\it" " "* | "{\\em" " "*
                  { print_string "<i>"; upto '}' main lexbuf;
                    print_string "</i>"; main lexbuf }
  | "{\\bf" " "*  { print_string "<b>"; upto '}' main lexbuf;
                    print_string "</b>"; main lexbuf }
  | "{\\rm" " "*  { print_string "<u>"; upto '}' main lexbuf;
                    print_string "</u>"; main lexbuf }
  | "{\\tt" " "*  { print_string "<tt>"; upto '}' main lexbuf;
                    print_string "</tt>"; main lexbuf }
  | '"'           { print_string "<tt>"; indoublequote lexbuf;
                    print_string "</tt>"; main lexbuf }
(* Verb, verbatim *)
  | "\\verb" _  { verb_delim := lexeme_char lexbuf 5;
                  print_string "<tt>"; inverb lexbuf; print_string "</tt>";
                  main lexbuf }
  | "\\begin{verbatim}"
                { print_string "<pre>"; inverbatim lexbuf;
                  print_string "</pre>"; main lexbuf }
(* Caml programs *)
  | "\\caml"
      	       	{ print_string "<pre>";
                  first_caml_line := true; in_caml := false;
                  camlprog lexbuf; print_string "</pre>"; main lexbuf }
(* Raw html, latex only *)
  | "\\begin{rawhtml}"
                { rawhtml lexbuf; main lexbuf }
  | "\\begin{latexonly}"
                { latexonly lexbuf; main lexbuf }
(* Itemize and similar environments *)
  | "\\item["   { print_string "<dt>"; upto ']' main lexbuf; 
                  print_string "<dd>"; main lexbuf }
  | "\\item"    { print_string "<li>"; main lexbuf }
(* Math mode (hmph) *)
  | "$"         { main lexbuf }
(* Special characters *)
  | "\\char" ['0'-'9']+
                { let lxm = lexeme lexbuf in
                  let code = String.sub lxm 5 (String.length lxm - 5) in
                  print_char(char_of_int(int_of_string code));
                  main lexbuf }
  | "<"         { print_string "&lt;"; main lexbuf }
  | ">"         { print_string "&gt;"; main lexbuf }
  | "~"         { print_string " "; main lexbuf }
(* Definitions of very simple macros *)
  | "\\def\\" (['A'-'Z' 'a'-'z']+ | [^ 'A'-'Z' 'a'-'z']) "{" [^ '{' '}']* "}"
                { let s = lexeme lexbuf in
                  let l = String.length s in
                  let p = rindex '{' s in
                  let name = String.sub s 4 (p - 4) in
                  let expansion = String.sub s (p + 1) (l - p - 2) in
                  def_macro name [Print expansion];
                  main lexbuf }
(* General case for environments and commands *)
  | ("\\begin{" | "\\end{") ['A'-'Z' 'a'-'z']+ "}" |
    "\\" (['A'-'Z' 'a'-'z']+ '*'? | [^ 'A'-'Z' 'a'-'z'])
                { let exec_action = function
                      Print str -> print_string str
                    | Print_arg -> print_arg lexbuf
                    | Skip_arg -> skip_arg lexbuf in
                  List.iter exec_action (find_macro(lexeme lexbuf));
                  main lexbuf }
(* Default rule for other characters *)
  | eof         { () }
  | _           { let c = lexeme_char lexbuf 0 in
                  if c == !delimiter then () else (print_char c; main lexbuf) }

and indoublequote = parse
    '"'         { () }
  | "<"         { print_string "&lt;"; indoublequote lexbuf }
  | ">"         { print_string "&gt;"; indoublequote lexbuf }
  | "&"         { print_string "&amp;"; indoublequote lexbuf }
  | "\\\""      { print_string "\""; indoublequote lexbuf }
  | "\\\\"      { print_string "\\"; indoublequote lexbuf }
  | _           { print_char(lexeme_char lexbuf 0); indoublequote lexbuf }

and inverb = parse
    "<"         { print_string "&lt;"; inverb lexbuf }
  | ">"         { print_string "&gt;"; inverb lexbuf }
  | "&"         { print_string "&amp;"; inverb lexbuf }
  | _           { let c = lexeme_char lexbuf 0 in
                  if c == !verb_delim then ()
                                      else (print_char c; inverb lexbuf) }
and inverbatim = parse
    "<"         { print_string "&lt;"; inverbatim lexbuf }
  | ">"         { print_string "&gt;"; inverbatim lexbuf }
  | "&"         { print_string "&amp;"; inverbatim lexbuf }
  | "\\end{verbatim}" { () }
  | _           { print_char(lexeme_char lexbuf 0); inverbatim lexbuf }

and camlprog = parse
    "<"         { print_string "&lt;"; camlprog lexbuf }
  | ">"         { print_string "&gt;"; camlprog lexbuf }
  | "&"         { print_string "&amp;"; camlprog lexbuf }
  | "\\?"       { if !first_caml_line then begin
                    print_string "# <FONT COLOR=\"blue\">";
                    first_caml_line := false
                  end else
                    print_string "  <FONT COLOR=\"blue\">";
                  in_caml := true;
                  camlprog lexbuf }
  | "\\:"       { print_string "<FONT COLOR=\"green\">";
                  in_caml := true;
                  camlprog lexbuf }
  | "\\;"       { first_caml_line := true; camlprog lexbuf }
  | "\\\\"      { print_string "\\"; camlprog lexbuf }
  | "\\endcaml" { () }
  | "\n"        { if !in_caml then begin
                    print_string "</FONT>";
                    in_caml := false
                  end;
                  print_char '\n';
                  camlprog lexbuf }
  | _           { print_char(lexeme_char lexbuf 0); camlprog lexbuf }

and rawhtml = parse
    "\\end{rawhtml}" { () }
  | _           { print_char(lexeme_char lexbuf 0); rawhtml lexbuf }

and latexonly = parse
    "\\end{latexonly}" { () }
  | _           { latexonly lexbuf }

and print_arg = parse
    [' ' '\n'] * "{" { upto '}' main lexbuf }
  | _           { print_char(lexeme_char lexbuf 0); rawhtml lexbuf }

and skip_arg = parse
    "{"         { incr brace_nesting; skip_arg lexbuf }
  | "}"         { decr brace_nesting;
                  if !brace_nesting > 0 then skip_arg lexbuf }
  | _           { skip_arg lexbuf }


