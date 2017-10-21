
(* préprocesseur pour les environnement ocaml (avec alltt) *)

{
  open Lexing 

  let slides = ref false

  let ocaml_keywords = 
    let h = Hashtbl.create 97 in
    List.iter (fun s -> Hashtbl.add h s ())
      [ 
	"fun"; "match"; "with"; "begin"; 
	"end"; "try"; "as"; "let"; "rec"; "in";
	"function"; "if"; "then"; "else"; "sig"; "val"; "type"; "module";
	"while"; "do"; "done"
      ];
    h

  let is_keyword s = Hashtbl.mem ocaml_keywords s 

}

let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* 

rule alltt = parse
  | '{'  { print_string "\\{"; alltt lexbuf }
  | '}'  { print_string "\\}"; alltt lexbuf }
(*
  | '\\' { print_string "\\ensuremath{\\backslash}"; alltt lexbuf }
*)
  | '#' { print_string "\\diese{}"; alltt lexbuf }
  | '_'  { print_string "\\_{}"; alltt lexbuf }
  | '%'  { print_string "\\%{}"; alltt lexbuf }
  | '&'  { print_string "\\&{}"; alltt lexbuf }
  | '%'  { print_string "\\%{}"; alltt lexbuf }
  | '\n' { print_string "\n"; alltt lexbuf }
  | "->" { print_string "\\ensuremath{\\rightarrow}"; alltt lexbuf }
  | "=>" { print_string "\\ensuremath{\\Rightarrow}"; alltt lexbuf }
  | "<->" { print_string "\\ensuremath{\\leftrightarrow}"; alltt lexbuf }
  | '\n' "\\end{" ( "ocaml" | "coq" ) "}\n" { print_newline () }
  | "\\emph{" [^'}']* '}' { print_string (lexeme lexbuf); alltt lexbuf }
  | eof  { () }
  | "'a" { print_string "\\ensuremath{\\alpha}"; alltt lexbuf }
  | "(*" | "*)" as s { print_string s; alltt lexbuf }
  | "*"  { print_string "\\ensuremath{\\times}"; alltt lexbuf }
  | ident as s
	{ if !slides && is_keyword s then begin
	    print_string "{\\color{blue}"; print_string (lexeme lexbuf);
	    print_string "}"
	  end else 
            print_string (lexeme lexbuf); 
	  alltt lexbuf 
	}
  | _   { print_string (lexeme lexbuf); alltt lexbuf }

and pp = parse
  | "\\begin{ocaml}\n" 
      { print_endline "\\begin{alltt}"; 
	alltt lexbuf;
	print_endline "\\end{alltt}";
	pp lexbuf }
  | eof 
      { () }
  | _ 
      { print_string (lexeme lexbuf); pp lexbuf }

{
  let f = Sys.argv.(1)
  let () = slides := (String.length f > 6 && String.sub f 0 7 = "slides-")
  let cin = open_in f
  let lb = from_channel cin
  let _ = pp lb
}
