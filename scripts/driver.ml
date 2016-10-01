#directory "+compiler-libs";;
#load "ocamlcommon.cma";;
#mod_use "/Users/hzhang295/git/ocaml-wk/parsing/lexer.ml";;
let d token lexbuf =
   let c = ref [] in
   try 
     while true do 
       let t = token lexbuf in
       if t <> Parser.EOF then 
         c := t :: !c 
       else  raise End_of_file
     done;
     !c 
   with 
   | End_of_file -> List.rev !c 
   | Lexer.Error(e,loc) -> 
     Format.fprintf Format.err_formatter  
       "@[%a : %a @]@."
       Lexer.report_error e Location.print loc  ; 
     List.rev !c ;;



let f str = 
  Lexer.init () ;
  d (fun lexbuf ->  Lexer.token lexbuf ) 
    (Lexing.from_string str) ;;

f {|
#if ocaml_major > 3 && ocaml_minor > 1 
    1 + 2 
#else 
    32
#end
|}

;; f {|
#if ocaml_major > 3 && ocaml_minor > 10
  1 + 2 
#end
|}

;; f {|
#if ocaml_major > 3 && ocaml_minor > 1
  1 + 2 
#end
|}

;; f {|
#if ocaml_major > 3 && ocaml_minor > 1
  1 + 2 
#else 
#end
|}

;; f {|
#if ocaml_major > 3 && ocaml_minor > 1
  1 + 2 
#else 
#if
#end
|}
