#mod_use "sexp_lexer.ml";;
#mod_use "sexp_eval.ml";;
#install_printer Sexp_eval.print_env;;
#install_printer Sexp_eval.print;;
(try ignore @@ Sexp_lexer.token (Lexing.from_string {|  (1 2 3  ( a (b) ) |}); 
   raise Not_found  with e -> ());;

Sexp_lexer.token (Lexing.from_string {|    ( a  ) |});;
Sexp_lexer.token (Lexing.from_string {|    ( a () ) |});;
Sexp_lexer.token (Lexing.from_string {|    ( a (b) ) |});;

Sexp_lexer.token (Lexing.from_string {|    ( a (b) ) (c d) |});;
Sexp_lexer.token (Lexing.from_string {|    ( a (b 1 2 3)  c  d) (c d) |});;


Sexp_eval.eval_string {|
(setq 
 bsc 
 "../bin/bsc.exe"
 bs-external-includes
 '( "../runtime" "../stdlib"  "./others")
 bs-package-name
 "bs-platform")

(setq 
 bsc-flags '("-w" "-40" "-bs-no-version-header " "-bs-diagnose" "-bs-cross-module-opt"))

|}
