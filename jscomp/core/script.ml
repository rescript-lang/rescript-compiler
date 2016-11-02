#mod_use "ext/ext_string.ml";;
#mod_use "ext/ext_bytes.ml";;
#mod_use "ext/string_map.ml";;
#mod_use "ext/ext_array.ml";;
#mod_use "json_lexer.ml";;
#mod_use "ext/ext_file_pp.ml";;

#install_printer String_map.print;;

let print_position fmt (pos : Lexing.position) = 
  Format.fprintf fmt "(%d,%d)" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
#install_printer print_position;;

Bs_json.parse_json_from_file "../bsconfig.json";;
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
