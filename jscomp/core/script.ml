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

Ext_json_parse.parse_json_from_file "../bsconfig.json";;
