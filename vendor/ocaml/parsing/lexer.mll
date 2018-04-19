(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The lexer definition *)

{
open Lexing
open Misc
open Parser

type directive_value =
  | Dir_bool of bool 
  | Dir_float of float
  | Dir_int of int
  | Dir_string of string
  | Dir_null 

type directive_type = 
  | Dir_type_bool 
  | Dir_type_float 
  | Dir_type_int 
  | Dir_type_string 
  | Dir_type_null 

let type_of_directive x =
  match x with 
  | Dir_bool _ -> Dir_type_bool
  | Dir_float _ -> Dir_type_float
  | Dir_int _ -> Dir_type_int
  | Dir_string _ -> Dir_type_string
  | Dir_null -> Dir_type_null

let string_of_type_directive x = 
  match x with 
  | Dir_type_bool  -> "bool"
  | Dir_type_float  -> "float"
  | Dir_type_int  -> "int"
  | Dir_type_string  -> "string"
  | Dir_type_null -> "null"

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Literal_overflow of string
  | Unterminated_paren_in_conditional
  | Unterminated_if
  | Unterminated_else 
  | Unexpected_token_in_conditional 
  | Expect_hash_then_in_conditional
  | Illegal_semver of string
  | Unexpected_directive 
  | Conditional_expr_expected_type of directive_type * directive_type

;;

exception Error of error * Location.t;;

let assert_same_type  lexbuf x y = 
  let lhs = type_of_directive x in let rhs =  type_of_directive y  in
  if lhs <> rhs then 
    raise (Error(Conditional_expr_expected_type(lhs,rhs), Location.curr lexbuf))
  else y

let directive_built_in_values  =
  Hashtbl.create 51


let replace_directive_built_in_value k v = 
  Hashtbl.replace directive_built_in_values k v 

let remove_directive_built_in_value k  = 
  Hashtbl.replace directive_built_in_values k Dir_null

let replace_directive_int k v = 
  Hashtbl.replace directive_built_in_values k (Dir_int v)

let replace_directive_bool k v = 
  Hashtbl.replace directive_built_in_values k (Dir_bool v)

let replace_directive_string k v = 
  Hashtbl.replace directive_built_in_values k (Dir_string v)

let () =
  (* Note we use {!Config} instead of {!Sys} becasue 
     we want to overwrite in some cases with the 
     same stdlib
  *)
  let version = 
#if undefined BS_MIN_LEX_DEPS then 
    Config.version (* so that it can be overridden*)
#else 
    Sys.ocaml_version
#end
  in
  replace_directive_built_in_value "OCAML_VERSION" 
    (Dir_string version);
  replace_directive_built_in_value "OCAML_PATCH"
    (Dir_string 
       (match String.rindex version '+' with 
       | exception Not_found -> ""
       | i -> 
           String.sub version (i + 1)
             (String.length version - i - 1)))
  ;
  replace_directive_built_in_value "OS_TYPE" 
    (Dir_string Sys.os_type);
  replace_directive_built_in_value "BIG_ENDIAN" 
    (Dir_bool Sys.big_endian);
  replace_directive_built_in_value "WORD_SIZE" 
    (Dir_int Sys.word_size)

let find_directive_built_in_value k =
  Hashtbl.find directive_built_in_values k 

let iter_directive_built_in_value f = Hashtbl.iter f directive_built_in_values

(*
   {[
     # semver 0 "12";;
     - : int * int * int * string = (12, 0, 0, "");;
     # semver 0 "12.3";;
     - : int * int * int * string = (12, 3, 0, "");;
       semver 0 "12.3.10";;
     - : int * int * int * string = (12, 3, 10, "");;
     # semver 0 "12.3.10+x";;
     - : int * int * int * string = (12, 3, 10, "+x")
   ]}
*)    
let zero = Char.code '0' 
let dot = Char.code '.'
let semantic_version_parse str start  last_index = 
  let rec aux start  acc last_index =
    if start <= last_index then
      let c = Char.code (String.unsafe_get str start) in
      if c = dot then (acc, start + 1) (* consume [4.] instead of [4]*)
      else 
        let v =  c - zero in
        if v >=0 && v <= 9  then
          aux (start + 1) (acc * 10 + v) last_index
        else (acc , start)
    else (acc, start)
  in
  let major, major_end =  aux start 0 last_index  in
  let minor, minor_end = aux major_end 0 last_index in
  let patch, patch_end = aux minor_end 0 last_index in 
  let additional = String.sub str patch_end (last_index - patch_end  +1) in
  (major, minor, patch), additional

(** 
   {[
     semver Location.none "1.2.3" "~1.3.0" = false;;
     semver Location.none "1.2.3" "^1.3.0" = true ;;
     semver Location.none "1.2.3" ">1.3.0" = false ;;
     semver Location.none "1.2.3" ">=1.3.0" = false ;;
     semver Location.none "1.2.3" "<1.3.0" = true ;;
     semver Location.none "1.2.3" "<=1.3.0" = true ;;
   ]}
*)
let semver loc lhs str =
  let last_index = String.length str - 1 in 
  if last_index < 0 then raise (Error(Illegal_semver str, loc))
  else 
    let pred, ((major, minor,patch) as version, _) = 
      let v = String.unsafe_get str 0 in 
      match v with
      | '>' -> 
          if last_index = 0 then raise (Error(Illegal_semver str, loc)) else 
          if String.unsafe_get str 1 = '=' then 
            `Ge, semantic_version_parse str 2 last_index
          else `Gt, semantic_version_parse str 1 last_index
      | '<' 
        ->
          if last_index = 0 then raise (Error(Illegal_semver str, loc)) else 
          if String.unsafe_get str 1 = '=' then 
            `Le, semantic_version_parse str 2 last_index
          else `Lt, semantic_version_parse str 1 last_index
      | '^' 
        -> `Compatible, semantic_version_parse str 1 last_index
      | '~' ->  `Approximate, semantic_version_parse str 1 last_index
      | _ -> `Exact, semantic_version_parse str 0 last_index 
    in 
    let ((l_major, l_minor, l_patch) as lversion,_) =
      semantic_version_parse lhs 0 (String.length lhs - 1) in 
    match pred with 
    | `Ge -> lversion >= version 
    | `Gt -> lversion > version 
    | `Le -> lversion <= version
    | `Lt -> lversion < version 
    | `Approximate -> major = l_major && minor = l_minor 
    |  `Compatible -> major = l_major
    | `Exact -> lversion = version 


let pp_directive_value fmt (x : directive_value) =
  match x with
  | Dir_bool b -> Format.pp_print_bool fmt b
  | Dir_int b -> Format.pp_print_int fmt b
  | Dir_float b -> Format.pp_print_float fmt b
  | Dir_string s  -> Format.fprintf fmt "%S" s
  | Dir_null -> Format.pp_print_string fmt "null"    

let list_variables fmt = 
  iter_directive_built_in_value 
    (fun s  dir_value ->
       Format.fprintf
         fmt "@[%s@ %a@]@."
         s pp_directive_value dir_value
    )

let defined str =
  begin match  find_directive_built_in_value str with 
  |  Dir_null -> false 
  | _ ->  true
  | exception _ -> 
      try ignore @@ Sys.getenv str; true with _ ->  false 
  end

let query loc str =
  begin match find_directive_built_in_value str with
  | Dir_null -> Dir_bool false
  | v -> v
  | exception Not_found ->
      begin match Sys.getenv str with 
      | v -> 
          begin 
            try Dir_bool (bool_of_string v) with 
              _ -> 
                begin 
                  try Dir_int (int_of_string v )
                  with 
                    _ -> 
                      begin try (Dir_float (float_of_string v)) 
                      with _ -> Dir_string v
                      end
                end
          end
      | exception Not_found -> 
          Dir_bool false
      end
  end


let define_key_value key v  =
  if String.length key > 0
      && Char.uppercase (key.[0]) = key.[0] then 
    begin 
      replace_directive_built_in_value key
      begin
        (* NEED Sync up across {!lexer.mll} {!bspp.ml} and here,
           TODO: put it in {!lexer.mll}
        *)
        try Dir_bool (bool_of_string v) with 
          _ -> 
          begin 
            try Dir_int (int_of_string v )
            with 
              _ -> 
              begin try (Dir_float (float_of_string v)) 
                with _ -> Dir_string v
              end
          end
      end;
    true
    end
  else false 


let value_of_token loc (t : Parser.token)  = 
  match t with 
  | INT i -> Dir_int i 
  | STRING (s,_) -> Dir_string s 
  | FLOAT s  -> Dir_float (float_of_string s)
  | TRUE -> Dir_bool true
  | FALSE -> Dir_bool false
  | UIDENT s -> query loc s 
  | _ -> raise (Error (Unexpected_token_in_conditional, loc))


let directive_parse token_with_comments lexbuf =
  let look_ahead = ref None in
  let token () : Parser.token =
    let v = !look_ahead in
    match v with 
    | Some v -> 
        look_ahead := None ;
        v
    | None ->
       let rec skip () = 
        match token_with_comments lexbuf  with
        | COMMENT _ -> skip ()
#if undefined BS_MIN_LEX_DEPS then
        | DOCSTRING _ -> skip ()
#end
        | EOL -> skip ()
        | EOF -> raise (Error (Unterminated_if, Location.curr lexbuf)) 
        | t -> t 
        in  skip ()
  in
  let push e =
    (* INVARIANT: only look at most one token *)
    assert (!look_ahead = None);
    look_ahead := Some e 
  in
  let rec
    token_op calc   ~no  lhs   =
    match token () with 
    | (LESS 
    | GREATER 
    | INFIXOP0 "<=" 
    | INFIXOP0 ">=" 
    | EQUAL
    | INFIXOP0 "<>" as op) ->
        let f =  
          match op with 
          | LESS -> (<) 
          | GREATER -> (>)
          | INFIXOP0 "<=" -> (<=)
          | EQUAL -> (=)
          | INFIXOP0 "<>" -> (<>) 
          | _ -> assert false
        in 
        let curr_loc = Location.curr lexbuf in 
        let rhs = value_of_token curr_loc (token ()) in 
        not calc ||
        f lhs (assert_same_type lexbuf lhs rhs)
    | INFIXOP0 "=~" -> 
        not calc ||
        begin match lhs with 
        | Dir_string s ->
            let curr_loc = Location.curr lexbuf in 
            let rhs = value_of_token curr_loc (token ()) in 
            begin match rhs with 
            | Dir_string rhs -> 
                semver curr_loc s rhs
            | _ -> 
                raise
                  (Error
                     ( Conditional_expr_expected_type
                         (Dir_type_string, type_of_directive lhs), Location.curr lexbuf))
            end
        | _ -> raise
                 (Error
                    ( Conditional_expr_expected_type
                        (Dir_type_string, type_of_directive lhs), Location.curr lexbuf))
        end
    | e -> no e 
  and
    parse_or calc : bool =
    parse_or_aux calc (parse_and calc)
  and  (* a || (b || (c || d))*)
    parse_or_aux calc v : bool =
    (* let l = v  in *)
    match token () with
    | BARBAR ->
        let b =   parse_or (calc && not v)  in
        v || b 
    | e -> push e ; v
  and parse_and calc = 
    parse_and_aux calc (parse_relation calc)
  and parse_and_aux calc v = (* a && (b && (c && d)) *)
    (* let l = v  in *)
    match token () with
    | AMPERAMPER ->
        let b =  parse_and (calc && v) in
        v && b
    | e -> push e ; v
  and parse_relation (calc : bool) : bool  =
    let curr_token = token () in
    let curr_loc = Location.curr lexbuf in
    match curr_token with
    | TRUE -> true 
    | FALSE -> false
    | UIDENT v ->
        let value_v = query curr_loc v in
        token_op calc 
          ~no:(fun e -> push e ;
                match value_v with 
                | Dir_bool b -> b 
                | _ -> 
                    let ty = type_of_directive value_v in
                    raise
                      (Error(Conditional_expr_expected_type (Dir_type_bool, ty),
                             curr_loc)))
          value_v
    | INT v -> 
        token_op calc
          ~no:(fun e -> 
              raise(Error(Conditional_expr_expected_type(Dir_type_bool,Dir_type_int), 
                          curr_loc)))
          (Dir_int v)
    | FLOAT v -> 
        token_op calc
          ~no:(fun e -> 
              raise (Error(Conditional_expr_expected_type(Dir_type_bool, Dir_type_float),
                           curr_loc)))
          (Dir_float (float_of_string v))
    | STRING (v,_) -> 
        token_op calc
          ~no:(fun e ->
              raise (Error
                       (Conditional_expr_expected_type(Dir_type_bool, Dir_type_string),
                        curr_loc)))
          (Dir_string v)
    | LIDENT ("defined" | "undefined" as r) ->
        let t = token () in 
        let loc = Location.curr lexbuf in
        begin match t with
        | UIDENT s -> 
            not calc || 
            if r.[0] = 'u' then 
              not @@ defined s
            else defined s 
        | _ -> raise (Error (Unexpected_token_in_conditional, loc))
        end
    | LPAREN ->
        let v = parse_or calc in
        begin match token () with
        | RPAREN ->  v
        | _ -> raise (Error(Unterminated_paren_in_conditional, Location.curr lexbuf))
        end 

    | _ -> raise (Error (Unexpected_token_in_conditional, curr_loc))
  in
  let v = parse_or true in
  begin match token () with
  | THEN ->  v 
  | _ -> raise (Error (Expect_hash_then_in_conditional, Location.curr lexbuf))
  end


type dir_conditional =
  | Dir_if_true
  | Dir_if_false
  | Dir_out 

let string_of_dir_conditional (x : dir_conditional) =
  match x with 
  | Dir_if_true -> "Dir_if_true"
  | Dir_if_false -> "Dir_if_false"
  | Dir_out -> "Dir_out"

let is_elif (i : Parser.token ) =
  match i with
  | LIDENT "elif" -> true
  | _ -> false (* avoid polymorphic equal *)


(* The table of keywords *)

let keyword_table =
  create_hashtable 149 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "match", MATCH;
    "method", METHOD;
    "module", MODULE;
    "mutable", MUTABLE;
    "new", NEW;
    "nonrec", NONREC;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
(*  "parser", PARSER; *)
    "private", PRIVATE;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lor", INFIXOP3("lor");
    "lxor", INFIXOP3("lxor");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr")
]

(* To buffer string literals *)

let initial_string_buffer = Bytes.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= Bytes.length !string_buff then begin
    let new_buff = Bytes.create (Bytes.length (!string_buff) * 2) in
    Bytes.blit !string_buff 0 new_buff 0 (Bytes.length !string_buff);
    string_buff := new_buff
  end;
  Bytes.unsafe_set !string_buff !string_index c;
  incr string_index

let store_string s =
  for i = 0 to String.length s - 1 do
    store_string_char s.[i];
  done

let store_lexeme lexbuf =
  store_string (Lexing.lexeme lexbuf)

let get_stored_string () =
  let s = Bytes.sub_string !string_buff 0 !string_index in
  string_buff := initial_string_buffer;
  s

(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref Location.none;;
let comment_start_loc = ref [];;
let in_comment () = !comment_start_loc <> [];;
let is_in_string = ref false
let in_string () = !is_in_string
let print_warnings = ref true
let if_then_else = ref Dir_out
let sharp_look_ahead = ref None
let update_if_then_else v = 
  (* Format.fprintf Format.err_formatter "@[update %s \n@]@." (string_of_dir_conditional v); *)
  if_then_else := v
    
let with_comment_buffer comment lexbuf =
  let start_loc = Location.curr lexbuf  in
  comment_start_loc := [start_loc];
  reset_string_buffer ();
  let end_loc = comment lexbuf in
  let s = get_stored_string () in
  reset_string_buffer ();
  let loc = { start_loc with Location.loc_end = end_loc.Location.loc_end } in
  s, loc

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then
    if in_comment ()
    then 'x'
    else raise (Error(Illegal_escape (Lexing.lexeme lexbuf),
                      Location.curr lexbuf))
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

(* To convert integer literals, allowing max_int + 1 (PR#4210) *)

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0
                                                       (String.length s - 1)))

(* Remove underscores from float literals *)

let remove_underscores s =
  let l = String.length s in
  let b = Bytes.create l in
  let rec remove src dst =
    if src >= l then
      if dst >= l then s else Bytes.sub_string b 0 dst
    else
      match s.[src] with
        '_' -> remove (src + 1) dst
      |  c  -> Bytes.set b dst c; remove (src + 1) (dst + 1)
  in remove 0 0

(* recover the name from a LABEL or OPTLABEL token *)

let get_label_name lexbuf =
  let s = Lexing.lexeme lexbuf in
  let name = String.sub s 1 (String.length s - 2) in
  if Hashtbl.mem keyword_table name then
    raise (Error(Keyword_as_label name, Location.curr lexbuf));
  name
;;

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

let preprocessor = ref None

let escaped_newlines = ref false

(* Warn about Latin-1 characters used in idents *)

let warn_latin1 lexbuf =
  Location.prerr_warning (Location.curr lexbuf)
    (Warnings.Deprecated "ISO-Latin1 characters in identifiers")
;;

let comment_list = ref []

let add_comment com =
  comment_list := com :: !comment_list

let add_docstring_comment ds =
#if undefined BS_MIN_LEX_DEPS then
  let com = (Docstrings.docstring_body ds, Docstrings.docstring_loc ds) in
    add_comment com
#else 
  ()    
#end

let comments () = List.rev !comment_list

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment _ ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment (_, loc) ->
      fprintf ppf "This comment contains an unterminated string literal@.\
                   %aString literal begins here"
              Location.print_error loc
  | Keyword_as_label kwd ->
      fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
  | Literal_overflow ty ->
      fprintf ppf "Integer literal exceeds the range of representable \
                   integers of type %s" ty
  | Unterminated_if -> 
      fprintf ppf "#if not terminated"
  | Unterminated_else -> 
      fprintf ppf "#else not terminated"
  | Unexpected_directive -> fprintf ppf "Unexpected directive"
  | Unexpected_token_in_conditional -> 
      fprintf ppf "Unexpected token in conditional predicate"
  | Unterminated_paren_in_conditional ->
    fprintf ppf "Unterminated parens in conditional predicate"
  | Expect_hash_then_in_conditional -> 
      fprintf ppf "Expect `then` after conditional predicate"
  | Conditional_expr_expected_type (a,b) -> 
      fprintf ppf "Conditional expression type mismatch (%s,%s)" 
        (string_of_type_directive a )
        (string_of_type_directive b )
  | Illegal_semver s -> 
      fprintf ppf "Illegal semantic version string %s" s
let () =
  Location.register_error_of_exn
    (function
      | Error (err, loc) ->
          Some (Location.error_of_printer loc report_error err)
      | _ ->
          None
    )

}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let lowercase_latin1 = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase_latin1 = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar_latin1 =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule token = parse
  | "\\" newline {
      if not !escaped_newlines then
        raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     Location.curr lexbuf));
      update_loc lexbuf None 1 false 0;
      token lexbuf }
  | newline
      { update_loc lexbuf None 1 false 0;
        EOL }
  | blank +
      { token lexbuf }
  | "_"
      { UNDERSCORE }
  | "~"
      { TILDE }
  | "~" lowercase identchar * ':'
      { LABEL (get_label_name lexbuf) }
  | "~" lowercase_latin1 identchar_latin1 * ':'
      { warn_latin1 lexbuf; LABEL (get_label_name lexbuf) }
  | "?"
      { QUESTION }
  | "?" lowercase identchar * ':'
      { OPTLABEL (get_label_name lexbuf) }
  | "?" lowercase_latin1 identchar_latin1 * ':'
      { warn_latin1 lexbuf; OPTLABEL (get_label_name lexbuf) }
  | lowercase identchar *
      { let s = Lexing.lexeme lexbuf in
        try Hashtbl.find keyword_table s
        with Not_found -> LIDENT s }
  | lowercase_latin1 identchar_latin1 *
      { warn_latin1 lexbuf; LIDENT (Lexing.lexeme lexbuf) }
  | uppercase identchar *
      { UIDENT(Lexing.lexeme lexbuf) }       (* No capitalized keywords *)
  | uppercase_latin1 identchar_latin1 *
      { warn_latin1 lexbuf; UIDENT(Lexing.lexeme lexbuf) }
  | int_literal
      { try
          INT (cvt_int_literal (Lexing.lexeme lexbuf))
        with Failure _ ->
          raise (Error(Literal_overflow "int", Location.curr lexbuf))
      }
  | float_literal
      { FLOAT (remove_underscores(Lexing.lexeme lexbuf)) }
  | int_literal "l"
      { try
          INT32 (cvt_int32_literal (Lexing.lexeme lexbuf))
        with Failure _ ->
          raise (Error(Literal_overflow "int32", Location.curr lexbuf)) }
  | int_literal "L"
      { try
          INT64 (cvt_int64_literal (Lexing.lexeme lexbuf))
        with Failure _ ->
          raise (Error(Literal_overflow "int64", Location.curr lexbuf)) }
  | int_literal "n"
      { try
          NATIVEINT (cvt_nativeint_literal (Lexing.lexeme lexbuf))
        with Failure _ ->
          raise (Error(Literal_overflow "nativeint", Location.curr lexbuf)) }
  | "\""
      { reset_string_buffer();
        is_in_string := true;
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        string lexbuf;
        is_in_string := false;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string(), None) }
  | "{" lowercase* "|"
      { reset_string_buffer();
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim 1 (String.length delim - 2) in
        is_in_string := true;
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        quoted_string delim lexbuf;
        is_in_string := false;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string(), Some delim) }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        CHAR (Lexing.lexeme_char lexbuf 1) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
      { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR(char_for_decimal_code lexbuf 2) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { CHAR(char_for_hexadecimal_code lexbuf 3) }
  | "'\\" _
      { let l = Lexing.lexeme lexbuf in
        let esc = String.sub l 1 (String.length l - 1) in
        raise (Error(Illegal_escape esc, Location.curr lexbuf))
      }
  | "(*"
      { let s, loc = with_comment_buffer comment lexbuf in
        COMMENT (s, loc) }
  | "(**"
      { let s, loc = with_comment_buffer comment lexbuf in
#if undefined BS_MIN_LEX_DEPS then
        DOCSTRING (Docstrings.docstring s loc) 
#else
        COMMENT (s,loc)    
#end
}
  | "(**" ('*'+) as stars
      { let s, loc =
          with_comment_buffer
            (fun lexbuf ->
               store_string ("*" ^ stars);
               comment lexbuf)
            lexbuf
        in
        COMMENT (s, loc) }
  | "(*)"
      { if !print_warnings then
          Location.prerr_warning (Location.curr lexbuf) Warnings.Comment_start;
        let s, loc = with_comment_buffer comment lexbuf in
        COMMENT (s, loc) }
  | "(*" ('*'*) as stars "*)"
      { COMMENT (stars, Location.curr lexbuf) }
  | "*)"
      { let loc = Location.curr lexbuf in
        Location.prerr_warning loc Warnings.Comment_not_end;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
        STAR
      }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
      { update_loc lexbuf name (int_of_string num) true 0;
        token lexbuf
      }
  | "#"  { SHARP }
  | "&"  { AMPERSAND }
  | "&&" { AMPERAMPER }
  | "`"  { BACKQUOTE }
  | "'"  { QUOTE }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "*"  { STAR }
  | ","  { COMMA }
  | "->" { MINUSGREATER }
  | "."  { DOT }
  | ".." { DOTDOT }
  | ":"  { COLON }
  | "::" { COLONCOLON }
  | ":=" { COLONEQUAL }
  | ":>" { COLONGREATER }
  | ";"  { SEMI }
  | ";;" { SEMISEMI }
  | "<"  { LESS }
  | "<-" { LESSMINUS }
  | "="  { EQUAL }
  | "["  { LBRACKET }
  | "[|" { LBRACKETBAR }
  | "[<" { LBRACKETLESS }
  | "[>" { LBRACKETGREATER }
  | "]"  { RBRACKET }
  | "{"  { LBRACE }
  | "{<" { LBRACELESS }
  | "|"  { BAR }
  | "||" { BARBAR }
  | "|]" { BARRBRACKET }
  | ">"  { GREATER }
  | ">]" { GREATERRBRACKET }
  | "}"  { RBRACE }
  | ">}" { GREATERRBRACE }
  | "[@" { LBRACKETAT }
  | "[%" { LBRACKETPERCENT }
  | "[%%" { LBRACKETPERCENTPERCENT }
  | "[@@" { LBRACKETATAT }
  | "[@@@" { LBRACKETATATAT }
  | "!"  { BANG }
  | "!=" { INFIXOP0 "!=" }
  | "+"  { PLUS }
  | "+." { PLUSDOT }
  | "+=" { PLUSEQ }
  | "-"  { MINUS }
  | "-." { MINUSDOT }

  | "!" symbolchar +
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['~' '?'] symbolchar +
            { PREFIXOP(Lexing.lexeme lexbuf) }
  | ['=' '<' '>' '|' '&' '$'] symbolchar *
            { INFIXOP0(Lexing.lexeme lexbuf) }
  | ['@' '^'] symbolchar *
            { INFIXOP1(Lexing.lexeme lexbuf) }
  | ['+' '-'] symbolchar *
            { INFIXOP2(Lexing.lexeme lexbuf) }
  | "**" symbolchar *
            { INFIXOP4(Lexing.lexeme lexbuf) }
  | '%'     { PERCENT }
  | ['*' '/' '%'] symbolchar *
            { INFIXOP3(Lexing.lexeme lexbuf) }
  | '#' (symbolchar | '#') +
            { SHARPOP(Lexing.lexeme lexbuf) }
  | eof {
      if !if_then_else <> Dir_out then
        if !if_then_else = Dir_if_true then
          raise (Error (Unterminated_if, Location.curr lexbuf))
        else raise (Error(Unterminated_else, Location.curr lexbuf))
      else 
        EOF
        
    }
  | _
      { raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     Location.curr lexbuf))
      }

and comment = parse
    "(*"
      { comment_start_loc := (Location.curr lexbuf) :: !comment_start_loc;
        store_lexeme lexbuf;
        comment lexbuf;
      }
  | "*)"
      { match !comment_start_loc with
        | [] -> assert false
        | [_] -> comment_start_loc := []; Location.curr lexbuf
        | _ :: l -> comment_start_loc := l;
                  store_lexeme lexbuf;
                  comment lexbuf;
       }
  | "\""
      {
        string_start_loc := Location.curr lexbuf;
        store_string_char '"';
        is_in_string := true;
        begin try string lexbuf
        with Error (Unterminated_string, str_start) ->
          match !comment_start_loc with
          | [] -> assert false
          | loc :: _ ->
            let start = List.hd (List.rev !comment_start_loc) in
            comment_start_loc := [];
            raise (Error (Unterminated_string_in_comment (start, str_start),
                          loc))
        end;
        is_in_string := false;
        store_string_char '"';
        comment lexbuf }
  | "{" lowercase* "|"
      {
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim 1 (String.length delim - 2) in
        string_start_loc := Location.curr lexbuf;
        store_lexeme lexbuf;
        is_in_string := true;
        begin try quoted_string delim lexbuf
        with Error (Unterminated_string, str_start) ->
          match !comment_start_loc with
          | [] -> assert false
          | loc :: _ ->
            let start = List.hd (List.rev !comment_start_loc) in
            comment_start_loc := [];
            raise (Error (Unterminated_string_in_comment (start, str_start),
                          loc))
        end;
        is_in_string := false;
        store_string_char '|';
        store_string delim;
        store_string_char '}';
        comment lexbuf }

  | "''"
      { store_lexeme lexbuf; comment lexbuf }
  | "'" newline "'"
      { update_loc lexbuf None 1 false 1;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
      { store_lexeme lexbuf; comment lexbuf }
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
      { store_lexeme lexbuf; comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { store_lexeme lexbuf; comment lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { store_lexeme lexbuf; comment lexbuf }
  | eof
      { match !comment_start_loc with
        | [] -> assert false
        | loc :: _ ->
          let start = List.hd (List.rev !comment_start_loc) in
          comment_start_loc := [];
          raise (Error (Unterminated_comment start, loc))
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | _
      { store_lexeme lexbuf; comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' newline ([' ' '\t'] * as space)
      { update_loc lexbuf None 1 false (String.length space);
        string lexbuf
      }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { store_string_char(char_for_hexadecimal_code lexbuf 2);
         string lexbuf }
  | '\\' _
      { if in_comment ()
        then string lexbuf
        else begin
(*  Should be an error, but we are very lax.
          raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
                        Location.curr lexbuf))
*)
          let loc = Location.curr lexbuf in
          Location.prerr_warning loc Warnings.Illegal_backslash;
          store_string_char (Lexing.lexeme_char lexbuf 0);
          store_string_char (Lexing.lexeme_char lexbuf 1);
          string lexbuf
        end
      }
  | newline
      { if not (in_comment ()) then
          Location.prerr_warning (Location.curr lexbuf) Warnings.Eol_in_string;
        update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        string lexbuf
      }
  | eof
      { is_in_string := false;
        raise (Error (Unterminated_string, !string_start_loc)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and quoted_string delim = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        quoted_string delim lexbuf
      }
  | eof
      { is_in_string := false;
        raise (Error (Unterminated_string, !string_start_loc)) }
  | "|" lowercase* "}"
      {
        let edelim = Lexing.lexeme lexbuf in
        let edelim = String.sub edelim 1 (String.length edelim - 2) in
        if delim = edelim then ()
        else (store_lexeme lexbuf; quoted_string delim lexbuf)
      }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        quoted_string delim lexbuf }

and skip_sharp_bang = parse
  | "#!" [^ '\n']* '\n' [^ '\n']* "\n!#\n"
       { update_loc lexbuf None 3 false 0 }
  | "#!" [^ '\n']* '\n'
       { update_loc lexbuf None 1 false 0 }
  | "" { () }

{

  let at_bol lexbuf = 
    let pos = Lexing.lexeme_start_p lexbuf in 
    pos.pos_cnum = pos.pos_bol 

  let token_with_comments lexbuf =
    match !preprocessor with
    | None -> token lexbuf
    | Some (_init, preprocess) -> preprocess token lexbuf

  type newline_state =
    | NoLine (* There have been no blank lines yet. *)
    | NewLine
        (* There have been no blank lines, and the previous
           token was a newline. *)
    | BlankLine (* There have been blank lines. *)

  type doc_state =
    | Initial  (* There have been no docstrings yet *)
#if undefined BS_MIN_LEX_DEPS then
    | After of docstring list
        (* There have been docstrings, none of which were
           preceeded by a blank line *)
    | Before of docstring list * docstring list * docstring list
        (* There have been docstrings, some of which were
           preceeded by a blank line *)

  and docstring = Docstrings.docstring
#end
  let interpret_directive lexbuf cont look_ahead = 
    let if_then_else = !if_then_else in
    begin match token_with_comments lexbuf, if_then_else with 
    |  IF, Dir_out  ->
        let rec skip_from_if_false () = 
          let token = token_with_comments lexbuf in
          if token = EOF then 
            raise (Error (Unterminated_if, Location.curr lexbuf)) else
          if token = SHARP && at_bol lexbuf then 
            begin 
              let token = token_with_comments lexbuf in
              match token with
              | END -> 
                  begin
                    update_if_then_else Dir_out;
                    cont lexbuf
                  end
              | ELSE -> 
                  begin
                    update_if_then_else Dir_if_false;
                    cont lexbuf
                  end
              | IF ->
                  raise (Error (Unexpected_directive, Location.curr lexbuf))
              | _ -> 
                  if is_elif token &&
                     directive_parse token_with_comments lexbuf then
                    begin
                      update_if_then_else Dir_if_true;
                      cont lexbuf
                    end
                  else skip_from_if_false ()                               
            end
          else skip_from_if_false () in 
        if directive_parse token_with_comments lexbuf then
          begin 
            update_if_then_else Dir_if_true (* Next state: ELSE *);
            cont lexbuf
          end
        else
          skip_from_if_false ()
    | IF,  (Dir_if_false | Dir_if_true)->
        raise (Error(Unexpected_directive, Location.curr lexbuf))
    | LIDENT "elif", (Dir_if_false | Dir_out)
      -> (* when the predicate is false, it will continue eating `elif` *)
        raise (Error(Unexpected_directive, Location.curr lexbuf))
    | (LIDENT "elif" | ELSE as token), Dir_if_true ->           
        (* looking for #end, however, it can not see #if anymore *)
        let rec skip_from_if_true else_seen = 
          let token = token_with_comments lexbuf in
          if token = EOF then 
            raise (Error (Unterminated_else, Location.curr lexbuf)) else
          if token = SHARP && at_bol lexbuf then 
            begin 
              let token = token_with_comments lexbuf in 
              match token with  
              | END -> 
                  begin
                    update_if_then_else Dir_out;
                    cont lexbuf
                  end  
              | IF ->  
                  raise (Error (Unexpected_directive, Location.curr lexbuf)) 
              | ELSE ->
                  if else_seen then 
                    raise (Error (Unexpected_directive, Location.curr lexbuf))
                  else 
                    skip_from_if_true true
              | _ ->
                  if else_seen && is_elif token then  
                    raise (Error (Unexpected_directive, Location.curr lexbuf))
                  else 
                    skip_from_if_true else_seen
            end
          else skip_from_if_true else_seen in 
        skip_from_if_true (token = ELSE)
    | ELSE, Dir_if_false 
    | ELSE, Dir_out -> 
        raise (Error(Unexpected_directive, Location.curr lexbuf))
    | END, (Dir_if_false | Dir_if_true ) -> 
        update_if_then_else  Dir_out;
        cont lexbuf
    | END,  Dir_out  -> 
        raise (Error(Unexpected_directive, Location.curr lexbuf))
    | token, (Dir_if_true | Dir_if_false | Dir_out) ->
        look_ahead token 
    end

  let token lexbuf =
    let post_pos = lexeme_end_p lexbuf in
#if undefined BS_MIN_LEX_DEPS then
    let attach lines docs pre_pos =
      let open Docstrings in
        match docs, lines with
        | Initial, _ -> ()
        | After a, (NoLine | NewLine) ->
            set_post_docstrings post_pos (List.rev a);
            set_pre_docstrings pre_pos a;
        | After a, BlankLine ->
            set_post_docstrings post_pos (List.rev a);
            set_pre_extra_docstrings pre_pos (List.rev a)
        | Before(a, f, b), (NoLine | NewLine) ->
            set_post_docstrings post_pos (List.rev a);
            set_post_extra_docstrings post_pos
              (List.rev_append f (List.rev b));
            set_floating_docstrings pre_pos (List.rev f);
            set_pre_extra_docstrings pre_pos (List.rev a);
            set_pre_docstrings pre_pos b
        | Before(a, f, b), BlankLine ->
            set_post_docstrings post_pos (List.rev a);
            set_post_extra_docstrings post_pos
              (List.rev_append f (List.rev b));
            set_floating_docstrings pre_pos
              (List.rev_append f (List.rev b));
            set_pre_extra_docstrings pre_pos (List.rev a)
    in
#end
    let rec loop lines docs lexbuf : Parser.token =
      match token_with_comments lexbuf with
      | COMMENT (s, loc) ->
          add_comment (s, loc);
          let lines' =
            match lines with
            | NoLine -> NoLine
            | NewLine -> NoLine
            | BlankLine -> BlankLine
          in
          loop lines' docs lexbuf
      | EOL ->
          let lines' =
            match lines with
            | NoLine -> NewLine
            | NewLine -> BlankLine
            | BlankLine -> BlankLine
          in
          loop lines' docs lexbuf
      | SHARP when at_bol lexbuf -> 
          interpret_directive lexbuf 
            (fun lexbuf -> loop lines docs lexbuf)
            (fun token -> sharp_look_ahead := Some token; SHARP)
#if undefined BS_MIN_LEX_DEPS then
      | DOCSTRING doc ->
          add_docstring_comment doc;
          let docs' =
            match docs, lines with
            | Initial, (NoLine | NewLine) -> After [doc]
            | Initial, BlankLine -> Before([], [], [doc])
            | After a, (NoLine | NewLine) -> After (doc :: a)
            | After a, BlankLine -> Before (a, [], [doc])
            | Before(a, f, b), (NoLine | NewLine) -> Before(a, f, doc :: b)
            | Before(a, f, b), BlankLine -> Before(a, b @ f, [doc])
          in
          loop NoLine docs' lexbuf
#end
      | tok ->
#if undefined BS_MIN_LEX_DEPS then
          attach lines docs (lexeme_start_p lexbuf);
#end
          tok

          
    in
      match !sharp_look_ahead with
      | None -> 
           loop NoLine Initial lexbuf
      | Some token -> 
           sharp_look_ahead := None ;
           token

  let init () =
    sharp_look_ahead := None;
    update_if_then_else  Dir_out;
    is_in_string := false;
    comment_start_loc := [];
    comment_list := [];
    match !preprocessor with
    | None -> ()
    | Some (init, _preprocess) -> init ()

  let rec filter_directive pos   acc lexbuf : (int * int ) list =
    match token_with_comments lexbuf with
    | SHARP when at_bol lexbuf ->
        (* ^[start_pos]#if ... #then^[end_pos] *)
        let start_pos = Lexing.lexeme_start lexbuf in 
        interpret_directive lexbuf 
          (fun lexbuf -> 
             filter_directive 
               (Lexing.lexeme_end lexbuf)
               ((pos, start_pos) :: acc)
               lexbuf
          
          )
          (fun _token -> filter_directive pos acc lexbuf  )
    | EOF -> (pos, Lexing.lexeme_end lexbuf) :: acc
    | _ -> filter_directive pos  acc lexbuf

  let filter_directive_from_lexbuf lexbuf = 
    List.rev (filter_directive 0 [] lexbuf )

  let set_preprocessor init preprocess =
    escaped_newlines := true;
    preprocessor := Some (init, preprocess)

}
