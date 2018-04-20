# 15 "parsing/lexer.mll"
 
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


# 727 "parsing/lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\164\255\165\255\224\000\003\001\038\001\073\001\108\001\
    \143\001\188\255\178\001\215\001\196\255\091\000\252\001\031\002\
    \068\000\071\000\084\000\066\002\213\255\215\255\218\255\101\002\
    \196\002\231\002\089\000\255\000\005\003\236\255\082\003\115\003\
    \188\003\140\004\092\005\044\006\011\007\103\007\055\008\125\000\
    \254\255\001\000\005\000\255\255\006\000\007\000\022\009\052\009\
    \004\010\250\255\249\255\212\010\164\011\247\255\246\255\237\255\
    \238\255\239\255\093\000\118\002\091\000\110\000\231\002\007\004\
    \215\004\101\002\254\002\118\000\194\255\235\255\120\005\132\012\
    \096\000\113\000\011\000\234\255\233\255\229\255\229\004\128\000\
    \115\000\232\255\224\000\117\000\231\255\119\006\147\000\230\255\
    \146\000\225\255\148\000\224\255\217\000\132\012\223\255\171\012\
    \175\008\174\006\222\255\012\000\024\001\044\001\080\001\045\001\
    \222\255\013\000\217\012\000\013\035\013\073\013\210\255\206\255\
    \207\255\208\255\204\255\108\013\154\000\183\000\197\255\198\255\
    \199\255\199\000\182\255\184\255\191\255\143\013\187\255\189\255\
    \178\013\213\013\248\013\027\014\235\005\243\255\244\255\017\000\
    \245\255\062\002\172\007\253\255\223\000\241\000\255\255\254\255\
    \252\255\200\007\045\014\250\000\252\000\018\000\251\255\250\255\
    \249\255\128\009\030\003\003\001\248\255\092\003\004\001\247\255\
    \079\010\005\001\246\255\043\001\199\001\247\255\248\255\249\255\
    \059\001\118\014\255\255\250\255\031\011\036\004\253\255\038\001\
    \069\001\094\001\252\004\252\255\239\011\251\255\095\001\181\001\
    \252\255\238\006\254\255\255\255\111\001\112\001\253\255\074\007\
    \016\001\019\001\050\001\063\001\026\001\107\001\033\001\019\000\
    \255\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\088\000\087\000\084\000\083\000\076\000\
    \074\000\255\255\065\000\062\000\255\255\055\000\054\000\052\000\
    \050\000\046\000\044\000\079\000\255\255\255\255\255\255\035\000\
    \034\000\041\000\039\000\038\000\060\000\255\255\014\000\014\000\
    \013\000\012\000\011\000\010\000\007\000\004\000\003\000\002\000\
    \255\255\091\000\091\000\255\255\255\255\255\255\082\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\015\000\255\255\255\255\255\255\014\000\
    \014\000\014\000\015\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\026\000\026\000\
    \026\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \027\000\255\255\028\000\255\255\029\000\086\000\255\255\089\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\036\000\085\000\080\000\043\000\255\255\255\255\
    \255\255\255\255\255\255\053\000\070\000\069\000\255\255\255\255\
    \255\255\072\000\255\255\255\255\255\255\063\000\255\255\255\255\
    \081\000\075\000\078\000\077\000\255\255\255\255\255\255\012\000\
    \255\255\012\000\012\000\255\255\012\000\012\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \008\000\008\000\255\255\255\255\005\000\005\000\255\255\001\000\
    \005\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\003\000\255\255\255\255\003\000\255\255\255\255\255\255\
    \002\000\255\255\255\255\001\000\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\072\000\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\255\255\077\000\
    \255\255\255\255\255\255\000\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\255\255\255\255\000\000\255\255\255\255\000\000\
    \255\255\000\000\255\255\000\000\255\255\255\255\000\000\255\255\
    \100\000\255\255\000\000\255\255\100\000\101\000\100\000\103\000\
    \000\000\255\255\255\255\255\255\255\255\255\255\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\255\255\000\000\000\000\
    \255\255\255\255\255\255\255\255\133\000\000\000\000\000\255\255\
    \000\000\147\000\255\255\000\000\255\255\255\255\000\000\000\000\
    \000\000\255\255\255\255\255\255\255\255\255\255\000\000\000\000\
    \000\000\255\255\255\255\255\255\000\000\255\255\255\255\000\000\
    \255\255\255\255\000\000\255\255\165\000\000\000\000\000\000\000\
    \255\255\171\000\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\255\255\184\000\
    \000\000\255\255\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\194\000\197\000\255\255\197\000\255\255\255\255\
    \000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\039\000\040\000\040\000\039\000\041\000\045\000\043\000\
    \043\000\040\000\044\000\044\000\045\000\073\000\098\000\104\000\
    \074\000\099\000\105\000\134\000\148\000\200\000\163\000\149\000\
    \039\000\008\000\029\000\024\000\006\000\004\000\023\000\027\000\
    \026\000\021\000\025\000\007\000\020\000\019\000\018\000\003\000\
    \031\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\017\000\016\000\015\000\014\000\010\000\036\000\
    \005\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\013\000\042\000\012\000\005\000\038\000\
    \022\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\028\000\011\000\009\000\037\000\114\000\
    \116\000\113\000\110\000\088\000\112\000\111\000\039\000\076\000\
    \067\000\039\000\067\000\065\000\065\000\066\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\119\000\
    \075\000\118\000\081\000\117\000\084\000\039\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\087\000\089\000\090\000\091\000\092\000\123\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\120\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\121\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \002\000\003\000\091\000\092\000\003\000\003\000\003\000\122\000\
    \143\000\073\000\003\000\003\000\074\000\003\000\003\000\003\000\
    \083\000\083\000\083\000\083\000\083\000\083\000\083\000\083\000\
    \083\000\083\000\003\000\142\000\003\000\003\000\003\000\003\000\
    \003\000\152\000\098\000\151\000\003\000\099\000\255\255\003\000\
    \003\000\003\000\156\000\159\000\162\000\003\000\003\000\175\000\
    \003\000\003\000\003\000\193\000\194\000\134\000\098\000\104\000\
    \163\000\099\000\105\000\198\000\195\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\199\000\167\000\175\000\005\000\
    \182\000\196\000\005\000\005\000\005\000\000\000\103\000\175\000\
    \005\000\005\000\177\000\005\000\005\000\005\000\000\000\000\000\
    \000\000\102\000\098\000\071\000\003\000\099\000\003\000\000\000\
    \005\000\003\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \175\000\167\000\006\000\177\000\182\000\006\000\006\000\006\000\
    \102\000\000\000\101\000\006\000\006\000\196\000\006\000\006\000\
    \006\000\187\000\187\000\000\000\189\000\189\000\000\000\003\000\
    \000\000\003\000\000\000\006\000\005\000\006\000\006\000\006\000\
    \006\000\006\000\000\000\000\000\000\000\107\000\000\000\000\000\
    \107\000\107\000\107\000\000\000\000\000\000\000\107\000\107\000\
    \000\000\107\000\131\000\107\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\005\000\000\000\005\000\000\000\107\000\006\000\
    \107\000\130\000\107\000\107\000\107\000\000\000\000\000\000\000\
    \128\000\000\000\000\000\128\000\128\000\128\000\000\000\000\000\
    \000\000\128\000\128\000\000\000\128\000\128\000\128\000\187\000\
    \000\000\000\000\188\000\000\000\000\000\006\000\000\000\006\000\
    \000\000\128\000\107\000\128\000\129\000\128\000\128\000\128\000\
    \000\000\167\000\000\000\006\000\168\000\000\000\006\000\006\000\
    \006\000\000\000\000\000\000\000\006\000\006\000\000\000\006\000\
    \006\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \107\000\170\000\107\000\000\000\006\000\128\000\006\000\006\000\
    \006\000\006\000\006\000\000\000\000\000\000\000\000\000\000\000\
    \006\000\000\000\000\000\006\000\006\000\006\000\000\000\255\255\
    \000\000\006\000\006\000\000\000\006\000\006\000\006\000\000\000\
    \000\000\000\000\000\000\128\000\000\000\128\000\000\000\127\000\
    \006\000\006\000\000\000\006\000\006\000\006\000\006\000\006\000\
    \255\255\000\000\000\000\000\000\000\000\006\000\000\000\000\000\
    \006\000\006\000\006\000\169\000\000\000\000\000\006\000\006\000\
    \000\000\006\000\006\000\006\000\255\255\255\255\006\000\126\000\
    \006\000\185\000\255\255\000\000\124\000\006\000\006\000\000\000\
    \006\000\006\000\006\000\006\000\006\000\000\000\000\000\255\255\
    \006\000\000\000\000\000\006\000\006\000\006\000\000\000\000\000\
    \148\000\006\000\006\000\149\000\115\000\006\000\006\000\000\000\
    \255\255\000\000\000\000\125\000\000\000\006\000\000\000\000\000\
    \000\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \000\000\000\000\000\000\107\000\000\000\150\000\107\000\107\000\
    \107\000\000\000\000\000\255\255\107\000\107\000\000\000\107\000\
    \108\000\107\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \006\000\000\000\006\000\000\000\107\000\006\000\107\000\107\000\
    \109\000\107\000\107\000\000\000\000\000\000\000\006\000\000\000\
    \000\000\006\000\006\000\106\000\000\000\000\000\000\000\006\000\
    \006\000\000\000\006\000\006\000\006\000\065\000\065\000\000\000\
    \000\000\000\000\146\000\006\000\000\000\006\000\000\000\006\000\
    \107\000\006\000\006\000\006\000\006\000\006\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \000\000\056\000\000\000\000\000\000\000\186\000\000\000\000\000\
    \000\000\000\000\000\000\058\000\000\000\000\000\107\000\000\000\
    \107\000\000\000\000\000\006\000\065\000\000\000\000\000\166\000\
    \000\000\000\000\000\000\000\000\000\000\097\000\000\000\000\000\
    \000\000\057\000\000\000\055\000\000\000\059\000\000\000\000\000\
    \000\000\000\000\000\000\058\000\000\000\000\000\000\000\000\000\
    \000\000\006\000\000\000\006\000\097\000\095\000\000\000\095\000\
    \095\000\095\000\095\000\000\000\000\000\000\000\095\000\095\000\
    \000\000\095\000\095\000\095\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\095\000\000\000\
    \095\000\095\000\095\000\095\000\095\000\000\000\000\000\000\000\
    \003\000\000\000\000\000\003\000\003\000\003\000\000\000\000\000\
    \094\000\093\000\003\000\000\000\003\000\003\000\003\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\003\000\095\000\003\000\003\000\003\000\003\000\003\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \095\000\068\000\095\000\000\000\000\000\003\000\000\000\000\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\157\000\157\000\
    \157\000\157\000\157\000\157\000\157\000\157\000\157\000\157\000\
    \000\000\000\000\000\000\000\000\000\000\066\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\070\000\003\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \059\000\069\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\158\000\158\000\158\000\158\000\
    \158\000\158\000\158\000\158\000\158\000\158\000\000\000\058\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\056\000\000\000\
    \000\000\059\000\000\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\000\000\000\000\000\000\
    \000\000\030\000\000\000\000\000\000\000\060\000\000\000\058\000\
    \058\000\000\000\000\000\000\000\000\000\000\000\057\000\056\000\
    \055\000\000\000\061\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\062\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\030\000\000\000\000\000\060\000\000\000\000\000\
    \058\000\000\000\000\000\000\000\000\000\000\000\000\000\057\000\
    \000\000\055\000\061\000\032\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\062\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\000\000\
    \000\000\000\000\000\000\032\000\000\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\000\000\000\000\
    \000\000\000\000\000\000\056\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\063\000\000\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\000\000\000\000\
    \000\000\000\000\000\000\057\000\000\000\055\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\000\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\033\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\000\000\
    \000\000\000\000\000\000\033\000\000\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\000\000\
    \000\000\000\000\000\000\056\000\000\000\000\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\064\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\057\000\000\000\055\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\000\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\034\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\000\000\
    \000\000\000\000\000\000\034\000\000\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\070\000\
    \000\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\000\000\069\000\134\000\000\000\000\000\
    \135\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\139\000\000\000\000\000\
    \000\000\000\000\137\000\141\000\000\000\140\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\000\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\035\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\138\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\000\000\
    \000\000\000\000\000\000\035\000\000\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\000\000\000\000\000\000\000\000\000\000\000\000\097\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\097\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \000\000\000\000\000\000\136\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\000\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\000\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\046\000\000\000\000\000\046\000\
    \046\000\046\000\000\000\000\000\000\000\046\000\046\000\000\000\
    \046\000\046\000\046\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\046\000\000\000\046\000\
    \046\000\046\000\046\000\046\000\000\000\191\000\000\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\046\000\052\000\190\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\000\000\046\000\
    \046\000\046\000\000\000\046\000\046\000\046\000\000\000\000\000\
    \000\000\046\000\046\000\000\000\046\000\046\000\046\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\046\000\000\000\046\000\046\000\046\000\046\000\046\000\
    \000\000\191\000\000\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\046\000\048\000\190\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\000\000\046\000\000\000\046\000\000\000\000\000\
    \000\000\000\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\000\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\145\000\000\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \144\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\000\000\144\000\000\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\035\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\000\000\000\000\000\000\000\000\035\000\000\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \102\000\098\000\000\000\000\000\099\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\102\000\
    \000\000\101\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\000\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\000\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\046\000\
    \000\000\000\000\046\000\046\000\046\000\000\000\000\000\000\000\
    \046\000\046\000\000\000\046\000\046\000\046\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \046\000\000\000\046\000\046\000\046\000\046\000\046\000\000\000\
    \000\000\000\000\000\000\047\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\050\000\000\000\
    \000\000\000\000\000\000\000\000\046\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\000\000\
    \000\000\000\000\046\000\047\000\046\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\255\255\
    \160\000\160\000\160\000\160\000\160\000\160\000\160\000\160\000\
    \160\000\160\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\160\000\160\000\160\000\160\000\160\000\160\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\160\000\160\000\160\000\160\000\160\000\160\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\000\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\048\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\049\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\000\000\
    \000\000\000\000\000\000\048\000\000\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\161\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\161\000\161\000\
    \161\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \161\000\161\000\161\000\161\000\161\000\161\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\000\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\051\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\054\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\000\000\
    \000\000\000\000\000\000\051\000\000\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\000\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\052\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\053\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\000\000\
    \000\000\000\000\000\000\052\000\000\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\000\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\000\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\080\000\093\000\080\000\000\000\
    \093\000\093\000\093\000\080\000\000\000\000\000\093\000\093\000\
    \000\000\093\000\093\000\093\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\093\000\000\000\
    \093\000\093\000\093\000\093\000\093\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\095\000\000\000\095\000\095\000\
    \095\000\095\000\000\000\000\000\000\000\095\000\095\000\000\000\
    \095\000\095\000\095\000\000\000\000\000\000\000\000\000\000\000\
    \080\000\000\000\093\000\000\000\000\000\095\000\080\000\095\000\
    \095\000\095\000\095\000\095\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\080\000\000\000\000\000\000\000\080\000\000\000\
    \080\000\000\000\006\000\000\000\078\000\006\000\006\000\006\000\
    \093\000\000\000\093\000\006\000\006\000\000\000\006\000\006\000\
    \006\000\095\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\000\000\006\000\006\000\006\000\
    \006\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\107\000\000\000\000\000\107\000\107\000\107\000\095\000\
    \000\000\095\000\107\000\107\000\000\000\107\000\107\000\107\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
    \000\000\000\000\107\000\000\000\107\000\107\000\107\000\107\000\
    \107\000\000\000\000\000\000\000\107\000\000\000\000\000\107\000\
    \107\000\107\000\000\000\000\000\000\000\107\000\107\000\000\000\
    \107\000\107\000\107\000\000\000\000\000\006\000\000\000\006\000\
    \000\000\000\000\000\000\000\000\000\000\107\000\107\000\107\000\
    \107\000\107\000\107\000\107\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\107\000\000\000\000\000\107\000\107\000\107\000\
    \000\000\000\000\000\000\107\000\107\000\000\000\107\000\107\000\
    \107\000\000\000\000\000\000\000\107\000\000\000\107\000\000\000\
    \000\000\107\000\000\000\107\000\255\255\107\000\107\000\107\000\
    \107\000\107\000\000\000\000\000\000\000\006\000\000\000\000\000\
    \006\000\006\000\006\000\000\000\000\000\000\000\006\000\006\000\
    \000\000\006\000\006\000\006\000\000\000\000\000\000\000\107\000\
    \000\000\107\000\000\000\000\000\000\000\000\000\006\000\107\000\
    \006\000\006\000\006\000\006\000\006\000\000\000\000\000\000\000\
    \006\000\000\000\000\000\006\000\006\000\006\000\000\000\000\000\
    \000\000\006\000\006\000\000\000\006\000\006\000\006\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\107\000\000\000\107\000\
    \000\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \000\000\000\000\000\000\128\000\000\000\000\000\128\000\128\000\
    \128\000\000\000\000\000\000\000\128\000\128\000\000\000\128\000\
    \128\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \006\000\000\000\006\000\000\000\128\000\006\000\128\000\128\000\
    \128\000\128\000\128\000\000\000\000\000\000\000\128\000\000\000\
    \000\000\128\000\128\000\128\000\000\000\000\000\000\000\128\000\
    \128\000\000\000\128\000\128\000\128\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\000\000\006\000\000\000\128\000\
    \128\000\128\000\128\000\128\000\128\000\128\000\000\000\000\000\
    \000\000\107\000\000\000\000\000\107\000\107\000\107\000\000\000\
    \000\000\000\000\107\000\107\000\000\000\107\000\107\000\107\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\
    \128\000\000\000\107\000\128\000\107\000\107\000\107\000\107\000\
    \107\000\000\000\000\000\000\000\107\000\000\000\000\000\107\000\
    \107\000\107\000\000\000\000\000\000\000\107\000\107\000\000\000\
    \107\000\107\000\107\000\000\000\000\000\155\000\000\000\155\000\
    \000\000\128\000\000\000\128\000\155\000\107\000\107\000\107\000\
    \107\000\107\000\107\000\107\000\000\000\154\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\107\000\000\000\107\000\000\000\
    \000\000\107\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \175\000\000\000\000\000\176\000\000\000\000\000\000\000\000\000\
    \000\000\155\000\000\000\000\000\000\000\000\000\000\000\155\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\174\000\107\000\
    \174\000\107\000\000\000\155\000\000\000\174\000\000\000\155\000\
    \000\000\155\000\000\000\000\000\000\000\153\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\174\000\000\000\000\000\000\000\000\000\000\000\
    \174\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\174\000\000\000\000\000\000\000\
    \174\000\000\000\174\000\000\000\000\000\000\000\172\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\041\000\000\000\000\000\041\000\042\000\
    \044\000\045\000\042\000\044\000\045\000\074\000\099\000\105\000\
    \074\000\099\000\105\000\135\000\149\000\199\000\135\000\149\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\
    \013\000\017\000\018\000\026\000\017\000\017\000\039\000\072\000\
    \058\000\039\000\058\000\060\000\060\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\013\000\
    \073\000\013\000\080\000\013\000\083\000\039\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\086\000\088\000\088\000\090\000\090\000\116\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\117\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\092\000\092\000\003\000\003\000\003\000\121\000\
    \140\000\027\000\003\000\003\000\027\000\003\000\003\000\003\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\003\000\141\000\003\000\003\000\003\000\003\000\
    \003\000\147\000\100\000\148\000\004\000\100\000\027\000\004\000\
    \004\000\004\000\155\000\158\000\161\000\004\000\004\000\175\000\
    \004\000\004\000\004\000\192\000\193\000\163\000\101\000\103\000\
    \163\000\101\000\103\000\196\000\194\000\004\000\003\000\004\000\
    \004\000\004\000\004\000\004\000\198\000\168\000\175\000\005\000\
    \168\000\195\000\005\000\005\000\005\000\255\255\101\000\176\000\
    \005\000\005\000\176\000\005\000\005\000\005\000\255\255\255\255\
    \255\255\102\000\102\000\027\000\003\000\102\000\003\000\255\255\
    \005\000\004\000\005\000\005\000\005\000\005\000\005\000\255\255\
    \177\000\182\000\006\000\177\000\182\000\006\000\006\000\006\000\
    \102\000\255\255\102\000\006\000\006\000\197\000\006\000\006\000\
    \006\000\188\000\189\000\255\255\188\000\189\000\255\255\004\000\
    \255\255\004\000\255\255\006\000\005\000\006\000\006\000\006\000\
    \006\000\006\000\255\255\255\255\255\255\007\000\255\255\255\255\
    \007\000\007\000\007\000\255\255\255\255\255\255\007\000\007\000\
    \255\255\007\000\007\000\007\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\005\000\255\255\005\000\255\255\007\000\006\000\
    \007\000\007\000\007\000\007\000\007\000\255\255\255\255\255\255\
    \008\000\255\255\255\255\008\000\008\000\008\000\255\255\255\255\
    \255\255\008\000\008\000\255\255\008\000\008\000\008\000\183\000\
    \255\255\255\255\183\000\255\255\255\255\006\000\255\255\006\000\
    \255\255\008\000\007\000\008\000\008\000\008\000\008\000\008\000\
    \255\255\164\000\255\255\010\000\164\000\255\255\010\000\010\000\
    \010\000\255\255\255\255\255\255\010\000\010\000\255\255\010\000\
    \010\000\010\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \007\000\164\000\007\000\255\255\010\000\008\000\010\000\010\000\
    \010\000\010\000\010\000\255\255\255\255\255\255\255\255\255\255\
    \011\000\255\255\255\255\011\000\011\000\011\000\255\255\027\000\
    \255\255\011\000\011\000\255\255\011\000\011\000\011\000\255\255\
    \255\255\255\255\255\255\008\000\255\255\008\000\255\255\010\000\
    \010\000\011\000\255\255\011\000\011\000\011\000\011\000\011\000\
    \100\000\255\255\255\255\255\255\255\255\014\000\255\255\255\255\
    \014\000\014\000\014\000\164\000\255\255\255\255\014\000\014\000\
    \255\255\014\000\014\000\014\000\101\000\103\000\010\000\010\000\
    \010\000\183\000\194\000\255\255\011\000\011\000\014\000\255\255\
    \014\000\014\000\014\000\014\000\014\000\255\255\255\255\195\000\
    \015\000\255\255\255\255\015\000\015\000\015\000\255\255\255\255\
    \137\000\015\000\015\000\137\000\015\000\015\000\015\000\255\255\
    \102\000\255\255\255\255\011\000\255\255\011\000\255\255\255\255\
    \255\255\015\000\014\000\015\000\015\000\015\000\015\000\015\000\
    \255\255\255\255\255\255\019\000\255\255\137\000\019\000\019\000\
    \019\000\255\255\255\255\197\000\019\000\019\000\255\255\019\000\
    \019\000\019\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \014\000\255\255\014\000\255\255\019\000\015\000\019\000\019\000\
    \019\000\019\000\019\000\255\255\255\255\255\255\023\000\255\255\
    \255\255\023\000\023\000\023\000\255\255\255\255\255\255\023\000\
    \023\000\255\255\023\000\023\000\023\000\065\000\065\000\255\255\
    \255\255\255\255\137\000\015\000\255\255\015\000\255\255\023\000\
    \019\000\023\000\023\000\023\000\023\000\023\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \255\255\065\000\255\255\255\255\255\255\183\000\255\255\255\255\
    \255\255\255\255\255\255\059\000\255\255\255\255\019\000\255\255\
    \019\000\255\255\255\255\023\000\065\000\255\255\255\255\164\000\
    \255\255\255\255\255\255\255\255\255\255\024\000\255\255\255\255\
    \255\255\065\000\255\255\065\000\255\255\059\000\255\255\255\255\
    \255\255\255\255\255\255\059\000\255\255\255\255\255\255\255\255\
    \255\255\023\000\255\255\023\000\024\000\024\000\255\255\024\000\
    \024\000\024\000\024\000\255\255\255\255\255\255\024\000\024\000\
    \255\255\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\255\255\
    \024\000\024\000\024\000\024\000\024\000\255\255\255\255\255\255\
    \025\000\255\255\255\255\025\000\025\000\025\000\255\255\255\255\
    \025\000\025\000\025\000\255\255\025\000\025\000\025\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\025\000\024\000\025\000\025\000\025\000\025\000\025\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\066\000\066\000\
    \066\000\066\000\066\000\066\000\066\000\066\000\066\000\066\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\137\000\255\255\
    \024\000\028\000\024\000\255\255\255\255\025\000\255\255\255\255\
    \062\000\062\000\062\000\062\000\062\000\062\000\154\000\154\000\
    \154\000\154\000\154\000\154\000\154\000\154\000\154\000\154\000\
    \255\255\255\255\255\255\255\255\255\255\066\000\255\255\255\255\
    \255\255\255\255\255\255\025\000\028\000\025\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \030\000\028\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\157\000\157\000\157\000\157\000\
    \157\000\157\000\157\000\157\000\157\000\157\000\255\255\030\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\030\000\255\255\
    \255\255\031\000\255\255\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\255\255\255\255\255\255\
    \255\255\030\000\255\255\255\255\255\255\031\000\255\255\030\000\
    \031\000\255\255\255\255\255\255\255\255\255\255\030\000\031\000\
    \030\000\255\255\031\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\031\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\031\000\255\255\255\255\031\000\255\255\255\255\
    \031\000\255\255\255\255\255\255\255\255\255\255\255\255\031\000\
    \255\255\031\000\031\000\032\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\031\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\255\255\
    \255\255\255\255\255\255\032\000\255\255\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\063\000\
    \063\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
    \063\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \063\000\063\000\063\000\063\000\063\000\063\000\255\255\255\255\
    \255\255\255\255\255\255\063\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\063\000\255\255\
    \063\000\063\000\063\000\063\000\063\000\063\000\255\255\255\255\
    \255\255\255\255\255\255\063\000\255\255\063\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\255\255\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\033\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\255\255\
    \255\255\255\255\255\255\033\000\255\255\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\078\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\255\255\
    \255\255\255\255\255\255\064\000\255\255\255\255\078\000\078\000\
    \078\000\078\000\078\000\078\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\064\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\064\000\255\255\064\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\255\255\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\034\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\255\255\
    \255\255\255\255\255\255\034\000\255\255\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\070\000\
    \255\255\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\255\255\070\000\132\000\255\255\255\255\
    \132\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\132\000\255\255\255\255\
    \255\255\255\255\132\000\132\000\255\255\132\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\255\255\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\035\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\132\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\255\255\
    \255\255\255\255\255\255\035\000\255\255\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\255\255\255\255\255\255\255\255\255\255\255\255\097\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\097\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \085\000\085\000\085\000\085\000\085\000\085\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \255\255\255\255\255\255\132\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\255\255\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\255\255\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\036\000\255\255\255\255\036\000\
    \036\000\036\000\255\255\255\255\255\255\036\000\036\000\255\255\
    \036\000\036\000\036\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\036\000\255\255\036\000\
    \036\000\036\000\036\000\036\000\255\255\185\000\255\255\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\185\000\185\000\185\000\
    \185\000\036\000\036\000\185\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\255\255\036\000\
    \037\000\036\000\255\255\037\000\037\000\037\000\255\255\255\255\
    \255\255\037\000\037\000\255\255\037\000\037\000\037\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\037\000\255\255\037\000\037\000\037\000\037\000\037\000\
    \255\255\191\000\255\255\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\191\000\037\000\037\000\191\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\255\255\037\000\255\255\037\000\255\255\255\255\
    \255\255\255\255\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\255\255\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\138\000\255\255\138\000\138\000\138\000\
    \138\000\138\000\138\000\138\000\138\000\138\000\138\000\138\000\
    \138\000\138\000\138\000\138\000\138\000\138\000\138\000\138\000\
    \138\000\138\000\138\000\138\000\138\000\138\000\138\000\145\000\
    \138\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\145\000\145\000\145\000\145\000\145\000\
    \145\000\145\000\145\000\255\255\145\000\255\255\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\038\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\255\255\255\255\255\255\255\255\038\000\255\255\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \096\000\096\000\255\255\255\255\096\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\096\000\
    \255\255\096\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\255\255\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\255\255\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\046\000\
    \255\255\255\255\046\000\046\000\046\000\255\255\255\255\255\255\
    \046\000\046\000\255\255\046\000\046\000\046\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \046\000\255\255\046\000\046\000\046\000\046\000\046\000\255\255\
    \255\255\255\255\255\255\047\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\255\255\
    \255\255\255\255\255\255\255\255\046\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\255\255\
    \255\255\255\255\046\000\047\000\046\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\096\000\
    \153\000\153\000\153\000\153\000\153\000\153\000\153\000\153\000\
    \153\000\153\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\153\000\153\000\153\000\153\000\153\000\153\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\153\000\153\000\153\000\153\000\153\000\153\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\255\255\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\048\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\255\255\
    \255\255\255\255\255\255\048\000\255\255\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\160\000\
    \160\000\160\000\160\000\160\000\160\000\160\000\160\000\160\000\
    \160\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \160\000\160\000\160\000\160\000\160\000\160\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \160\000\160\000\160\000\160\000\160\000\160\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\255\255\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\051\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\255\255\
    \255\255\255\255\255\255\051\000\255\255\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\172\000\
    \172\000\172\000\172\000\172\000\172\000\172\000\172\000\172\000\
    \172\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \172\000\172\000\172\000\172\000\172\000\172\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \172\000\172\000\172\000\172\000\172\000\172\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\255\255\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\052\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\255\255\
    \255\255\255\255\255\255\052\000\255\255\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \180\000\180\000\180\000\180\000\180\000\180\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \180\000\180\000\180\000\180\000\180\000\180\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\255\255\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\255\255\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\071\000\093\000\071\000\255\255\
    \093\000\093\000\093\000\071\000\255\255\255\255\093\000\093\000\
    \255\255\093\000\093\000\093\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\093\000\255\255\
    \093\000\093\000\093\000\093\000\093\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\095\000\255\255\095\000\095\000\
    \095\000\095\000\255\255\255\255\255\255\095\000\095\000\255\255\
    \095\000\095\000\095\000\255\255\255\255\255\255\255\255\255\255\
    \071\000\255\255\093\000\255\255\255\255\095\000\071\000\095\000\
    \095\000\095\000\095\000\095\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\071\000\255\255\255\255\255\255\071\000\255\255\
    \071\000\255\255\106\000\255\255\071\000\106\000\106\000\106\000\
    \093\000\255\255\093\000\106\000\106\000\255\255\106\000\106\000\
    \106\000\095\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\106\000\255\255\106\000\106\000\106\000\
    \106\000\106\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\107\000\255\255\255\255\107\000\107\000\107\000\095\000\
    \255\255\095\000\107\000\107\000\255\255\107\000\107\000\107\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\106\000\
    \255\255\255\255\107\000\255\255\107\000\107\000\107\000\107\000\
    \107\000\255\255\255\255\255\255\108\000\255\255\255\255\108\000\
    \108\000\108\000\255\255\255\255\255\255\108\000\108\000\255\255\
    \108\000\108\000\108\000\255\255\255\255\106\000\255\255\106\000\
    \255\255\255\255\255\255\255\255\255\255\108\000\107\000\108\000\
    \108\000\108\000\108\000\108\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\109\000\255\255\255\255\109\000\109\000\109\000\
    \255\255\255\255\255\255\109\000\109\000\255\255\109\000\109\000\
    \109\000\255\255\255\255\255\255\107\000\255\255\107\000\255\255\
    \255\255\108\000\255\255\109\000\071\000\109\000\109\000\109\000\
    \109\000\109\000\255\255\255\255\255\255\115\000\255\255\255\255\
    \115\000\115\000\115\000\255\255\255\255\255\255\115\000\115\000\
    \255\255\115\000\115\000\115\000\255\255\255\255\255\255\108\000\
    \255\255\108\000\255\255\255\255\255\255\255\255\115\000\109\000\
    \115\000\115\000\115\000\115\000\115\000\255\255\255\255\255\255\
    \125\000\255\255\255\255\125\000\125\000\125\000\255\255\255\255\
    \255\255\125\000\125\000\255\255\125\000\125\000\125\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\109\000\255\255\109\000\
    \255\255\125\000\115\000\125\000\125\000\125\000\125\000\125\000\
    \255\255\255\255\255\255\128\000\255\255\255\255\128\000\128\000\
    \128\000\255\255\255\255\255\255\128\000\128\000\255\255\128\000\
    \128\000\128\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \115\000\255\255\115\000\255\255\128\000\125\000\128\000\128\000\
    \128\000\128\000\128\000\255\255\255\255\255\255\129\000\255\255\
    \255\255\129\000\129\000\129\000\255\255\255\255\255\255\129\000\
    \129\000\255\255\129\000\129\000\129\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\125\000\255\255\125\000\255\255\129\000\
    \128\000\129\000\129\000\129\000\129\000\129\000\255\255\255\255\
    \255\255\130\000\255\255\255\255\130\000\130\000\130\000\255\255\
    \255\255\255\255\130\000\130\000\255\255\130\000\130\000\130\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\128\000\255\255\
    \128\000\255\255\130\000\129\000\130\000\130\000\130\000\130\000\
    \130\000\255\255\255\255\255\255\131\000\255\255\255\255\131\000\
    \131\000\131\000\255\255\255\255\255\255\131\000\131\000\255\255\
    \131\000\131\000\131\000\255\255\255\255\146\000\255\255\146\000\
    \255\255\129\000\255\255\129\000\146\000\131\000\130\000\131\000\
    \131\000\131\000\131\000\131\000\255\255\146\000\146\000\146\000\
    \146\000\146\000\146\000\146\000\146\000\146\000\146\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\130\000\255\255\130\000\255\255\
    \255\255\131\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \169\000\255\255\255\255\169\000\255\255\255\255\255\255\255\255\
    \255\255\146\000\255\255\255\255\255\255\255\255\255\255\146\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\169\000\131\000\
    \169\000\131\000\255\255\146\000\255\255\169\000\255\255\146\000\
    \255\255\146\000\255\255\255\255\255\255\146\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\169\000\255\255\255\255\255\255\255\255\255\255\
    \169\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\169\000\255\255\255\255\255\255\
    \169\000\255\255\169\000\255\255\255\255\255\255\169\000\255\255\
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
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\169\000";
  Lexing.lex_base_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \010\000\036\000\012\000\000\000\000\000\000\000\002\000\000\000\
    \027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_backtrk_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\039\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_default_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_trans_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\000\000\036\000\036\000\000\000\036\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\000\000\001\000\022\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\007\000\001\000\000\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\001\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check_code = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\024\000\101\000\169\000\176\000\101\000\177\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \024\000\255\255\101\000\000\000\102\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\096\000\097\000\255\255\255\255\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\097\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\255\255\255\255\
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
    \101\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_code = 
   "\255\004\255\255\005\255\255\007\255\006\255\255\003\255\000\004\
    \001\005\255\007\255\255\006\255\007\255\255\000\004\001\005\003\
    \006\002\007\255\001\255\255\000\001\255";
}

let rec token lexbuf =
  lexbuf.Lexing.lex_mem <- Array.make 8 (-1) ;   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 767 "parsing/lexer.mll"
                 (
      if not !escaped_newlines then
        raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     Location.curr lexbuf));
      update_loc lexbuf None 1 false 0;
      token lexbuf )
# 1977 "parsing/lexer.ml"

  | 1 ->
# 774 "parsing/lexer.mll"
      ( update_loc lexbuf None 1 false 0;
        EOL )
# 1983 "parsing/lexer.ml"

  | 2 ->
# 777 "parsing/lexer.mll"
      ( token lexbuf )
# 1988 "parsing/lexer.ml"

  | 3 ->
# 779 "parsing/lexer.mll"
      ( UNDERSCORE )
# 1993 "parsing/lexer.ml"

  | 4 ->
# 781 "parsing/lexer.mll"
      ( TILDE )
# 1998 "parsing/lexer.ml"

  | 5 ->
# 783 "parsing/lexer.mll"
      ( LABEL (get_label_name lexbuf) )
# 2003 "parsing/lexer.ml"

  | 6 ->
# 785 "parsing/lexer.mll"
      ( warn_latin1 lexbuf; LABEL (get_label_name lexbuf) )
# 2008 "parsing/lexer.ml"

  | 7 ->
# 787 "parsing/lexer.mll"
      ( QUESTION )
# 2013 "parsing/lexer.ml"

  | 8 ->
# 789 "parsing/lexer.mll"
      ( OPTLABEL (get_label_name lexbuf) )
# 2018 "parsing/lexer.ml"

  | 9 ->
# 791 "parsing/lexer.mll"
      ( warn_latin1 lexbuf; OPTLABEL (get_label_name lexbuf) )
# 2023 "parsing/lexer.ml"

  | 10 ->
# 793 "parsing/lexer.mll"
      ( let s = Lexing.lexeme lexbuf in
        try Hashtbl.find keyword_table s
        with Not_found -> LIDENT s )
# 2030 "parsing/lexer.ml"

  | 11 ->
# 797 "parsing/lexer.mll"
      ( warn_latin1 lexbuf; LIDENT (Lexing.lexeme lexbuf) )
# 2035 "parsing/lexer.ml"

  | 12 ->
# 799 "parsing/lexer.mll"
      ( UIDENT(Lexing.lexeme lexbuf) )
# 2040 "parsing/lexer.ml"

  | 13 ->
# 801 "parsing/lexer.mll"
      ( warn_latin1 lexbuf; UIDENT(Lexing.lexeme lexbuf) )
# 2045 "parsing/lexer.ml"

  | 14 ->
# 803 "parsing/lexer.mll"
      ( try
          INT (cvt_int_literal (Lexing.lexeme lexbuf))
        with Failure _ ->
          raise (Error(Literal_overflow "int", Location.curr lexbuf))
      )
# 2054 "parsing/lexer.ml"

  | 15 ->
# 809 "parsing/lexer.mll"
      ( FLOAT (remove_underscores(Lexing.lexeme lexbuf)) )
# 2059 "parsing/lexer.ml"

  | 16 ->
# 811 "parsing/lexer.mll"
      ( try
          INT32 (cvt_int32_literal (Lexing.lexeme lexbuf))
        with Failure _ ->
          raise (Error(Literal_overflow "int32", Location.curr lexbuf)) )
# 2067 "parsing/lexer.ml"

  | 17 ->
# 816 "parsing/lexer.mll"
      ( try
          INT64 (cvt_int64_literal (Lexing.lexeme lexbuf))
        with Failure _ ->
          raise (Error(Literal_overflow "int64", Location.curr lexbuf)) )
# 2075 "parsing/lexer.ml"

  | 18 ->
# 821 "parsing/lexer.mll"
      ( try
          NATIVEINT (cvt_nativeint_literal (Lexing.lexeme lexbuf))
        with Failure _ ->
          raise (Error(Literal_overflow "nativeint", Location.curr lexbuf)) )
# 2083 "parsing/lexer.ml"

  | 19 ->
# 826 "parsing/lexer.mll"
      ( reset_string_buffer();
        is_in_string := true;
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        string lexbuf;
        is_in_string := false;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string(), None) )
# 2095 "parsing/lexer.ml"

  | 20 ->
# 835 "parsing/lexer.mll"
      ( reset_string_buffer();
        let delim = Lexing.lexeme lexbuf in
        let delim = String.sub delim 1 (String.length delim - 2) in
        is_in_string := true;
        let string_start = lexbuf.lex_start_p in
        string_start_loc := Location.curr lexbuf;
        quoted_string delim lexbuf;
        is_in_string := false;
        lexbuf.lex_start_p <- string_start;
        STRING (get_stored_string(), Some delim) )
# 2109 "parsing/lexer.ml"

  | 21 ->
# 846 "parsing/lexer.mll"
      ( update_loc lexbuf None 1 false 1;
        CHAR (Lexing.lexeme_char lexbuf 1) )
# 2115 "parsing/lexer.ml"

  | 22 ->
# 849 "parsing/lexer.mll"
      ( CHAR(Lexing.lexeme_char lexbuf 1) )
# 2120 "parsing/lexer.ml"

  | 23 ->
# 851 "parsing/lexer.mll"
      ( CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) )
# 2125 "parsing/lexer.ml"

  | 24 ->
# 853 "parsing/lexer.mll"
      ( CHAR(char_for_decimal_code lexbuf 2) )
# 2130 "parsing/lexer.ml"

  | 25 ->
# 855 "parsing/lexer.mll"
      ( CHAR(char_for_hexadecimal_code lexbuf 3) )
# 2135 "parsing/lexer.ml"

  | 26 ->
# 857 "parsing/lexer.mll"
      ( let l = Lexing.lexeme lexbuf in
        let esc = String.sub l 1 (String.length l - 1) in
        raise (Error(Illegal_escape esc, Location.curr lexbuf))
      )
# 2143 "parsing/lexer.ml"

  | 27 ->
# 862 "parsing/lexer.mll"
      ( let s, loc = with_comment_buffer comment lexbuf in
        COMMENT (s, loc) )
# 2149 "parsing/lexer.ml"

  | 28 ->
# 865 "parsing/lexer.mll"
      ( let s, loc = with_comment_buffer comment lexbuf in
#if undefined BS_MIN_LEX_DEPS then
        DOCSTRING (Docstrings.docstring s loc) 
#else
        COMMENT (s,loc)    
#end
)
# 2160 "parsing/lexer.ml"

  | 29 ->
let
# 872 "parsing/lexer.mll"
                    stars
# 2166 "parsing/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 873 "parsing/lexer.mll"
      ( let s, loc =
          with_comment_buffer
            (fun lexbuf ->
               store_string ("*" ^ stars);
               comment lexbuf)
            lexbuf
        in
        COMMENT (s, loc) )
# 2177 "parsing/lexer.ml"

  | 30 ->
# 882 "parsing/lexer.mll"
      ( if !print_warnings then
          Location.prerr_warning (Location.curr lexbuf) Warnings.Comment_start;
        let s, loc = with_comment_buffer comment lexbuf in
        COMMENT (s, loc) )
# 2185 "parsing/lexer.ml"

  | 31 ->
let
# 886 "parsing/lexer.mll"
                   stars
# 2191 "parsing/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_curr_pos + -2) in
# 887 "parsing/lexer.mll"
      ( COMMENT (stars, Location.curr lexbuf) )
# 2195 "parsing/lexer.ml"

  | 32 ->
# 889 "parsing/lexer.mll"
      ( let loc = Location.curr lexbuf in
        Location.prerr_warning loc Warnings.Comment_not_end;
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        let curpos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
        STAR
      )
# 2206 "parsing/lexer.ml"

  | 33 ->
let
# 896 "parsing/lexer.mll"
                                   num
# 2212 "parsing/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 897 "parsing/lexer.mll"
                                           name
# 2217 "parsing/lexer.ml"
= Lexing.sub_lexeme_opt lexbuf lexbuf.Lexing.lex_mem.(3) lexbuf.Lexing.lex_mem.(2) in
# 899 "parsing/lexer.mll"
      ( update_loc lexbuf name (int_of_string num) true 0;
        token lexbuf
      )
# 2223 "parsing/lexer.ml"

  | 34 ->
# 902 "parsing/lexer.mll"
         ( SHARP )
# 2228 "parsing/lexer.ml"

  | 35 ->
# 903 "parsing/lexer.mll"
         ( AMPERSAND )
# 2233 "parsing/lexer.ml"

  | 36 ->
# 904 "parsing/lexer.mll"
         ( AMPERAMPER )
# 2238 "parsing/lexer.ml"

  | 37 ->
# 905 "parsing/lexer.mll"
         ( BACKQUOTE )
# 2243 "parsing/lexer.ml"

  | 38 ->
# 906 "parsing/lexer.mll"
         ( QUOTE )
# 2248 "parsing/lexer.ml"

  | 39 ->
# 907 "parsing/lexer.mll"
         ( LPAREN )
# 2253 "parsing/lexer.ml"

  | 40 ->
# 908 "parsing/lexer.mll"
         ( RPAREN )
# 2258 "parsing/lexer.ml"

  | 41 ->
# 909 "parsing/lexer.mll"
         ( STAR )
# 2263 "parsing/lexer.ml"

  | 42 ->
# 910 "parsing/lexer.mll"
         ( COMMA )
# 2268 "parsing/lexer.ml"

  | 43 ->
# 911 "parsing/lexer.mll"
         ( MINUSGREATER )
# 2273 "parsing/lexer.ml"

  | 44 ->
# 912 "parsing/lexer.mll"
         ( DOT )
# 2278 "parsing/lexer.ml"

  | 45 ->
# 913 "parsing/lexer.mll"
         ( DOTDOT )
# 2283 "parsing/lexer.ml"

  | 46 ->
# 914 "parsing/lexer.mll"
         ( COLON )
# 2288 "parsing/lexer.ml"

  | 47 ->
# 915 "parsing/lexer.mll"
         ( COLONCOLON )
# 2293 "parsing/lexer.ml"

  | 48 ->
# 916 "parsing/lexer.mll"
         ( COLONEQUAL )
# 2298 "parsing/lexer.ml"

  | 49 ->
# 917 "parsing/lexer.mll"
         ( COLONGREATER )
# 2303 "parsing/lexer.ml"

  | 50 ->
# 918 "parsing/lexer.mll"
         ( SEMI )
# 2308 "parsing/lexer.ml"

  | 51 ->
# 919 "parsing/lexer.mll"
         ( SEMISEMI )
# 2313 "parsing/lexer.ml"

  | 52 ->
# 920 "parsing/lexer.mll"
         ( LESS )
# 2318 "parsing/lexer.ml"

  | 53 ->
# 921 "parsing/lexer.mll"
         ( LESSMINUS )
# 2323 "parsing/lexer.ml"

  | 54 ->
# 922 "parsing/lexer.mll"
         ( EQUAL )
# 2328 "parsing/lexer.ml"

  | 55 ->
# 923 "parsing/lexer.mll"
         ( LBRACKET )
# 2333 "parsing/lexer.ml"

  | 56 ->
# 924 "parsing/lexer.mll"
         ( LBRACKETBAR )
# 2338 "parsing/lexer.ml"

  | 57 ->
# 925 "parsing/lexer.mll"
         ( LBRACKETLESS )
# 2343 "parsing/lexer.ml"

  | 58 ->
# 926 "parsing/lexer.mll"
         ( LBRACKETGREATER )
# 2348 "parsing/lexer.ml"

  | 59 ->
# 927 "parsing/lexer.mll"
         ( RBRACKET )
# 2353 "parsing/lexer.ml"

  | 60 ->
# 928 "parsing/lexer.mll"
         ( LBRACE )
# 2358 "parsing/lexer.ml"

  | 61 ->
# 929 "parsing/lexer.mll"
         ( LBRACELESS )
# 2363 "parsing/lexer.ml"

  | 62 ->
# 930 "parsing/lexer.mll"
         ( BAR )
# 2368 "parsing/lexer.ml"

  | 63 ->
# 931 "parsing/lexer.mll"
         ( BARBAR )
# 2373 "parsing/lexer.ml"

  | 64 ->
# 932 "parsing/lexer.mll"
         ( BARRBRACKET )
# 2378 "parsing/lexer.ml"

  | 65 ->
# 933 "parsing/lexer.mll"
         ( GREATER )
# 2383 "parsing/lexer.ml"

  | 66 ->
# 934 "parsing/lexer.mll"
         ( GREATERRBRACKET )
# 2388 "parsing/lexer.ml"

  | 67 ->
# 935 "parsing/lexer.mll"
         ( RBRACE )
# 2393 "parsing/lexer.ml"

  | 68 ->
# 936 "parsing/lexer.mll"
         ( GREATERRBRACE )
# 2398 "parsing/lexer.ml"

  | 69 ->
# 937 "parsing/lexer.mll"
         ( LBRACKETAT )
# 2403 "parsing/lexer.ml"

  | 70 ->
# 938 "parsing/lexer.mll"
         ( LBRACKETPERCENT )
# 2408 "parsing/lexer.ml"

  | 71 ->
# 939 "parsing/lexer.mll"
          ( LBRACKETPERCENTPERCENT )
# 2413 "parsing/lexer.ml"

  | 72 ->
# 940 "parsing/lexer.mll"
          ( LBRACKETATAT )
# 2418 "parsing/lexer.ml"

  | 73 ->
# 941 "parsing/lexer.mll"
           ( LBRACKETATATAT )
# 2423 "parsing/lexer.ml"

  | 74 ->
# 942 "parsing/lexer.mll"
         ( BANG )
# 2428 "parsing/lexer.ml"

  | 75 ->
# 943 "parsing/lexer.mll"
         ( INFIXOP0 "!=" )
# 2433 "parsing/lexer.ml"

  | 76 ->
# 944 "parsing/lexer.mll"
         ( PLUS )
# 2438 "parsing/lexer.ml"

  | 77 ->
# 945 "parsing/lexer.mll"
         ( PLUSDOT )
# 2443 "parsing/lexer.ml"

  | 78 ->
# 946 "parsing/lexer.mll"
         ( PLUSEQ )
# 2448 "parsing/lexer.ml"

  | 79 ->
# 947 "parsing/lexer.mll"
         ( MINUS )
# 2453 "parsing/lexer.ml"

  | 80 ->
# 948 "parsing/lexer.mll"
         ( MINUSDOT )
# 2458 "parsing/lexer.ml"

  | 81 ->
# 951 "parsing/lexer.mll"
            ( PREFIXOP(Lexing.lexeme lexbuf) )
# 2463 "parsing/lexer.ml"

  | 82 ->
# 953 "parsing/lexer.mll"
            ( PREFIXOP(Lexing.lexeme lexbuf) )
# 2468 "parsing/lexer.ml"

  | 83 ->
# 955 "parsing/lexer.mll"
            ( INFIXOP0(Lexing.lexeme lexbuf) )
# 2473 "parsing/lexer.ml"

  | 84 ->
# 957 "parsing/lexer.mll"
            ( INFIXOP1(Lexing.lexeme lexbuf) )
# 2478 "parsing/lexer.ml"

  | 85 ->
# 959 "parsing/lexer.mll"
            ( INFIXOP2(Lexing.lexeme lexbuf) )
# 2483 "parsing/lexer.ml"

  | 86 ->
# 961 "parsing/lexer.mll"
            ( INFIXOP4(Lexing.lexeme lexbuf) )
# 2488 "parsing/lexer.ml"

  | 87 ->
# 962 "parsing/lexer.mll"
            ( PERCENT )
# 2493 "parsing/lexer.ml"

  | 88 ->
# 964 "parsing/lexer.mll"
            ( INFIXOP3(Lexing.lexeme lexbuf) )
# 2498 "parsing/lexer.ml"

  | 89 ->
# 966 "parsing/lexer.mll"
            ( SHARPOP(Lexing.lexeme lexbuf) )
# 2503 "parsing/lexer.ml"

  | 90 ->
# 967 "parsing/lexer.mll"
        (
      if !if_then_else <> Dir_out then
        if !if_then_else = Dir_if_true then
          raise (Error (Unterminated_if, Location.curr lexbuf))
        else raise (Error(Unterminated_else, Location.curr lexbuf))
      else 
        EOF
        
    )
# 2516 "parsing/lexer.ml"

  | 91 ->
# 977 "parsing/lexer.mll"
      ( raise (Error(Illegal_character (Lexing.lexeme_char lexbuf 0),
                     Location.curr lexbuf))
      )
# 2523 "parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
    __ocaml_lex_comment_rec lexbuf 132
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 983 "parsing/lexer.mll"
      ( comment_start_loc := (Location.curr lexbuf) :: !comment_start_loc;
        store_lexeme lexbuf;
        comment lexbuf;
      )
# 2538 "parsing/lexer.ml"

  | 1 ->
# 988 "parsing/lexer.mll"
      ( match !comment_start_loc with
        | [] -> assert false
        | [_] -> comment_start_loc := []; Location.curr lexbuf
        | _ :: l -> comment_start_loc := l;
                  store_lexeme lexbuf;
                  comment lexbuf;
       )
# 2549 "parsing/lexer.ml"

  | 2 ->
# 996 "parsing/lexer.mll"
      (
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
        comment lexbuf )
# 2570 "parsing/lexer.ml"

  | 3 ->
# 1014 "parsing/lexer.mll"
      (
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
        comment lexbuf )
# 2595 "parsing/lexer.ml"

  | 4 ->
# 1037 "parsing/lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 2600 "parsing/lexer.ml"

  | 5 ->
# 1039 "parsing/lexer.mll"
      ( update_loc lexbuf None 1 false 1;
        store_lexeme lexbuf;
        comment lexbuf
      )
# 2608 "parsing/lexer.ml"

  | 6 ->
# 1044 "parsing/lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 2613 "parsing/lexer.ml"

  | 7 ->
# 1046 "parsing/lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 2618 "parsing/lexer.ml"

  | 8 ->
# 1048 "parsing/lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 2623 "parsing/lexer.ml"

  | 9 ->
# 1050 "parsing/lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 2628 "parsing/lexer.ml"

  | 10 ->
# 1052 "parsing/lexer.mll"
      ( match !comment_start_loc with
        | [] -> assert false
        | loc :: _ ->
          let start = List.hd (List.rev !comment_start_loc) in
          comment_start_loc := [];
          raise (Error (Unterminated_comment start, loc))
      )
# 2639 "parsing/lexer.ml"

  | 11 ->
# 1060 "parsing/lexer.mll"
      ( update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        comment lexbuf
      )
# 2647 "parsing/lexer.ml"

  | 12 ->
# 1065 "parsing/lexer.mll"
      ( store_lexeme lexbuf; comment lexbuf )
# 2652 "parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and string lexbuf =
  lexbuf.Lexing.lex_mem <- Array.make 2 (-1) ;   __ocaml_lex_string_rec lexbuf 164
and __ocaml_lex_string_rec lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1069 "parsing/lexer.mll"
      ( () )
# 2664 "parsing/lexer.ml"

  | 1 ->
let
# 1070 "parsing/lexer.mll"
                                  space
# 2670 "parsing/lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 1071 "parsing/lexer.mll"
      ( update_loc lexbuf None 1 false (String.length space);
        string lexbuf
      )
# 2676 "parsing/lexer.ml"

  | 2 ->
# 1075 "parsing/lexer.mll"
      ( store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf )
# 2682 "parsing/lexer.ml"

  | 3 ->
# 1078 "parsing/lexer.mll"
      ( store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf )
# 2688 "parsing/lexer.ml"

  | 4 ->
# 1081 "parsing/lexer.mll"
      ( store_string_char(char_for_hexadecimal_code lexbuf 2);
         string lexbuf )
# 2694 "parsing/lexer.ml"

  | 5 ->
# 1084 "parsing/lexer.mll"
      ( if in_comment ()
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
      )
# 2712 "parsing/lexer.ml"

  | 6 ->
# 1099 "parsing/lexer.mll"
      ( if not (in_comment ()) then
          Location.prerr_warning (Location.curr lexbuf) Warnings.Eol_in_string;
        update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        string lexbuf
      )
# 2722 "parsing/lexer.ml"

  | 7 ->
# 1106 "parsing/lexer.mll"
      ( is_in_string := false;
        raise (Error (Unterminated_string, !string_start_loc)) )
# 2728 "parsing/lexer.ml"

  | 8 ->
# 1109 "parsing/lexer.mll"
      ( store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf )
# 2734 "parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_string_rec lexbuf __ocaml_lex_state

and quoted_string delim lexbuf =
    __ocaml_lex_quoted_string_rec delim lexbuf 183
and __ocaml_lex_quoted_string_rec delim lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1114 "parsing/lexer.mll"
      ( update_loc lexbuf None 1 false 0;
        store_lexeme lexbuf;
        quoted_string delim lexbuf
      )
# 2749 "parsing/lexer.ml"

  | 1 ->
# 1119 "parsing/lexer.mll"
      ( is_in_string := false;
        raise (Error (Unterminated_string, !string_start_loc)) )
# 2755 "parsing/lexer.ml"

  | 2 ->
# 1122 "parsing/lexer.mll"
      (
        let edelim = Lexing.lexeme lexbuf in
        let edelim = String.sub edelim 1 (String.length edelim - 2) in
        if delim = edelim then ()
        else (store_lexeme lexbuf; quoted_string delim lexbuf)
      )
# 2765 "parsing/lexer.ml"

  | 3 ->
# 1129 "parsing/lexer.mll"
      ( store_string_char(Lexing.lexeme_char lexbuf 0);
        quoted_string delim lexbuf )
# 2771 "parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_quoted_string_rec delim lexbuf __ocaml_lex_state

and skip_sharp_bang lexbuf =
    __ocaml_lex_skip_sharp_bang_rec lexbuf 192
and __ocaml_lex_skip_sharp_bang_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 1134 "parsing/lexer.mll"
       ( update_loc lexbuf None 3 false 0 )
# 2783 "parsing/lexer.ml"

  | 1 ->
# 1136 "parsing/lexer.mll"
       ( update_loc lexbuf None 1 false 0 )
# 2788 "parsing/lexer.ml"

  | 2 ->
# 1137 "parsing/lexer.mll"
       ( () )
# 2793 "parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_skip_sharp_bang_rec lexbuf __ocaml_lex_state

;;

# 1139 "parsing/lexer.mll"
 

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


# 3035 "parsing/lexer.ml"
