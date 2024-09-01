(* Copyright (C) 2021- Hongbo Zhang, Authors of ReScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type directive_type =
  | Dir_type_bool
  | Dir_type_float
  | Dir_type_int
  | Dir_type_string
  | Dir_type_null

type pp_error =
  | Unterminated_paren_in_conditional
  | Unterminated_if
  | Unterminated_else
  | Unexpected_token_in_conditional
  | Expect_hash_then_in_conditional
  | Illegal_semver of string
  | Unexpected_directive
  | Conditional_expr_expected_type of directive_type * directive_type

exception Pp_error of pp_error * Location.t

type directive_value =
  | Dir_bool of bool
  | Dir_float of float
  | Dir_int of int
  | Dir_string of string
  | Dir_null

let type_of_directive x =
  match x with
  | Dir_bool _ -> Dir_type_bool
  | Dir_float _ -> Dir_type_float
  | Dir_int _ -> Dir_type_int
  | Dir_string _ -> Dir_type_string
  | Dir_null -> Dir_type_null

let string_of_type_directive x =
  match x with
  | Dir_type_bool -> "bool"
  | Dir_type_float -> "float"
  | Dir_type_int -> "int"
  | Dir_type_string -> "string"
  | Dir_type_null -> "null"

let prepare_pp_error loc = function
  | Unterminated_if -> Location.errorf ~loc "#if not terminated"
  | Unterminated_else -> Location.errorf ~loc "#else not terminated"
  | Unexpected_directive -> Location.errorf ~loc "Unexpected directive"
  | Unexpected_token_in_conditional ->
      Location.errorf ~loc "Unexpected token in conditional predicate"
  | Unterminated_paren_in_conditional ->
      Location.errorf ~loc "Unterminated parens in conditional predicate"
  | Expect_hash_then_in_conditional ->
      Location.errorf ~loc "Expect `then` after conditional predicate"
  | Conditional_expr_expected_type (a, b) ->
      Location.errorf ~loc "Conditional expression type mismatch (%s,%s)"
        (string_of_type_directive a)
        (string_of_type_directive b)
  | Illegal_semver s ->
      Location.errorf ~loc "Illegal semantic version string %s" s

let () =
  Location.register_error_of_exn (function
    | Pp_error (err, loc) -> Some (prepare_pp_error loc err)
    | _ -> None)

let assert_same_type lexbuf x y =
  let lhs = type_of_directive x in
  let rhs = type_of_directive y in
  if lhs <> rhs then
    raise
      (Pp_error (Conditional_expr_expected_type (lhs, rhs), Location.curr lexbuf))
  else y

let directive_built_in_values = Hashtbl.create 51

let replace_directive_built_in_value k v =
  Hashtbl.replace directive_built_in_values k v

let remove_directive_built_in_value k =
  Hashtbl.replace directive_built_in_values k Dir_null

let replace_directive_bool k v =
  Hashtbl.replace directive_built_in_values k (Dir_bool v)

let replace_directive_string k v =
  Hashtbl.replace directive_built_in_values k (Dir_string v)

let () =
  (* Note we use {!Config} instead of {!Sys} becasue
     we want to overwrite in some cases with the
     same stdlib
  *)
  let version = Config.version (* so that it can be overridden*) in
  replace_directive_built_in_value "OCAML_VERSION" (Dir_string version);
  replace_directive_built_in_value "OS_TYPE" (Dir_string Sys.os_type)

let find_directive_built_in_value k = Hashtbl.find directive_built_in_values k

let iter_directive_built_in_value f = Hashtbl.iter f directive_built_in_values
(* let iter_directive_built_in_value f = Hashtbl.iter f directive_built_in_values *)

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

let semantic_version_parse str start last_index =
  let rec aux start acc last_index =
    if start <= last_index then
      let c = Char.code (String.unsafe_get str start) in
      if c = dot then (acc, start + 1) (* consume [4.] instead of [4]*)
      else
        let v = c - zero in
        if v >= 0 && v <= 9 then aux (start + 1) ((acc * 10) + v) last_index
        else (acc, start)
    else (acc, start)
  in
  let major, major_end = aux start 0 last_index in
  let minor, minor_end = aux major_end 0 last_index in
  let patch, patch_end = aux minor_end 0 last_index in
  let additional = String.sub str patch_end (last_index - patch_end + 1) in
  ((major, minor, patch), additional)

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
  if last_index < 0 then raise (Pp_error (Illegal_semver str, loc))
  else
    let pred, (((major, minor, _patch) as version), _) =
      let v = String.unsafe_get str 0 in
      match v with
      | '>' ->
          if last_index = 0 then raise (Pp_error (Illegal_semver str, loc))
          else if String.unsafe_get str 1 = '=' then
            (`Ge, semantic_version_parse str 2 last_index)
          else (`Gt, semantic_version_parse str 1 last_index)
      | '<' ->
          if last_index = 0 then raise (Pp_error (Illegal_semver str, loc))
          else if String.unsafe_get str 1 = '=' then
            (`Le, semantic_version_parse str 2 last_index)
          else (`Lt, semantic_version_parse str 1 last_index)
      | '^' -> (`Compatible, semantic_version_parse str 1 last_index)
      | '~' -> (`Approximate, semantic_version_parse str 1 last_index)
      | _ -> (`Exact, semantic_version_parse str 0 last_index)
    in
    let ((l_major, l_minor, _l_patch) as lversion), _ =
      semantic_version_parse lhs 0 (String.length lhs - 1)
    in
    match pred with
    | `Ge -> lversion >= version
    | `Gt -> lversion > version
    | `Le -> lversion <= version
    | `Lt -> lversion < version
    | `Approximate -> major = l_major && minor = l_minor
    | `Compatible -> major = l_major
    | `Exact -> lversion = version

let pp_directive_value fmt (x : directive_value) =
  match x with
  | Dir_bool b -> Format.pp_print_bool fmt b
  | Dir_int b -> Format.pp_print_int fmt b
  | Dir_float b -> Format.pp_print_float fmt b
  | Dir_string s -> Format.fprintf fmt "%S" s
  | Dir_null -> Format.pp_print_string fmt "null"

let list_variables fmt =
  iter_directive_built_in_value (fun s dir_value ->
      Format.fprintf fmt "@[%s@ %a@]@." s pp_directive_value dir_value)

let defined str =
  match find_directive_built_in_value str with
  | Dir_null -> false
  | _ -> true
  | exception _ -> (
      try
        ignore @@ Sys.getenv str;
        true
      with _ -> false)

let query _loc str =
  match find_directive_built_in_value str with
  | Dir_null -> Dir_bool false
  | v -> v
  | exception Not_found -> (
      match Sys.getenv str with
      | v -> (
          try Dir_bool (bool_of_string v)
          with _ -> (
            try Dir_int (int_of_string v)
            with _ -> (
              try Dir_float (float_of_string v) with _ -> Dir_string v)))
      | exception Not_found -> Dir_bool false)

let define_key_value key v =
  if String.length key > 0 && Char.uppercase_ascii key.[0] = key.[0] then (
    replace_directive_built_in_value key
      (* NEED Sync up across {!lexer.mll} {!bspp.ml} and here,
         TODO: put it in {!lexer.mll}
      *)
      (try Dir_bool (bool_of_string v)
       with _ -> (
         try Dir_int (int_of_string v)
         with _ -> (
           try Dir_float (float_of_string v) with _ -> Dir_string v)));
    true)
  else false

let cvt_int_literal s = -int_of_string ("-" ^ s)

let value_of_token loc (t : Parser.token) =
  match t with
  | INT (i, None) -> Dir_int (cvt_int_literal i)
  | STRING (s, _) -> Dir_string s
  | FLOAT (s, None) -> Dir_float (float_of_string s)
  | TRUE -> Dir_bool true
  | FALSE -> Dir_bool false
  | UIDENT s -> query loc s
  | _ -> raise (Pp_error (Unexpected_token_in_conditional, loc))

let directive_parse (token_with_comments : Lexing.lexbuf -> Parser.token) lexbuf
    =
  let look_ahead = ref None in
  let token () : Parser.token =
    let v = !look_ahead in
    match v with
    | Some v ->
        look_ahead := None;
        v
    | None ->
        let rec skip () =
          match token_with_comments lexbuf with
          | COMMENT _ | DOCSTRING _ -> skip ()
          | EOF -> raise (Pp_error (Unterminated_if, Location.curr lexbuf))
          | t -> t
        in
        skip ()
  in
  let push e =
    (* INVARIANT: only look at most one token *)
    assert (!look_ahead = None);
    look_ahead := Some e
  in
  let rec token_op calc ~no lhs =
    match token () with
    | (LESS | GREATER | INFIXOP0 "<=" | INFIXOP0 ">=" | EQUAL | INFIXOP0 "<>")
      as op ->
        let f =
          match op with
          | LESS -> ( < )
          | GREATER -> ( > )
          | INFIXOP0 "<=" -> ( <= )
          | EQUAL -> ( = )
          | INFIXOP0 "<>" -> ( <> )
          | _ -> assert false
        in
        let curr_loc = Location.curr lexbuf in
        let rhs = value_of_token curr_loc (token ()) in
        (not calc) || f lhs (assert_same_type lexbuf lhs rhs)
    | INFIXOP0 "=~" -> (
        (not calc)
        ||
        match lhs with
        | Dir_string s -> (
            let curr_loc = Location.curr lexbuf in
            let rhs = value_of_token curr_loc (token ()) in
            match rhs with
            | Dir_string rhs -> semver curr_loc s rhs
            | _ ->
                raise
                  (Pp_error
                     ( Conditional_expr_expected_type
                         (Dir_type_string, type_of_directive lhs),
                       Location.curr lexbuf )))
        | _ ->
            raise
              (Pp_error
                 ( Conditional_expr_expected_type
                     (Dir_type_string, type_of_directive lhs),
                   Location.curr lexbuf )))
    | e -> no e
  and parse_or calc : bool = parse_or_aux calc (parse_and calc)
  and (* a || (b || (c || d))*)
      parse_or_aux calc v : bool =
    (* let l = v  in *)
    match token () with
    | BARBAR ->
        let b = parse_or (calc && not v) in
        v || b
    | e ->
        push e;
        v
  and parse_and calc = parse_and_aux calc (parse_relation calc)
  and parse_and_aux calc v =
    (* a && (b && (c && d)) *)
    (* let l = v  in *)
    match token () with
    | AMPERAMPER ->
        let b = parse_and (calc && v) in
        v && b
    | e ->
        push e;
        v
  and parse_relation (calc : bool) : bool =
    let curr_token = token () in
    let curr_loc = Location.curr lexbuf in
    match curr_token with
    | TRUE -> true
    | FALSE -> false
    | UIDENT v ->
        let value_v = query curr_loc v in
        token_op calc
          ~no:(fun e ->
            push e;
            match value_v with
            | Dir_bool b -> b
            | _ ->
                let ty = type_of_directive value_v in
                raise
                  (Pp_error
                     ( Conditional_expr_expected_type (Dir_type_bool, ty),
                       curr_loc )))
          value_v
    | INT (v, None) ->
        let num_v = cvt_int_literal v in
        token_op calc
          ~no:(fun e ->
            push e;
            num_v <> 0)
          (Dir_int num_v)
    | FLOAT (v, None) ->
        token_op calc
          ~no:(fun _e ->
            raise
              (Pp_error
                 ( Conditional_expr_expected_type (Dir_type_bool, Dir_type_float),
                   curr_loc )))
          (Dir_float (float_of_string v))
    | STRING (v, _) ->
        token_op calc
          ~no:(fun _e ->
            raise
              (Pp_error
                 ( Conditional_expr_expected_type
                     (Dir_type_bool, Dir_type_string),
                   curr_loc )))
          (Dir_string v)
    | LIDENT (("defined" | "undefined") as r) -> (
        let t = token () in
        let loc = Location.curr lexbuf in
        match t with
        | UIDENT s ->
            (not calc) || if r.[0] = 'u' then not @@ defined s else defined s
        | _ -> raise (Pp_error (Unexpected_token_in_conditional, loc)))
    | LPAREN -> (
        let v = parse_or calc in
        match token () with
        | RPAREN -> v
        | _ ->
            raise
              (Pp_error (Unterminated_paren_in_conditional, Location.curr lexbuf))
        )
    | _ -> raise (Pp_error (Unexpected_token_in_conditional, curr_loc))
  in
  let v = parse_or true in
  match token () with
  | THEN | EOL -> v
  | _ ->
      raise (Pp_error (Expect_hash_then_in_conditional, Location.curr lexbuf))

type dir_conditional = Dir_if_true | Dir_if_false | Dir_out

(* let string_of_dir_conditional (x : dir_conditional) = *)
(*   match x with  *)
(*   | Dir_if_true -> "Dir_if_true" *)
(*   | Dir_if_false -> "Dir_if_false" *)
(*   | Dir_out -> "Dir_out" *)

let if_then_else = ref Dir_out

(* store the token after hash, [# token]
   when we see `#if` we do the processing immediately
   when we see #method, we produce `HASH` token and save `method`
   token so that the next lexing produce the right one.
*)
let sharp_look_ahead = ref None

let update_if_then_else v =
  (* Format.fprintf Format.err_formatter "@[update %s \n@]@." (string_of_dir_conditional v); *)
  if_then_else := v

let at_bol lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  pos.pos_cnum = pos.pos_bol

(* skip to #else | #end | #elif *)
let rec skip_from_if_false (token_with_comments : Lexing.lexbuf -> Parser.token)
    cont lexbuf =
  let token = token_with_comments lexbuf in
  if token = EOF then raise (Pp_error (Unterminated_if, Location.curr lexbuf))
  else if token = HASH && at_bol lexbuf then
    let token = token_with_comments lexbuf in
    match token with
    | END | LIDENT "endif" ->
        update_if_then_else Dir_out;
        cont lexbuf
    | ELSE ->
        update_if_then_else Dir_if_false;
        cont lexbuf
    | IF -> raise (Pp_error (Unexpected_directive, Location.curr lexbuf))
    | LIDENT "elif" when directive_parse token_with_comments lexbuf ->
        update_if_then_else Dir_if_true;
        cont lexbuf
    | _ -> skip_from_if_false token_with_comments cont lexbuf
  else skip_from_if_false token_with_comments cont lexbuf

let interpret_directive_cont lexbuf ~cont
    ~(token_with_comments : Lexing.lexbuf -> Parser.token) look_ahead =
  (* current state *)
  let if_then_else = !if_then_else in
  match (token_with_comments lexbuf, if_then_else) with
  | IF, Dir_out ->
      if directive_parse token_with_comments lexbuf then (
        update_if_then_else Dir_if_true (* Next state: ELSE *);
        cont lexbuf)
      else skip_from_if_false token_with_comments cont lexbuf
  | LIDENT (("ifndef" | "ifdef") as s), Dir_out ->
      let rec token () =
        match token_with_comments lexbuf with
        | COMMENT _ | DOCSTRING _ -> token ()
        | EOF -> raise (Pp_error (Unterminated_if, Location.curr lexbuf))
        | t -> t
      in
      let t0 = token () in
      let t =
        match t0 with
        | UIDENT t -> t
        | _ ->
            raise
              (Pp_error (Unexpected_token_in_conditional, Location.curr lexbuf))
      in
      let t1 = token () in
      (match t1 with
      | THEN | EOL -> ()
      | _ ->
          raise
            (Pp_error (Expect_hash_then_in_conditional, Location.curr lexbuf)));
      let boolean = defined t = (s = "ifdef") in
      if boolean then (
        update_if_then_else Dir_if_true (* Next state: ELSE *);
        cont lexbuf)
      else skip_from_if_false token_with_comments cont lexbuf
  | (IF | LIDENT "ifndef" | LIDENT "ifdef"), (Dir_if_false | Dir_if_true) ->
      raise (Pp_error (Unexpected_directive, Location.curr lexbuf))
  | LIDENT "elif", (Dir_if_false | Dir_out) ->
      (* when the predicate is false, it will continue eating `elif` *)
      raise (Pp_error (Unexpected_directive, Location.curr lexbuf))
  | ((LIDENT "elif" | ELSE) as token), Dir_if_true ->
      (* looking for #end, however, it can not see #if anymore,
         we need do some validation *)
      let rec skip_from_if_true else_seen =
        let token = token_with_comments lexbuf in
        if token = EOF then
          raise (Pp_error (Unterminated_else, Location.curr lexbuf))
        else if token = HASH && at_bol lexbuf then
          let token = token_with_comments lexbuf in
          match token with
          | END | LIDENT "endif" ->
              update_if_then_else Dir_out;
              cont lexbuf
          | IF -> raise (Pp_error (Unexpected_directive, Location.curr lexbuf))
          | ELSE ->
              if else_seen then
                raise (Pp_error (Unexpected_directive, Location.curr lexbuf))
              else skip_from_if_true true
          | LIDENT "elif" when else_seen ->
              raise (Pp_error (Unexpected_directive, Location.curr lexbuf))
          | _ -> skip_from_if_true else_seen
        else skip_from_if_true else_seen
      in
      skip_from_if_true (token = ELSE)
  | ELSE, Dir_if_false | ELSE, Dir_out ->
      raise (Pp_error (Unexpected_directive, Location.curr lexbuf))
  | (END | LIDENT "endif"), (Dir_if_false | Dir_if_true) ->
      update_if_then_else Dir_out;
      cont lexbuf
  | (END | LIDENT "endif"), Dir_out ->
      raise (Pp_error (Unexpected_directive, Location.curr lexbuf))
  | token, (Dir_if_true | Dir_if_false | Dir_out) -> look_ahead token

let interpret_directive lexbuf ~cont ~token_with_comments : Parser.token =
  interpret_directive_cont lexbuf ~cont ~token_with_comments
    (fun (token : 'a) : 'a ->
      sharp_look_ahead := Some token;
      HASH)

let eof_check lexbuf =
  if !if_then_else <> Dir_out then
    if !if_then_else = Dir_if_true then
      raise (Pp_error (Unterminated_if, Location.curr lexbuf))
    else raise (Pp_error (Unterminated_else, Location.curr lexbuf))

let init () =
  sharp_look_ahead := None;
  update_if_then_else Dir_out

let check_sharp_look_ahead action : Parser.token =
  match !sharp_look_ahead with
  | None -> action ()
  | Some token ->
      sharp_look_ahead := None;
      token

let rec filter_directive ~(token_with_comments : Lexing.lexbuf -> Parser.token)
    pos acc lexbuf : (int * int) list =
  match token_with_comments lexbuf with
  | HASH when at_bol lexbuf ->
      (* ^[start_pos]#if ... #then^[end_pos] *)
      let start_pos = Lexing.lexeme_start lexbuf in
      interpret_directive_cont lexbuf
        ~cont:(fun lexbuf ->
          filter_directive (Lexing.lexeme_end lexbuf) ~token_with_comments
            ((pos, start_pos) :: acc) lexbuf)
        ~token_with_comments
        (fun _token -> filter_directive pos acc lexbuf ~token_with_comments)
  | EOF -> (pos, Lexing.lexeme_end lexbuf) :: acc
  | _ -> filter_directive ~token_with_comments pos acc lexbuf

let filter_directive_from_lexbuf lexbuf ~token_with_comments =
  List.rev (filter_directive 0 [] lexbuf ~token_with_comments)
