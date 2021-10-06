(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Sedlexing = Flow_sedlexing
open Flow_ast
module SSet = Set.Make (String)

module Lex_mode = struct
  type t =
    | NORMAL
    | TYPE
    | JSX_TAG
    | JSX_CHILD
    | TEMPLATE
    | REGEXP

  let debug_string_of_lex_mode (mode : t) =
    match mode with
    | NORMAL -> "NORMAL"
    | TYPE -> "TYPE"
    | JSX_TAG -> "JSX_TAG"
    | JSX_CHILD -> "JSX_CHILD"
    | TEMPLATE -> "TEMPLATE"
    | REGEXP -> "REGEXP"
end

module Lookahead : sig
  type t

  val create : Lex_env.t -> Lex_mode.t -> t

  val peek_0 : t -> Lex_result.t

  val peek_1 : t -> Lex_result.t

  val lex_env_0 : t -> Lex_env.t

  val junk : t -> unit
end = struct
  type la_result = (Lex_env.t * Lex_result.t) option

  type t = {
    mutable la_results_0: la_result;
    mutable la_results_1: la_result;
    la_lex_mode: Lex_mode.t;
    mutable la_lex_env: Lex_env.t;
  }

  let create lex_env mode =
    let lex_env = Lex_env.clone lex_env in
    { la_results_0 = None; la_results_1 = None; la_lex_mode = mode; la_lex_env = lex_env }

  let lex t =
    let lex_env = t.la_lex_env in
    let (lex_env, lex_result) =
      match t.la_lex_mode with
      | Lex_mode.NORMAL -> Flow_lexer.token lex_env
      | Lex_mode.TYPE -> Flow_lexer.type_token lex_env
      | Lex_mode.JSX_TAG -> Flow_lexer.jsx_tag lex_env
      | Lex_mode.JSX_CHILD -> Flow_lexer.jsx_child lex_env
      | Lex_mode.TEMPLATE -> Flow_lexer.template_tail lex_env
      | Lex_mode.REGEXP -> Flow_lexer.regexp lex_env
    in
    let cloned_env = Lex_env.clone lex_env in
    let result = (cloned_env, lex_result) in
    t.la_lex_env <- lex_env;
    (match t.la_results_0 with
    | None -> t.la_results_0 <- Some result
    | Some _ -> t.la_results_1 <- Some result);
    result

  let peek_0 t =
    match t.la_results_0 with
    | Some (_, result) -> result
    | None -> snd (lex t)

  let peek_1 t =
    (match t.la_results_0 with
    | None -> ignore (lex t)
    | Some _ -> ());
    match t.la_results_1 with
    | None -> snd (lex t)
    | Some (_, result) -> result

  let lex_env_0 t =
    match t.la_results_0 with
    | Some (lex_env, _) -> lex_env
    | None -> fst (lex t)

  let junk t =
    match t.la_results_1 with
    | None ->
      ignore (peek_0 t);
      t.la_results_0 <- None
    | Some _ ->
      t.la_results_0 <- t.la_results_1;
      t.la_results_1 <- None
end

type token_sink_result = {
  token_loc: Loc.t;
  token: Token.t;
  token_context: Lex_mode.t;
}

type parse_options = {
  enums: bool; [@ocaml.doc " enable parsing of Flow enums "]
  esproposal_class_instance_fields: bool; [@ocaml.doc " enable parsing of class instance fields "]
  esproposal_class_static_fields: bool; [@ocaml.doc " enable parsing of class static fields "]
  esproposal_decorators: bool; [@ocaml.doc " enable parsing of decorators "]
  esproposal_export_star_as: bool; [@ocaml.doc " enable parsing of `export * as` syntax "]
  esproposal_nullish_coalescing: bool; [@ocaml.doc " enable parsing of nullish coalescing (`??`) "]
  esproposal_optional_chaining: bool; [@ocaml.doc " enable parsing of optional chaining (`?.`) "]
  types: bool; [@ocaml.doc " enable parsing of Flow types "]
  use_strict: bool;
      [@ocaml.doc " treat the file as strict, without needing a \"use strict\" directive "]
}

let default_parse_options =
  {
    enums = false;
    esproposal_class_instance_fields = false;
    esproposal_class_static_fields = false;
    esproposal_decorators = false;
    esproposal_export_star_as = false;
    esproposal_optional_chaining = false;
    esproposal_nullish_coalescing = false;
    types = true;
    use_strict = false;
  }

type allowed_super =
  | No_super
  | Super_prop
  | Super_prop_or_call

type env = {
  errors: (Loc.t * Parse_error.t) list ref;
  comments: Loc.t Comment.t list ref;
  labels: SSet.t;
  exports: SSet.t ref;
  last_lex_result: Lex_result.t option ref;
  in_strict_mode: bool;
  in_export: bool;
  in_loop: bool;
  in_switch: bool;
  in_formal_parameters: bool;
  in_function: bool;
  no_in: bool;
  no_call: bool;
  no_let: bool;
  no_anon_function_type: bool;
  no_new: bool;
  allow_yield: bool;
  allow_await: bool;
  allow_directive: bool;
  allow_super: allowed_super;
  error_callback: (env -> Parse_error.t -> unit) option;
  lex_mode_stack: Lex_mode.t list ref;
  lex_env: Lex_env.t ref;
  lookahead: Lookahead.t ref;
  token_sink: (token_sink_result -> unit) option ref;
  parse_options: parse_options;
  source: File_key.t option;
  privates: (SSet.t * (string * Loc.t) list) list ref;
  consumed_comments_pos: Loc.position ref;
}

let init_env ?(token_sink = None) ?(parse_options = None) source content =
  let (lb, errors) =
    try (Sedlexing.Utf8.from_string content, []) with
    | Sedlexing.MalFormed ->
      (Sedlexing.Utf8.from_string "", [({ Loc.none with Loc.source }, Parse_error.MalformedUnicode)])
  in
  let parse_options =
    match parse_options with
    | Some opts -> opts
    | None -> default_parse_options
  in
  let enable_types_in_comments = parse_options.types in
  let lex_env = Lex_env.new_lex_env source lb ~enable_types_in_comments in
  {
    errors = ref errors;
    comments = ref [];
    labels = SSet.empty;
    exports = ref SSet.empty;
    last_lex_result = ref None;
    in_strict_mode = parse_options.use_strict;
    in_export = false;
    in_loop = false;
    in_switch = false;
    in_formal_parameters = false;
    in_function = false;
    no_in = false;
    no_call = false;
    no_let = false;
    no_anon_function_type = false;
    no_new = false;
    allow_yield = false;
    allow_await = false;
    allow_directive = false;
    allow_super = No_super;
    error_callback = None;
    lex_mode_stack = ref [Lex_mode.NORMAL];
    lex_env = ref lex_env;
    lookahead = ref (Lookahead.create lex_env Lex_mode.NORMAL);
    token_sink = ref token_sink;
    parse_options;
    source;
    privates = ref [];
    consumed_comments_pos = ref { Loc.line = 0; column = 0 };
  }

let in_strict_mode env = env.in_strict_mode

let lex_mode env = List.hd !(env.lex_mode_stack)

let in_export env = env.in_export

let comments env = !(env.comments)

let labels env = env.labels

let in_loop env = env.in_loop

let in_switch env = env.in_switch

let in_formal_parameters env = env.in_formal_parameters

let in_function env = env.in_function

let allow_yield env = env.allow_yield

let allow_await env = env.allow_await

let allow_directive env = env.allow_directive

let allow_super env = env.allow_super

let no_in env = env.no_in

let no_call env = env.no_call

let no_let env = env.no_let

let no_anon_function_type env = env.no_anon_function_type

let no_new env = env.no_new

let errors env = !(env.errors)

let parse_options env = env.parse_options

let source env = env.source

let should_parse_types env = env.parse_options.types

let error_at env (loc, e) =
  env.errors := (loc, e) :: !(env.errors);
  match env.error_callback with
  | None -> ()
  | Some callback -> callback env e

let record_export env (loc, { Identifier.name = export_name; comments = _ }) =
  if export_name = "" then
    ()
  else
    let exports = !(env.exports) in
    if SSet.mem export_name exports then
      error_at env (loc, Parse_error.DuplicateExport export_name)
    else
      env.exports := SSet.add export_name !(env.exports)

let enter_class env = env.privates := (SSet.empty, []) :: !(env.privates)

let exit_class env =
  let get_unbound_privates declared_privates used_privates =
    List.filter (fun x -> not (SSet.mem (fst x) declared_privates)) used_privates
  in
  match !(env.privates) with
  | [(declared_privates, used_privates)] ->
    let unbound_privates = get_unbound_privates declared_privates used_privates in
    List.iter
      (fun (name, loc) -> error_at env (loc, Parse_error.UnboundPrivate name))
      unbound_privates;
    env.privates := []
  | (loc_declared_privates, loc_used_privates) :: privates ->
    let unbound_privates = get_unbound_privates loc_declared_privates loc_used_privates in
    let (decl_head, used_head) = List.hd privates in
    env.privates := (decl_head, used_head @ unbound_privates) :: List.tl privates
  | _ -> failwith "Internal Error: `exit_class` called before a matching `enter_class`"

let add_declared_private env name =
  match !(env.privates) with
  | [] -> failwith "Internal Error: Tried to add_declared_private with outside of class scope."
  | (declared, used) :: xs -> env.privates := (SSet.add name declared, used) :: xs

let add_used_private env name loc =
  match !(env.privates) with
  | [] -> error_at env (loc, Parse_error.PrivateNotInClass)
  | (declared, used) :: xs -> env.privates := (declared, (name, loc) :: used) :: xs

let consume_comments_until env pos = env.consumed_comments_pos := pos

let lookahead_0 env = Lookahead.peek_0 !(env.lookahead)

let lookahead_1 env = Lookahead.peek_1 !(env.lookahead)

let lookahead ~i env =
  match i with
  | 0 -> lookahead_0 env
  | 1 -> lookahead_1 env
  | _ -> assert false

let with_strict in_strict_mode env =
  if in_strict_mode = env.in_strict_mode then
    env
  else
    { env with in_strict_mode }

let with_in_formal_parameters in_formal_parameters env =
  if in_formal_parameters = env.in_formal_parameters then
    env
  else
    { env with in_formal_parameters }

let with_in_function in_function env =
  if in_function = env.in_function then
    env
  else
    { env with in_function }

let with_allow_yield allow_yield env =
  if allow_yield = env.allow_yield then
    env
  else
    { env with allow_yield }

let with_allow_await allow_await env =
  if allow_await = env.allow_await then
    env
  else
    { env with allow_await }

let with_allow_directive allow_directive env =
  if allow_directive = env.allow_directive then
    env
  else
    { env with allow_directive }

let with_allow_super allow_super env =
  if allow_super = env.allow_super then
    env
  else
    { env with allow_super }

let with_no_let no_let env =
  if no_let = env.no_let then
    env
  else
    { env with no_let }

let with_in_loop in_loop env =
  if in_loop = env.in_loop then
    env
  else
    { env with in_loop }

let with_no_in no_in env =
  if no_in = env.no_in then
    env
  else
    { env with no_in }

let with_no_anon_function_type no_anon_function_type env =
  if no_anon_function_type = env.no_anon_function_type then
    env
  else
    { env with no_anon_function_type }

let with_no_new no_new env =
  if no_new = env.no_new then
    env
  else
    { env with no_new }

let with_in_switch in_switch env =
  if in_switch = env.in_switch then
    env
  else
    { env with in_switch }

let with_in_export in_export env =
  if in_export = env.in_export then
    env
  else
    { env with in_export }

let with_no_call no_call env =
  if no_call = env.no_call then
    env
  else
    { env with no_call }

let with_error_callback error_callback env = { env with error_callback = Some error_callback }

let error_list env = List.iter (error_at env)

let last_loc env =
  match !(env.last_lex_result) with
  | Some lex_result -> Some (Lex_result.loc lex_result)
  | None -> None

let last_token env =
  match !(env.last_lex_result) with
  | Some lex_result -> Some (Lex_result.token lex_result)
  | None -> None

let without_error_callback env = { env with error_callback = None }

let add_label env label = { env with labels = SSet.add label env.labels }

let enter_function env ~async ~generator =
  {
    env with
    in_formal_parameters = false;
    in_function = true;
    in_loop = false;
    in_switch = false;
    in_export = false;
    labels = SSet.empty;
    allow_await = async;
    allow_yield = generator;
  }

let is_keyword = function
  | "await"
  | "break"
  | "case"
  | "catch"
  | "class"
  | "const"
  | "continue"
  | "debugger"
  | "default"
  | "delete"
  | "do"
  | "else"
  | "export"
  | "extends"
  | "finally"
  | "for"
  | "function"
  | "if"
  | "import"
  | "in"
  | "instanceof"
  | "new"
  | "return"
  | "super"
  | "switch"
  | "this"
  | "throw"
  | "try"
  | "typeof"
  | "var"
  | "void"
  | "while"
  | "with"
  | "yield" ->
    true
  | _ -> false

let token_is_keyword =
  let open Token in
  function
  | T_IDENTIFIER { raw; _ } when is_keyword raw -> true
  | T_AWAIT
  | T_BREAK
  | T_CASE
  | T_CATCH
  | T_CLASS
  | T_CONST
  | T_CONTINUE
  | T_DEBUGGER
  | T_DEFAULT
  | T_DELETE
  | T_DO
  | T_ELSE
  | T_EXPORT
  | T_EXTENDS
  | T_FINALLY
  | T_FOR
  | T_FUNCTION
  | T_IF
  | T_IMPORT
  | T_IN
  | T_INSTANCEOF
  | T_NEW
  | T_RETURN
  | T_SUPER
  | T_SWITCH
  | T_THIS
  | T_THROW
  | T_TRY
  | T_TYPEOF
  | T_VAR
  | T_VOID
  | T_WHILE
  | T_WITH
  | T_YIELD ->
    true
  | _ -> false

let is_future_reserved = function
  | "enum" -> true
  | _ -> false

let token_is_future_reserved =
  let open Token in
  function
  | T_IDENTIFIER { raw; _ } when is_future_reserved raw -> true
  | T_ENUM -> true
  | _ -> false

let is_strict_reserved = function
  | "interface"
  | "implements"
  | "package"
  | "private"
  | "protected"
  | "public"
  | "static"
  | "yield" ->
    true
  | _ -> false

let token_is_strict_reserved =
  let open Token in
  function
  | T_IDENTIFIER { raw; _ } when is_strict_reserved raw -> true
  | T_INTERFACE
  | T_IMPLEMENTS
  | T_PACKAGE
  | T_PRIVATE
  | T_PROTECTED
  | T_PUBLIC
  | T_STATIC
  | T_YIELD ->
    true
  | _ -> false

let is_restricted = function
  | "eval"
  | "arguments" ->
    true
  | _ -> false

let token_is_restricted =
  let open Token in
  function
  | T_IDENTIFIER { raw; _ } when is_restricted raw -> true
  | _ -> false

let is_reserved str_val =
  is_keyword str_val
  || is_future_reserved str_val
  ||
  match str_val with
  | "null"
  | "true"
  | "false" ->
    true
  | _ -> false

let token_is_reserved t =
  token_is_keyword t
  || token_is_future_reserved t
  ||
  match t with
  | Token.T_IDENTIFIER { raw = "null" | "true" | "false"; _ }
  | Token.T_NULL
  | Token.T_TRUE
  | Token.T_FALSE ->
    true
  | _ -> false

let is_reserved_type str_val =
  match str_val with
  | "any"
  | "bool"
  | "boolean"
  | "empty"
  | "false"
  | "mixed"
  | "null"
  | "number"
  | "bigint"
  | "static"
  | "string"
  | "true"
  | "typeof"
  | "void"
  | "interface"
  | "extends"
  | "_" ->
    true
  | _ -> false

module Peek = struct
  open Loc
  open Token

  let ith_token ~i env = Lex_result.token (lookahead ~i env)

  let ith_loc ~i env = Lex_result.loc (lookahead ~i env)

  let ith_errors ~i env = Lex_result.errors (lookahead ~i env)

  let ith_comments ~i env =
    let comments = Lex_result.comments (lookahead ~i env) in
    match comments with
    | [] -> []
    | _ ->
      List.filter
        (fun ({ Loc.start; _ }, _) -> Loc.pos_cmp !(env.consumed_comments_pos) start <= 0)
        comments

  let token env = ith_token ~i:0 env

  let loc env = ith_loc ~i:0 env

  let loc_skip_lookahead env =
    let loc =
      match last_loc env with
      | Some loc -> loc
      | None -> failwith "Peeking current location when not available"
    in
    let open Loc in
    { loc with start = loc._end }

  let errors env = ith_errors ~i:0 env

  let comments env = ith_comments ~i:0 env

  let has_eaten_comments env =
    let comments = Lex_result.comments (lookahead ~i:0 env) in
    List.exists
      (fun ({ Loc.start; _ }, _) -> Loc.pos_cmp start !(env.consumed_comments_pos) < 0)
      comments

  let lex_env env = Lookahead.lex_env_0 !(env.lookahead)

  let ith_is_line_terminator ~i env =
    let loc =
      if i > 0 then
        Some (ith_loc ~i:(i - 1) env)
      else
        last_loc env
    in
    match loc with
    | None -> false
    | Some loc' -> (ith_loc ~i env).start.line > loc'.start.line

  let is_line_terminator env = ith_is_line_terminator ~i:0 env

  let ith_is_implicit_semicolon ~i env =
    match ith_token ~i env with
    | T_EOF
    | T_RCURLY ->
      true
    | T_SEMICOLON -> false
    | _ -> ith_is_line_terminator ~i env

  let is_implicit_semicolon env = ith_is_implicit_semicolon ~i:0 env

  let ith_is_identifier ~i env =
    match ith_token ~i env with
    | t when token_is_strict_reserved t -> true
    | t when token_is_future_reserved t -> true
    | t when token_is_restricted t -> true
    | T_LET
    | T_TYPE
    | T_OPAQUE
    | T_OF
    | T_DECLARE
    | T_ASYNC
    | T_AWAIT
    | T_POUND
    | T_IDENTIFIER _ ->
      true
    | _ -> false

  let ith_is_type_identifier ~i env =
    match lex_mode env with
    | Lex_mode.TYPE ->
      (match ith_token ~i env with
      | T_IDENTIFIER _ -> true
      | _ -> false)
    | Lex_mode.NORMAL ->
      (match ith_token ~i env with
      | T_IDENTIFIER { raw; _ } when is_reserved_type raw -> false
      | T_ANY_TYPE
      | T_MIXED_TYPE
      | T_EMPTY_TYPE
      | T_NUMBER_TYPE
      | T_BIGINT_TYPE
      | T_STRING_TYPE
      | T_VOID_TYPE
      | T_SYMBOL_TYPE
      | T_BOOLEAN_TYPE _
      | T_NUMBER_SINGLETON_TYPE _
      | T_BIGINT_SINGLETON_TYPE _
      | T_ASYNC
      | T_AWAIT
      | T_BREAK
      | T_CASE
      | T_CATCH
      | T_CLASS
      | T_CONST
      | T_CONTINUE
      | T_DEBUGGER
      | T_DECLARE
      | T_DEFAULT
      | T_DELETE
      | T_DO
      | T_ELSE
      | T_ENUM
      | T_EXPORT
      | T_EXTENDS
      | T_FALSE
      | T_FINALLY
      | T_FOR
      | T_FUNCTION
      | T_IDENTIFIER _
      | T_IF
      | T_IMPLEMENTS
      | T_IMPORT
      | T_IN
      | T_INSTANCEOF
      | T_INTERFACE
      | T_LET
      | T_NEW
      | T_NULL
      | T_OF
      | T_OPAQUE
      | T_PACKAGE
      | T_PRIVATE
      | T_PROTECTED
      | T_PUBLIC
      | T_RETURN
      | T_SUPER
      | T_SWITCH
      | T_THIS
      | T_THROW
      | T_TRUE
      | T_TRY
      | T_TYPE
      | T_VAR
      | T_WHILE
      | T_WITH
      | T_YIELD ->
        true
      | T_STATIC
      | T_TYPEOF
      | T_VOID ->
        false
      | T_LCURLY
      | T_RCURLY
      | T_LCURLYBAR
      | T_RCURLYBAR
      | T_LPAREN
      | T_RPAREN
      | T_LBRACKET
      | T_RBRACKET
      | T_SEMICOLON
      | T_COMMA
      | T_PERIOD
      | T_ARROW
      | T_ELLIPSIS
      | T_AT
      | T_POUND
      | T_CHECKS
      | T_RSHIFT3_ASSIGN
      | T_RSHIFT_ASSIGN
      | T_LSHIFT_ASSIGN
      | T_BIT_XOR_ASSIGN
      | T_BIT_OR_ASSIGN
      | T_BIT_AND_ASSIGN
      | T_MOD_ASSIGN
      | T_DIV_ASSIGN
      | T_MULT_ASSIGN
      | T_EXP_ASSIGN
      | T_MINUS_ASSIGN
      | T_PLUS_ASSIGN
      | T_ASSIGN
      | T_PLING_PERIOD
      | T_PLING_PLING
      | T_PLING
      | T_COLON
      | T_OR
      | T_AND
      | T_BIT_OR
      | T_BIT_XOR
      | T_BIT_AND
      | T_EQUAL
      | T_NOT_EQUAL
      | T_STRICT_EQUAL
      | T_STRICT_NOT_EQUAL
      | T_LESS_THAN_EQUAL
      | T_GREATER_THAN_EQUAL
      | T_LESS_THAN
      | T_GREATER_THAN
      | T_LSHIFT
      | T_RSHIFT
      | T_RSHIFT3
      | T_PLUS
      | T_MINUS
      | T_DIV
      | T_MULT
      | T_EXP
      | T_MOD
      | T_NOT
      | T_BIT_NOT
      | T_INCR
      | T_DECR
      | T_EOF ->
        false
      | T_NUMBER _
      | T_BIGINT _
      | T_STRING _
      | T_TEMPLATE_PART _
      | T_REGEXP _
      | T_JSX_IDENTIFIER _
      | T_JSX_TEXT _
      | T_ERROR _ ->
        false)
    | Lex_mode.JSX_TAG
    | Lex_mode.JSX_CHILD
    | Lex_mode.TEMPLATE
    | Lex_mode.REGEXP ->
      false

  let ith_is_identifier_name ~i env = ith_is_identifier ~i env || ith_is_type_identifier ~i env

  let is_identifier env = ith_is_identifier ~i:0 env

  let is_identifier_name env = ith_is_identifier_name ~i:0 env

  let is_type_identifier env = ith_is_type_identifier ~i:0 env

  let is_function env =
    token env = T_FUNCTION
    || token env = T_ASYNC
       && ith_token ~i:1 env = T_FUNCTION
       && (loc env)._end.line = (ith_loc ~i:1 env).start.line

  let is_class env =
    match token env with
    | T_CLASS
    | T_AT ->
      true
    | _ -> false
end

let error env e =
  let loc = Peek.loc env in
  error_at env (loc, e)

let get_unexpected_error ?expected token =
  if token_is_future_reserved token then
    Parse_error.UnexpectedReserved
  else if token_is_strict_reserved token then
    Parse_error.StrictReservedWord
  else
    let unexpected = Token.explanation_of_token token in
    match expected with
    | Some expected_msg -> Parse_error.UnexpectedWithExpected (unexpected, expected_msg)
    | None -> Parse_error.Unexpected unexpected

let error_unexpected ?expected env =
  error_list env (Peek.errors env);
  error env (get_unexpected_error ?expected (Peek.token env))

let error_on_decorators env =
  List.iter (fun decorator -> error_at env (fst decorator, Parse_error.UnsupportedDecorator))

let strict_error env e = if in_strict_mode env then error env e

let strict_error_at env (loc, e) = if in_strict_mode env then error_at env (loc, e)

let function_as_statement_error_at env loc =
  error_at env (loc, Parse_error.FunctionAsStatement { in_strict_mode = in_strict_mode env })

module Eat = struct
  let token env =
    (match !(env.token_sink) with
    | None -> ()
    | Some token_sink ->
      token_sink { token_loc = Peek.loc env; token = Peek.token env; token_context = lex_mode env });
    env.lex_env := Peek.lex_env env;
    error_list env (Peek.errors env);
    env.comments := List.rev_append (Lex_result.comments (lookahead ~i:0 env)) !(env.comments);
    env.last_lex_result := Some (lookahead ~i:0 env);
    Lookahead.junk !(env.lookahead)

  let maybe env t =
    if Token.equal (Peek.token env) t then (
      token env;
      true
    ) else
      false
    [@@ocaml.doc
      " [maybe env t] eats the next token and returns [true] if it is [t], else return [false] "]

  let push_lex_mode env mode =
    env.lex_mode_stack := mode :: !(env.lex_mode_stack);
    env.lookahead := Lookahead.create !(env.lex_env) (lex_mode env)

  let pop_lex_mode env =
    let new_stack =
      match !(env.lex_mode_stack) with
      | _mode :: stack -> stack
      | _ -> failwith "Popping lex mode from empty stack"
    in
    env.lex_mode_stack := new_stack;
    env.lookahead := Lookahead.create !(env.lex_env) (lex_mode env)

  let double_pop_lex_mode env =
    let new_stack =
      match !(env.lex_mode_stack) with
      | _ :: _ :: stack -> stack
      | _ -> failwith "Popping lex mode from empty stack"
    in
    env.lex_mode_stack := new_stack;
    env.lookahead := Lookahead.create !(env.lex_env) (lex_mode env)

  let trailing_comments env =
    let open Loc in
    let loc = Peek.loc env in
    if Peek.token env = Token.T_COMMA && Peek.ith_is_line_terminator ~i:1 env then (
      let trailing_before_comma = Peek.comments env in
      let trailing_after_comma =
        List.filter
          (fun (comment_loc, _) -> comment_loc.start.line <= loc._end.line)
          (Lex_result.comments (lookahead ~i:1 env))
      in
      let trailing = trailing_before_comma @ trailing_after_comma in
      consume_comments_until env { Loc.line = loc._end.line + 1; column = 0 };
      trailing
    ) else
      let trailing = Peek.comments env in
      consume_comments_until env loc._end;
      trailing

  let comments_until_next_line env =
    let open Loc in
    match !(env.last_lex_result) with
    | None -> []
    | Some { Lex_result.lex_loc = last_loc; _ } ->
      let comments = Peek.comments env in
      let comments = List.filter (fun (loc, _) -> loc.start.line <= last_loc._end.line) comments in
      consume_comments_until env { line = last_loc._end.line + 1; column = 0 };
      comments

  let program_comments env =
    let open Flow_ast.Comment in
    let comments = Peek.comments env in
    let flow_directive = "@flow" in
    let flow_directive_length = String.length flow_directive in
    let contains_flow_directive { text; _ } =
      let text_length = String.length text in
      let rec contains_flow_directive_after_offset off =
        if off + flow_directive_length > text_length then
          false
        else
          String.sub text off flow_directive_length = flow_directive
          || contains_flow_directive_after_offset (off + 1)
      in
      contains_flow_directive_after_offset 0
    in
    let rec flow_directive_comments comments =
      match comments with
      | [] -> []
      | (loc, comment) :: rest ->
        if contains_flow_directive comment then (
          (env.consumed_comments_pos :=
             let open Loc in
             loc._end);
          List.rev ((loc, comment) :: rest)
        ) else
          flow_directive_comments rest
    in
    let program_comments = flow_directive_comments (List.rev comments) in
    let program_comments =
      if program_comments <> [] then
        program_comments
      else
        match comments with
        | ((loc, { kind = Block; text; _ }) as first_comment) :: _
          when String.length text >= 1 && text.[0] = '*' ->
          (env.consumed_comments_pos :=
             let open Loc in
             loc._end);
          [first_comment]
        | _ -> []
    in
    program_comments
end

module Expect = struct
  let error env t =
    let expected = Token.explanation_of_token ~use_article:true t in
    error_unexpected ~expected env

  let token env t =
    if not (Token.equal (Peek.token env) t) then error env t;
    Eat.token env

  let token_opt env t =
    if not (Token.equal (Peek.token env) t) then
      error env t
    else
      Eat.token env
    [@@ocaml.doc
      " [token_opt env T_FOO] eats a token if it is [T_FOO], and errors without consuming if not.\n      This differs from [token], which always consumes. Only use [token_opt] when it's ok for\n      the parser to not advance, like if you are guaranteed that something else has eaten a\n      token. "]

  let identifier env name =
    let t = Peek.token env in
    (match t with
    | Token.T_IDENTIFIER { raw; _ } when raw = name -> ()
    | _ ->
      let expected = Printf.sprintf "the identifier `%s`" name in
      error_unexpected ~expected env);
    Eat.token env
end

module Try = struct
  type 'a parse_result =
    | ParsedSuccessfully of 'a
    | FailedToParse

  exception Rollback

  type saved_state = {
    saved_errors: (Loc.t * Parse_error.t) list;
    saved_comments: Loc.t Flow_ast.Comment.t list;
    saved_last_lex_result: Lex_result.t option;
    saved_lex_mode_stack: Lex_mode.t list;
    saved_lex_env: Lex_env.t;
    saved_consumed_comments_pos: Loc.position;
    token_buffer: ((token_sink_result -> unit) * token_sink_result Queue.t) option;
  }

  let save_state env =
    let token_buffer =
      match !(env.token_sink) with
      | None -> None
      | Some orig_token_sink ->
        let buffer = Queue.create () in
        env.token_sink := Some (fun token_data -> Queue.add token_data buffer);
        Some (orig_token_sink, buffer)
    in
    {
      saved_errors = !(env.errors);
      saved_comments = !(env.comments);
      saved_last_lex_result = !(env.last_lex_result);
      saved_lex_mode_stack = !(env.lex_mode_stack);
      saved_lex_env = !(env.lex_env);
      saved_consumed_comments_pos = !(env.consumed_comments_pos);
      token_buffer;
    }

  let reset_token_sink ~flush env token_buffer_info =
    match token_buffer_info with
    | None -> ()
    | Some (orig_token_sink, token_buffer) ->
      env.token_sink := Some orig_token_sink;
      if flush then Queue.iter orig_token_sink token_buffer

  let rollback_state env saved_state =
    reset_token_sink ~flush:false env saved_state.token_buffer;
    env.errors := saved_state.saved_errors;
    env.comments := saved_state.saved_comments;
    env.last_lex_result := saved_state.saved_last_lex_result;
    env.lex_mode_stack := saved_state.saved_lex_mode_stack;
    env.lex_env := saved_state.saved_lex_env;
    env.consumed_comments_pos := saved_state.saved_consumed_comments_pos;
    env.lookahead := Lookahead.create !(env.lex_env) (lex_mode env);
    FailedToParse

  let success env saved_state result =
    reset_token_sink ~flush:true env saved_state.token_buffer;
    ParsedSuccessfully result

  let to_parse env parse =
    let saved_state = save_state env in
    try success env saved_state (parse env) with
    | Rollback -> rollback_state env saved_state

  let or_else env ~fallback parse =
    match to_parse env parse with
    | ParsedSuccessfully result -> result
    | FailedToParse -> fallback
end
