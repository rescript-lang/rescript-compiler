(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open Primitive_deriving

type t =
  | T_NUMBER of {
      kind: number_type;
      raw: string;
    }
  | T_BIGINT of {
      kind: bigint_type;
      raw: string;
    }
  | T_STRING of (Loc.t * string * string * bool) (* loc, value, raw, octal *)
  | T_TEMPLATE_PART of (Loc.t * template_part * bool) (* loc, value, is_tail *)
  | T_IDENTIFIER of {
      loc: Loc.t;
      value: string;
      raw: string;
    }
  | T_REGEXP of Loc.t * string * string (* /pattern/flags *)
  (* Syntax *)
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
  (* Keywords *)
  | T_FUNCTION
  | T_IF
  | T_IN
  | T_INSTANCEOF
  | T_RETURN
  | T_SWITCH
  | T_THIS
  | T_THROW
  | T_TRY
  | T_VAR
  | T_WHILE
  | T_WITH
  | T_CONST
  | T_LET
  | T_NULL
  | T_FALSE
  | T_TRUE
  | T_BREAK
  | T_CASE
  | T_CATCH
  | T_CONTINUE
  | T_DEFAULT
  | T_DO
  | T_FINALLY
  | T_FOR
  | T_CLASS
  | T_EXTENDS
  | T_STATIC
  | T_ELSE
  | T_NEW
  | T_DELETE
  | T_TYPEOF
  | T_VOID
  | T_ENUM
  | T_EXPORT
  | T_IMPORT
  | T_SUPER
  | T_IMPLEMENTS
  | T_INTERFACE
  | T_PACKAGE
  | T_PRIVATE
  | T_PROTECTED
  | T_PUBLIC
  | T_YIELD
  | T_DEBUGGER
  | T_DECLARE
  | T_TYPE
  | T_OPAQUE
  | T_OF
  | T_ASYNC
  | T_AWAIT
  | T_CHECKS
  (* Operators *)
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
  | T_NULLISH_ASSIGN
  | T_AND_ASSIGN
  | T_OR_ASSIGN
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
  (* Extra tokens *)
  | T_ERROR of string
  | T_EOF
  (* JSX *)
  | T_JSX_IDENTIFIER of {
      raw: string;
      loc: Loc.t;
    }
  | T_JSX_TEXT of Loc.t * string * string (* loc, value, raw *)
  (* Type primitives *)
  | T_ANY_TYPE
  | T_MIXED_TYPE
  | T_EMPTY_TYPE
  | T_BOOLEAN_TYPE of bool_or_boolean
  | T_NUMBER_TYPE
  | T_BIGINT_TYPE
  | T_NUMBER_SINGLETON_TYPE of {
      kind: number_type;
      value: float;
      raw: string;
    }
  | T_BIGINT_SINGLETON_TYPE of {
      kind: bigint_type;
      value: int64 option;
      raw: string;
    }
  | T_STRING_TYPE
  | T_VOID_TYPE
  | T_SYMBOL_TYPE

(* `bool` and `boolean` are equivalent annotations, but we need to track
   which one was used for when it might be an identifier, as in
   `(bool: boolean) => void`. It's lexed as two T_BOOLEAN_TYPEs, then the
   first one is converted into an identifier. *)
and bool_or_boolean =
  | BOOL
  | BOOLEAN

and number_type =
  | BINARY
  | LEGACY_OCTAL
  | LEGACY_NON_OCTAL (* NonOctalDecimalIntegerLiteral in Annex B *)
  | OCTAL
  | NORMAL

and bigint_type =
  | BIG_BINARY
  | BIG_OCTAL
  | BIG_NORMAL

and template_part = {
  cooked: string;
  (* string after processing special chars *)
  raw: string;
  (* string as specified in source *)
  literal: string; (* same as raw, plus characters like ` and ${ *)
}
[@@deriving_inline equal]
let _ = fun (_ : t) -> ()
let _ = fun (_ : bool_or_boolean) -> ()
let _ = fun (_ : number_type) -> ()
let _ = fun (_ : bigint_type) -> ()
let _ = fun (_ : template_part) -> ()
let rec equal =
  (fun a__001_ ->
     fun b__002_ ->
       if Ppx_compare_lib.phys_equal a__001_ b__002_
       then true
       else
         (match (a__001_, b__002_) with
          | (T_NUMBER _a__003_, T_NUMBER _b__004_) ->
              Ppx_compare_lib.(&&)
                (equal_number_type _a__003_.kind _b__004_.kind)
                (equal_string _a__003_.raw _b__004_.raw)
          | (T_NUMBER _, _) -> false
          | (_, T_NUMBER _) -> false
          | (T_BIGINT _a__005_, T_BIGINT _b__006_) ->
              Ppx_compare_lib.(&&)
                (equal_bigint_type _a__005_.kind _b__006_.kind)
                (equal_string _a__005_.raw _b__006_.raw)
          | (T_BIGINT _, _) -> false
          | (_, T_BIGINT _) -> false
          | (T_STRING _a__007_, T_STRING _b__008_) ->
              let (t__009_, t__010_, t__011_, t__012_) = _a__007_ in
              let (t__013_, t__014_, t__015_, t__016_) = _b__008_ in
              Ppx_compare_lib.(&&) (Loc.equal t__009_ t__013_)
                (Ppx_compare_lib.(&&) (equal_string t__010_ t__014_)
                   (Ppx_compare_lib.(&&) (equal_string t__011_ t__015_)
                      (equal_bool t__012_ t__016_)))
          | (T_STRING _, _) -> false
          | (_, T_STRING _) -> false
          | (T_TEMPLATE_PART _a__017_, T_TEMPLATE_PART _b__018_) ->
              let (t__019_, t__020_, t__021_) = _a__017_ in
              let (t__022_, t__023_, t__024_) = _b__018_ in
              Ppx_compare_lib.(&&) (Loc.equal t__019_ t__022_)
                (Ppx_compare_lib.(&&) (equal_template_part t__020_ t__023_)
                   (equal_bool t__021_ t__024_))
          | (T_TEMPLATE_PART _, _) -> false
          | (_, T_TEMPLATE_PART _) -> false
          | (T_IDENTIFIER _a__025_, T_IDENTIFIER _b__026_) ->
              Ppx_compare_lib.(&&) (Loc.equal _a__025_.loc _b__026_.loc)
                (Ppx_compare_lib.(&&)
                   (equal_string _a__025_.value _b__026_.value)
                   (equal_string _a__025_.raw _b__026_.raw))
          | (T_IDENTIFIER _, _) -> false
          | (_, T_IDENTIFIER _) -> false
          | (T_REGEXP (_a__027_, _a__029_, _a__031_), T_REGEXP
             (_b__028_, _b__030_, _b__032_)) ->
              Ppx_compare_lib.(&&) (Loc.equal _a__027_ _b__028_)
                (Ppx_compare_lib.(&&) (equal_string _a__029_ _b__030_)
                   (equal_string _a__031_ _b__032_))
          | (T_REGEXP _, _) -> false
          | (_, T_REGEXP _) -> false
          | (T_LCURLY, T_LCURLY) -> true
          | (T_LCURLY, _) -> false
          | (_, T_LCURLY) -> false
          | (T_RCURLY, T_RCURLY) -> true
          | (T_RCURLY, _) -> false
          | (_, T_RCURLY) -> false
          | (T_LCURLYBAR, T_LCURLYBAR) -> true
          | (T_LCURLYBAR, _) -> false
          | (_, T_LCURLYBAR) -> false
          | (T_RCURLYBAR, T_RCURLYBAR) -> true
          | (T_RCURLYBAR, _) -> false
          | (_, T_RCURLYBAR) -> false
          | (T_LPAREN, T_LPAREN) -> true
          | (T_LPAREN, _) -> false
          | (_, T_LPAREN) -> false
          | (T_RPAREN, T_RPAREN) -> true
          | (T_RPAREN, _) -> false
          | (_, T_RPAREN) -> false
          | (T_LBRACKET, T_LBRACKET) -> true
          | (T_LBRACKET, _) -> false
          | (_, T_LBRACKET) -> false
          | (T_RBRACKET, T_RBRACKET) -> true
          | (T_RBRACKET, _) -> false
          | (_, T_RBRACKET) -> false
          | (T_SEMICOLON, T_SEMICOLON) -> true
          | (T_SEMICOLON, _) -> false
          | (_, T_SEMICOLON) -> false
          | (T_COMMA, T_COMMA) -> true
          | (T_COMMA, _) -> false
          | (_, T_COMMA) -> false
          | (T_PERIOD, T_PERIOD) -> true
          | (T_PERIOD, _) -> false
          | (_, T_PERIOD) -> false
          | (T_ARROW, T_ARROW) -> true
          | (T_ARROW, _) -> false
          | (_, T_ARROW) -> false
          | (T_ELLIPSIS, T_ELLIPSIS) -> true
          | (T_ELLIPSIS, _) -> false
          | (_, T_ELLIPSIS) -> false
          | (T_AT, T_AT) -> true
          | (T_AT, _) -> false
          | (_, T_AT) -> false
          | (T_POUND, T_POUND) -> true
          | (T_POUND, _) -> false
          | (_, T_POUND) -> false
          | (T_FUNCTION, T_FUNCTION) -> true
          | (T_FUNCTION, _) -> false
          | (_, T_FUNCTION) -> false
          | (T_IF, T_IF) -> true
          | (T_IF, _) -> false
          | (_, T_IF) -> false
          | (T_IN, T_IN) -> true
          | (T_IN, _) -> false
          | (_, T_IN) -> false
          | (T_INSTANCEOF, T_INSTANCEOF) -> true
          | (T_INSTANCEOF, _) -> false
          | (_, T_INSTANCEOF) -> false
          | (T_RETURN, T_RETURN) -> true
          | (T_RETURN, _) -> false
          | (_, T_RETURN) -> false
          | (T_SWITCH, T_SWITCH) -> true
          | (T_SWITCH, _) -> false
          | (_, T_SWITCH) -> false
          | (T_THIS, T_THIS) -> true
          | (T_THIS, _) -> false
          | (_, T_THIS) -> false
          | (T_THROW, T_THROW) -> true
          | (T_THROW, _) -> false
          | (_, T_THROW) -> false
          | (T_TRY, T_TRY) -> true
          | (T_TRY, _) -> false
          | (_, T_TRY) -> false
          | (T_VAR, T_VAR) -> true
          | (T_VAR, _) -> false
          | (_, T_VAR) -> false
          | (T_WHILE, T_WHILE) -> true
          | (T_WHILE, _) -> false
          | (_, T_WHILE) -> false
          | (T_WITH, T_WITH) -> true
          | (T_WITH, _) -> false
          | (_, T_WITH) -> false
          | (T_CONST, T_CONST) -> true
          | (T_CONST, _) -> false
          | (_, T_CONST) -> false
          | (T_LET, T_LET) -> true
          | (T_LET, _) -> false
          | (_, T_LET) -> false
          | (T_NULL, T_NULL) -> true
          | (T_NULL, _) -> false
          | (_, T_NULL) -> false
          | (T_FALSE, T_FALSE) -> true
          | (T_FALSE, _) -> false
          | (_, T_FALSE) -> false
          | (T_TRUE, T_TRUE) -> true
          | (T_TRUE, _) -> false
          | (_, T_TRUE) -> false
          | (T_BREAK, T_BREAK) -> true
          | (T_BREAK, _) -> false
          | (_, T_BREAK) -> false
          | (T_CASE, T_CASE) -> true
          | (T_CASE, _) -> false
          | (_, T_CASE) -> false
          | (T_CATCH, T_CATCH) -> true
          | (T_CATCH, _) -> false
          | (_, T_CATCH) -> false
          | (T_CONTINUE, T_CONTINUE) -> true
          | (T_CONTINUE, _) -> false
          | (_, T_CONTINUE) -> false
          | (T_DEFAULT, T_DEFAULT) -> true
          | (T_DEFAULT, _) -> false
          | (_, T_DEFAULT) -> false
          | (T_DO, T_DO) -> true
          | (T_DO, _) -> false
          | (_, T_DO) -> false
          | (T_FINALLY, T_FINALLY) -> true
          | (T_FINALLY, _) -> false
          | (_, T_FINALLY) -> false
          | (T_FOR, T_FOR) -> true
          | (T_FOR, _) -> false
          | (_, T_FOR) -> false
          | (T_CLASS, T_CLASS) -> true
          | (T_CLASS, _) -> false
          | (_, T_CLASS) -> false
          | (T_EXTENDS, T_EXTENDS) -> true
          | (T_EXTENDS, _) -> false
          | (_, T_EXTENDS) -> false
          | (T_STATIC, T_STATIC) -> true
          | (T_STATIC, _) -> false
          | (_, T_STATIC) -> false
          | (T_ELSE, T_ELSE) -> true
          | (T_ELSE, _) -> false
          | (_, T_ELSE) -> false
          | (T_NEW, T_NEW) -> true
          | (T_NEW, _) -> false
          | (_, T_NEW) -> false
          | (T_DELETE, T_DELETE) -> true
          | (T_DELETE, _) -> false
          | (_, T_DELETE) -> false
          | (T_TYPEOF, T_TYPEOF) -> true
          | (T_TYPEOF, _) -> false
          | (_, T_TYPEOF) -> false
          | (T_VOID, T_VOID) -> true
          | (T_VOID, _) -> false
          | (_, T_VOID) -> false
          | (T_ENUM, T_ENUM) -> true
          | (T_ENUM, _) -> false
          | (_, T_ENUM) -> false
          | (T_EXPORT, T_EXPORT) -> true
          | (T_EXPORT, _) -> false
          | (_, T_EXPORT) -> false
          | (T_IMPORT, T_IMPORT) -> true
          | (T_IMPORT, _) -> false
          | (_, T_IMPORT) -> false
          | (T_SUPER, T_SUPER) -> true
          | (T_SUPER, _) -> false
          | (_, T_SUPER) -> false
          | (T_IMPLEMENTS, T_IMPLEMENTS) -> true
          | (T_IMPLEMENTS, _) -> false
          | (_, T_IMPLEMENTS) -> false
          | (T_INTERFACE, T_INTERFACE) -> true
          | (T_INTERFACE, _) -> false
          | (_, T_INTERFACE) -> false
          | (T_PACKAGE, T_PACKAGE) -> true
          | (T_PACKAGE, _) -> false
          | (_, T_PACKAGE) -> false
          | (T_PRIVATE, T_PRIVATE) -> true
          | (T_PRIVATE, _) -> false
          | (_, T_PRIVATE) -> false
          | (T_PROTECTED, T_PROTECTED) -> true
          | (T_PROTECTED, _) -> false
          | (_, T_PROTECTED) -> false
          | (T_PUBLIC, T_PUBLIC) -> true
          | (T_PUBLIC, _) -> false
          | (_, T_PUBLIC) -> false
          | (T_YIELD, T_YIELD) -> true
          | (T_YIELD, _) -> false
          | (_, T_YIELD) -> false
          | (T_DEBUGGER, T_DEBUGGER) -> true
          | (T_DEBUGGER, _) -> false
          | (_, T_DEBUGGER) -> false
          | (T_DECLARE, T_DECLARE) -> true
          | (T_DECLARE, _) -> false
          | (_, T_DECLARE) -> false
          | (T_TYPE, T_TYPE) -> true
          | (T_TYPE, _) -> false
          | (_, T_TYPE) -> false
          | (T_OPAQUE, T_OPAQUE) -> true
          | (T_OPAQUE, _) -> false
          | (_, T_OPAQUE) -> false
          | (T_OF, T_OF) -> true
          | (T_OF, _) -> false
          | (_, T_OF) -> false
          | (T_ASYNC, T_ASYNC) -> true
          | (T_ASYNC, _) -> false
          | (_, T_ASYNC) -> false
          | (T_AWAIT, T_AWAIT) -> true
          | (T_AWAIT, _) -> false
          | (_, T_AWAIT) -> false
          | (T_CHECKS, T_CHECKS) -> true
          | (T_CHECKS, _) -> false
          | (_, T_CHECKS) -> false
          | (T_RSHIFT3_ASSIGN, T_RSHIFT3_ASSIGN) -> true
          | (T_RSHIFT3_ASSIGN, _) -> false
          | (_, T_RSHIFT3_ASSIGN) -> false
          | (T_RSHIFT_ASSIGN, T_RSHIFT_ASSIGN) -> true
          | (T_RSHIFT_ASSIGN, _) -> false
          | (_, T_RSHIFT_ASSIGN) -> false
          | (T_LSHIFT_ASSIGN, T_LSHIFT_ASSIGN) -> true
          | (T_LSHIFT_ASSIGN, _) -> false
          | (_, T_LSHIFT_ASSIGN) -> false
          | (T_BIT_XOR_ASSIGN, T_BIT_XOR_ASSIGN) -> true
          | (T_BIT_XOR_ASSIGN, _) -> false
          | (_, T_BIT_XOR_ASSIGN) -> false
          | (T_BIT_OR_ASSIGN, T_BIT_OR_ASSIGN) -> true
          | (T_BIT_OR_ASSIGN, _) -> false
          | (_, T_BIT_OR_ASSIGN) -> false
          | (T_BIT_AND_ASSIGN, T_BIT_AND_ASSIGN) -> true
          | (T_BIT_AND_ASSIGN, _) -> false
          | (_, T_BIT_AND_ASSIGN) -> false
          | (T_MOD_ASSIGN, T_MOD_ASSIGN) -> true
          | (T_MOD_ASSIGN, _) -> false
          | (_, T_MOD_ASSIGN) -> false
          | (T_DIV_ASSIGN, T_DIV_ASSIGN) -> true
          | (T_DIV_ASSIGN, _) -> false
          | (_, T_DIV_ASSIGN) -> false
          | (T_MULT_ASSIGN, T_MULT_ASSIGN) -> true
          | (T_MULT_ASSIGN, _) -> false
          | (_, T_MULT_ASSIGN) -> false
          | (T_EXP_ASSIGN, T_EXP_ASSIGN) -> true
          | (T_EXP_ASSIGN, _) -> false
          | (_, T_EXP_ASSIGN) -> false
          | (T_MINUS_ASSIGN, T_MINUS_ASSIGN) -> true
          | (T_MINUS_ASSIGN, _) -> false
          | (_, T_MINUS_ASSIGN) -> false
          | (T_PLUS_ASSIGN, T_PLUS_ASSIGN) -> true
          | (T_PLUS_ASSIGN, _) -> false
          | (_, T_PLUS_ASSIGN) -> false
          | (T_NULLISH_ASSIGN, T_NULLISH_ASSIGN) -> true
          | (T_NULLISH_ASSIGN, _) -> false
          | (_, T_NULLISH_ASSIGN) -> false
          | (T_AND_ASSIGN, T_AND_ASSIGN) -> true
          | (T_AND_ASSIGN, _) -> false
          | (_, T_AND_ASSIGN) -> false
          | (T_OR_ASSIGN, T_OR_ASSIGN) -> true
          | (T_OR_ASSIGN, _) -> false
          | (_, T_OR_ASSIGN) -> false
          | (T_ASSIGN, T_ASSIGN) -> true
          | (T_ASSIGN, _) -> false
          | (_, T_ASSIGN) -> false
          | (T_PLING_PERIOD, T_PLING_PERIOD) -> true
          | (T_PLING_PERIOD, _) -> false
          | (_, T_PLING_PERIOD) -> false
          | (T_PLING_PLING, T_PLING_PLING) -> true
          | (T_PLING_PLING, _) -> false
          | (_, T_PLING_PLING) -> false
          | (T_PLING, T_PLING) -> true
          | (T_PLING, _) -> false
          | (_, T_PLING) -> false
          | (T_COLON, T_COLON) -> true
          | (T_COLON, _) -> false
          | (_, T_COLON) -> false
          | (T_OR, T_OR) -> true
          | (T_OR, _) -> false
          | (_, T_OR) -> false
          | (T_AND, T_AND) -> true
          | (T_AND, _) -> false
          | (_, T_AND) -> false
          | (T_BIT_OR, T_BIT_OR) -> true
          | (T_BIT_OR, _) -> false
          | (_, T_BIT_OR) -> false
          | (T_BIT_XOR, T_BIT_XOR) -> true
          | (T_BIT_XOR, _) -> false
          | (_, T_BIT_XOR) -> false
          | (T_BIT_AND, T_BIT_AND) -> true
          | (T_BIT_AND, _) -> false
          | (_, T_BIT_AND) -> false
          | (T_EQUAL, T_EQUAL) -> true
          | (T_EQUAL, _) -> false
          | (_, T_EQUAL) -> false
          | (T_NOT_EQUAL, T_NOT_EQUAL) -> true
          | (T_NOT_EQUAL, _) -> false
          | (_, T_NOT_EQUAL) -> false
          | (T_STRICT_EQUAL, T_STRICT_EQUAL) -> true
          | (T_STRICT_EQUAL, _) -> false
          | (_, T_STRICT_EQUAL) -> false
          | (T_STRICT_NOT_EQUAL, T_STRICT_NOT_EQUAL) -> true
          | (T_STRICT_NOT_EQUAL, _) -> false
          | (_, T_STRICT_NOT_EQUAL) -> false
          | (T_LESS_THAN_EQUAL, T_LESS_THAN_EQUAL) -> true
          | (T_LESS_THAN_EQUAL, _) -> false
          | (_, T_LESS_THAN_EQUAL) -> false
          | (T_GREATER_THAN_EQUAL, T_GREATER_THAN_EQUAL) -> true
          | (T_GREATER_THAN_EQUAL, _) -> false
          | (_, T_GREATER_THAN_EQUAL) -> false
          | (T_LESS_THAN, T_LESS_THAN) -> true
          | (T_LESS_THAN, _) -> false
          | (_, T_LESS_THAN) -> false
          | (T_GREATER_THAN, T_GREATER_THAN) -> true
          | (T_GREATER_THAN, _) -> false
          | (_, T_GREATER_THAN) -> false
          | (T_LSHIFT, T_LSHIFT) -> true
          | (T_LSHIFT, _) -> false
          | (_, T_LSHIFT) -> false
          | (T_RSHIFT, T_RSHIFT) -> true
          | (T_RSHIFT, _) -> false
          | (_, T_RSHIFT) -> false
          | (T_RSHIFT3, T_RSHIFT3) -> true
          | (T_RSHIFT3, _) -> false
          | (_, T_RSHIFT3) -> false
          | (T_PLUS, T_PLUS) -> true
          | (T_PLUS, _) -> false
          | (_, T_PLUS) -> false
          | (T_MINUS, T_MINUS) -> true
          | (T_MINUS, _) -> false
          | (_, T_MINUS) -> false
          | (T_DIV, T_DIV) -> true
          | (T_DIV, _) -> false
          | (_, T_DIV) -> false
          | (T_MULT, T_MULT) -> true
          | (T_MULT, _) -> false
          | (_, T_MULT) -> false
          | (T_EXP, T_EXP) -> true
          | (T_EXP, _) -> false
          | (_, T_EXP) -> false
          | (T_MOD, T_MOD) -> true
          | (T_MOD, _) -> false
          | (_, T_MOD) -> false
          | (T_NOT, T_NOT) -> true
          | (T_NOT, _) -> false
          | (_, T_NOT) -> false
          | (T_BIT_NOT, T_BIT_NOT) -> true
          | (T_BIT_NOT, _) -> false
          | (_, T_BIT_NOT) -> false
          | (T_INCR, T_INCR) -> true
          | (T_INCR, _) -> false
          | (_, T_INCR) -> false
          | (T_DECR, T_DECR) -> true
          | (T_DECR, _) -> false
          | (_, T_DECR) -> false
          | (T_ERROR _a__033_, T_ERROR _b__034_) ->
              equal_string _a__033_ _b__034_
          | (T_ERROR _, _) -> false
          | (_, T_ERROR _) -> false
          | (T_EOF, T_EOF) -> true
          | (T_EOF, _) -> false
          | (_, T_EOF) -> false
          | (T_JSX_IDENTIFIER _a__035_, T_JSX_IDENTIFIER _b__036_) ->
              Ppx_compare_lib.(&&) (equal_string _a__035_.raw _b__036_.raw)
                (Loc.equal _a__035_.loc _b__036_.loc)
          | (T_JSX_IDENTIFIER _, _) -> false
          | (_, T_JSX_IDENTIFIER _) -> false
          | (T_JSX_TEXT (_a__037_, _a__039_, _a__041_), T_JSX_TEXT
             (_b__038_, _b__040_, _b__042_)) ->
              Ppx_compare_lib.(&&) (Loc.equal _a__037_ _b__038_)
                (Ppx_compare_lib.(&&) (equal_string _a__039_ _b__040_)
                   (equal_string _a__041_ _b__042_))
          | (T_JSX_TEXT _, _) -> false
          | (_, T_JSX_TEXT _) -> false
          | (T_ANY_TYPE, T_ANY_TYPE) -> true
          | (T_ANY_TYPE, _) -> false
          | (_, T_ANY_TYPE) -> false
          | (T_MIXED_TYPE, T_MIXED_TYPE) -> true
          | (T_MIXED_TYPE, _) -> false
          | (_, T_MIXED_TYPE) -> false
          | (T_EMPTY_TYPE, T_EMPTY_TYPE) -> true
          | (T_EMPTY_TYPE, _) -> false
          | (_, T_EMPTY_TYPE) -> false
          | (T_BOOLEAN_TYPE _a__043_, T_BOOLEAN_TYPE _b__044_) ->
              equal_bool_or_boolean _a__043_ _b__044_
          | (T_BOOLEAN_TYPE _, _) -> false
          | (_, T_BOOLEAN_TYPE _) -> false
          | (T_NUMBER_TYPE, T_NUMBER_TYPE) -> true
          | (T_NUMBER_TYPE, _) -> false
          | (_, T_NUMBER_TYPE) -> false
          | (T_BIGINT_TYPE, T_BIGINT_TYPE) -> true
          | (T_BIGINT_TYPE, _) -> false
          | (_, T_BIGINT_TYPE) -> false
          | (T_NUMBER_SINGLETON_TYPE _a__045_, T_NUMBER_SINGLETON_TYPE
             _b__046_) ->
              Ppx_compare_lib.(&&)
                (equal_number_type _a__045_.kind _b__046_.kind)
                (Ppx_compare_lib.(&&)
                   (equal_float _a__045_.value _b__046_.value)
                   (equal_string _a__045_.raw _b__046_.raw))
          | (T_NUMBER_SINGLETON_TYPE _, _) -> false
          | (_, T_NUMBER_SINGLETON_TYPE _) -> false
          | (T_BIGINT_SINGLETON_TYPE _a__047_, T_BIGINT_SINGLETON_TYPE
             _b__048_) ->
              Ppx_compare_lib.(&&)
                (equal_bigint_type _a__047_.kind _b__048_.kind)
                (Ppx_compare_lib.(&&)
                   (equal_option equal_int64 _a__047_.value _b__048_.value)
                   (equal_string _a__047_.raw _b__048_.raw))
          | (T_BIGINT_SINGLETON_TYPE _, _) -> false
          | (_, T_BIGINT_SINGLETON_TYPE _) -> false
          | (T_STRING_TYPE, T_STRING_TYPE) -> true
          | (T_STRING_TYPE, _) -> false
          | (_, T_STRING_TYPE) -> false
          | (T_VOID_TYPE, T_VOID_TYPE) -> true
          | (T_VOID_TYPE, _) -> false
          | (_, T_VOID_TYPE) -> false
          | (T_SYMBOL_TYPE, T_SYMBOL_TYPE) -> true) : t -> t -> bool)
and equal_bool_or_boolean =
  (fun a__051_ ->
     fun b__052_ -> Ppx_compare_lib.polymorphic_equal a__051_ b__052_ :
  bool_or_boolean -> bool_or_boolean -> bool)
and equal_number_type =
  (fun a__053_ ->
     fun b__054_ -> Ppx_compare_lib.polymorphic_equal a__053_ b__054_ :
  number_type -> number_type -> bool)
and equal_bigint_type =
  (fun a__055_ ->
     fun b__056_ -> Ppx_compare_lib.polymorphic_equal a__055_ b__056_ :
  bigint_type -> bigint_type -> bool)
and equal_template_part =
  (fun a__057_ ->
     fun b__058_ ->
       if Ppx_compare_lib.phys_equal a__057_ b__058_
       then true
       else
         Ppx_compare_lib.(&&) (equal_string a__057_.cooked b__058_.cooked)
           (Ppx_compare_lib.(&&) (equal_string a__057_.raw b__058_.raw)
              (equal_string a__057_.literal b__058_.literal)) : template_part
                                                                  ->
                                                                  template_part
                                                                    ->
                                                                    bool)
let _ = equal
and _ = equal_bool_or_boolean
and _ = equal_number_type
and _ = equal_bigint_type
and _ = equal_template_part
[@@@end]
(*****************************************************************************)
(* Pretty printer (pretty?) *)
(*****************************************************************************)
let token_to_string = function
  | T_NUMBER _ -> "T_NUMBER"
  | T_BIGINT _ -> "T_BIGINT"
  | T_STRING _ -> "T_STRING"
  | T_TEMPLATE_PART _ -> "T_TEMPLATE_PART"
  | T_IDENTIFIER _ -> "T_IDENTIFIER"
  | T_REGEXP _ -> "T_REGEXP"
  | T_FUNCTION -> "T_FUNCTION"
  | T_IF -> "T_IF"
  | T_IN -> "T_IN"
  | T_INSTANCEOF -> "T_INSTANCEOF"
  | T_RETURN -> "T_RETURN"
  | T_SWITCH -> "T_SWITCH"
  | T_THIS -> "T_THIS"
  | T_THROW -> "T_THROW"
  | T_TRY -> "T_TRY"
  | T_VAR -> "T_VAR"
  | T_WHILE -> "T_WHILE"
  | T_WITH -> "T_WITH"
  | T_CONST -> "T_CONST"
  | T_LET -> "T_LET"
  | T_NULL -> "T_NULL"
  | T_FALSE -> "T_FALSE"
  | T_TRUE -> "T_TRUE"
  | T_BREAK -> "T_BREAK"
  | T_CASE -> "T_CASE"
  | T_CATCH -> "T_CATCH"
  | T_CONTINUE -> "T_CONTINUE"
  | T_DEFAULT -> "T_DEFAULT"
  | T_DO -> "T_DO"
  | T_FINALLY -> "T_FINALLY"
  | T_FOR -> "T_FOR"
  | T_CLASS -> "T_CLASS"
  | T_EXTENDS -> "T_EXTENDS"
  | T_STATIC -> "T_STATIC"
  | T_ELSE -> "T_ELSE"
  | T_NEW -> "T_NEW"
  | T_DELETE -> "T_DELETE"
  | T_TYPEOF -> "T_TYPEOF"
  | T_VOID -> "T_VOID"
  | T_ENUM -> "T_ENUM"
  | T_EXPORT -> "T_EXPORT"
  | T_IMPORT -> "T_IMPORT"
  | T_SUPER -> "T_SUPER"
  | T_IMPLEMENTS -> "T_IMPLEMENTS"
  | T_INTERFACE -> "T_INTERFACE"
  | T_PACKAGE -> "T_PACKAGE"
  | T_PRIVATE -> "T_PRIVATE"
  | T_PROTECTED -> "T_PROTECTED"
  | T_PUBLIC -> "T_PUBLIC"
  | T_YIELD -> "T_YIELD"
  | T_DEBUGGER -> "T_DEBUGGER"
  | T_DECLARE -> "T_DECLARE"
  | T_TYPE -> "T_TYPE"
  | T_OPAQUE -> "T_OPAQUE"
  | T_OF -> "T_OF"
  | T_ASYNC -> "T_ASYNC"
  | T_AWAIT -> "T_AWAIT"
  | T_CHECKS -> "T_CHECKS"
  | T_LCURLY -> "T_LCURLY"
  | T_RCURLY -> "T_RCURLY"
  | T_LCURLYBAR -> "T_LCURLYBAR"
  | T_RCURLYBAR -> "T_RCURLYBAR"
  | T_LPAREN -> "T_LPAREN"
  | T_RPAREN -> "T_RPAREN"
  | T_LBRACKET -> "T_LBRACKET"
  | T_RBRACKET -> "T_RBRACKET"
  | T_SEMICOLON -> "T_SEMICOLON"
  | T_COMMA -> "T_COMMA"
  | T_PERIOD -> "T_PERIOD"
  | T_ARROW -> "T_ARROW"
  | T_ELLIPSIS -> "T_ELLIPSIS"
  | T_AT -> "T_AT"
  | T_POUND -> "T_POUND"
  | T_RSHIFT3_ASSIGN -> "T_RSHIFT3_ASSIGN"
  | T_RSHIFT_ASSIGN -> "T_RSHIFT_ASSIGN"
  | T_LSHIFT_ASSIGN -> "T_LSHIFT_ASSIGN"
  | T_BIT_XOR_ASSIGN -> "T_BIT_XOR_ASSIGN"
  | T_BIT_OR_ASSIGN -> "T_BIT_OR_ASSIGN"
  | T_BIT_AND_ASSIGN -> "T_BIT_AND_ASSIGN"
  | T_MOD_ASSIGN -> "T_MOD_ASSIGN"
  | T_DIV_ASSIGN -> "T_DIV_ASSIGN"
  | T_MULT_ASSIGN -> "T_MULT_ASSIGN"
  | T_EXP_ASSIGN -> "T_EXP_ASSIGN"
  | T_MINUS_ASSIGN -> "T_MINUS_ASSIGN"
  | T_PLUS_ASSIGN -> "T_PLUS_ASSIGN"
  | T_NULLISH_ASSIGN -> "T_NULLISH_ASSIGN"
  | T_AND_ASSIGN -> "T_AND_ASSIGN"
  | T_OR_ASSIGN -> "T_OR_ASSIGN"
  | T_ASSIGN -> "T_ASSIGN"
  | T_PLING_PERIOD -> "T_PLING_PERIOD"
  | T_PLING_PLING -> "T_PLING_PLING"
  | T_PLING -> "T_PLING"
  | T_COLON -> "T_COLON"
  | T_OR -> "T_OR"
  | T_AND -> "T_AND"
  | T_BIT_OR -> "T_BIT_OR"
  | T_BIT_XOR -> "T_BIT_XOR"
  | T_BIT_AND -> "T_BIT_AND"
  | T_EQUAL -> "T_EQUAL"
  | T_NOT_EQUAL -> "T_NOT_EQUAL"
  | T_STRICT_EQUAL -> "T_STRICT_EQUAL"
  | T_STRICT_NOT_EQUAL -> "T_STRICT_NOT_EQUAL"
  | T_LESS_THAN_EQUAL -> "T_LESS_THAN_EQUAL"
  | T_GREATER_THAN_EQUAL -> "T_GREATER_THAN_EQUAL"
  | T_LESS_THAN -> "T_LESS_THAN"
  | T_GREATER_THAN -> "T_GREATER_THAN"
  | T_LSHIFT -> "T_LSHIFT"
  | T_RSHIFT -> "T_RSHIFT"
  | T_RSHIFT3 -> "T_RSHIFT3"
  | T_PLUS -> "T_PLUS"
  | T_MINUS -> "T_MINUS"
  | T_DIV -> "T_DIV"
  | T_MULT -> "T_MULT"
  | T_EXP -> "T_EXP"
  | T_MOD -> "T_MOD"
  | T_NOT -> "T_NOT"
  | T_BIT_NOT -> "T_BIT_NOT"
  | T_INCR -> "T_INCR"
  | T_DECR -> "T_DECR"
  (* Extra tokens *)
  | T_ERROR _ -> "T_ERROR"
  | T_EOF -> "T_EOF"
  | T_JSX_IDENTIFIER _ -> "T_JSX_IDENTIFIER"
  | T_JSX_TEXT _ -> "T_JSX_TEXT"
  (* Type primitives *)
  | T_ANY_TYPE -> "T_ANY_TYPE"
  | T_MIXED_TYPE -> "T_MIXED_TYPE"
  | T_EMPTY_TYPE -> "T_EMPTY_TYPE"
  | T_BOOLEAN_TYPE _ -> "T_BOOLEAN_TYPE"
  | T_NUMBER_TYPE -> "T_NUMBER_TYPE"
  | T_BIGINT_TYPE -> "T_BIGINT_TYPE"
  | T_NUMBER_SINGLETON_TYPE _ -> "T_NUMBER_SINGLETON_TYPE"
  | T_BIGINT_SINGLETON_TYPE _ -> "T_BIGINT_SINGLETON_TYPE"
  | T_STRING_TYPE -> "T_STRING_TYPE"
  | T_VOID_TYPE -> "T_VOID_TYPE"
  | T_SYMBOL_TYPE -> "T_SYMBOL_TYPE"

let value_of_token = function
  | T_NUMBER { raw; _ } -> raw
  | T_BIGINT { raw; _ } -> raw
  | T_STRING (_, _, raw, _) -> raw
  | T_TEMPLATE_PART (_, { literal; _ }, _) -> literal
  | T_IDENTIFIER { raw; _ } -> raw
  | T_REGEXP (_, pattern, flags) -> "/" ^ pattern ^ "/" ^ flags
  | T_LCURLY -> "{"
  | T_RCURLY -> "}"
  | T_LCURLYBAR -> "{|"
  | T_RCURLYBAR -> "|}"
  | T_LPAREN -> "("
  | T_RPAREN -> ")"
  | T_LBRACKET -> "["
  | T_RBRACKET -> "]"
  | T_SEMICOLON -> ";"
  | T_COMMA -> ","
  | T_PERIOD -> "."
  | T_ARROW -> "=>"
  | T_ELLIPSIS -> "..."
  | T_AT -> "@"
  | T_POUND -> "#"
  | T_FUNCTION -> "function"
  | T_IF -> "if"
  | T_IN -> "in"
  | T_INSTANCEOF -> "instanceof"
  | T_RETURN -> "return"
  | T_SWITCH -> "switch"
  | T_THIS -> "this"
  | T_THROW -> "throw"
  | T_TRY -> "try"
  | T_VAR -> "var"
  | T_WHILE -> "while"
  | T_WITH -> "with"
  | T_CONST -> "const"
  | T_LET -> "let"
  | T_NULL -> "null"
  | T_FALSE -> "false"
  | T_TRUE -> "true"
  | T_BREAK -> "break"
  | T_CASE -> "case"
  | T_CATCH -> "catch"
  | T_CONTINUE -> "continue"
  | T_DEFAULT -> "default"
  | T_DO -> "do"
  | T_FINALLY -> "finally"
  | T_FOR -> "for"
  | T_CLASS -> "class"
  | T_EXTENDS -> "extends"
  | T_STATIC -> "static"
  | T_ELSE -> "else"
  | T_NEW -> "new"
  | T_DELETE -> "delete"
  | T_TYPEOF -> "typeof"
  | T_VOID -> "void"
  | T_ENUM -> "enum"
  | T_EXPORT -> "export"
  | T_IMPORT -> "import"
  | T_SUPER -> "super"
  | T_IMPLEMENTS -> "implements"
  | T_INTERFACE -> "interface"
  | T_PACKAGE -> "package"
  | T_PRIVATE -> "private"
  | T_PROTECTED -> "protected"
  | T_PUBLIC -> "public"
  | T_YIELD -> "yield"
  | T_DEBUGGER -> "debugger"
  | T_DECLARE -> "declare"
  | T_TYPE -> "type"
  | T_OPAQUE -> "opaque"
  | T_OF -> "of"
  | T_ASYNC -> "async"
  | T_AWAIT -> "await"
  | T_CHECKS -> "%checks"
  | T_RSHIFT3_ASSIGN -> ">>>="
  | T_RSHIFT_ASSIGN -> ">>="
  | T_LSHIFT_ASSIGN -> "<<="
  | T_BIT_XOR_ASSIGN -> "^="
  | T_BIT_OR_ASSIGN -> "|="
  | T_BIT_AND_ASSIGN -> "&="
  | T_MOD_ASSIGN -> "%="
  | T_DIV_ASSIGN -> "/="
  | T_MULT_ASSIGN -> "*="
  | T_EXP_ASSIGN -> "**="
  | T_MINUS_ASSIGN -> "-="
  | T_PLUS_ASSIGN -> "+="
  | T_NULLISH_ASSIGN -> "??="
  | T_AND_ASSIGN -> "&&="
  | T_OR_ASSIGN -> "||="
  | T_ASSIGN -> "="
  | T_PLING_PERIOD -> "?."
  | T_PLING_PLING -> "??"
  | T_PLING -> "?"
  | T_COLON -> ":"
  | T_OR -> "||"
  | T_AND -> "&&"
  | T_BIT_OR -> "|"
  | T_BIT_XOR -> "^"
  | T_BIT_AND -> "&"
  | T_EQUAL -> "=="
  | T_NOT_EQUAL -> "!="
  | T_STRICT_EQUAL -> "==="
  | T_STRICT_NOT_EQUAL -> "!=="
  | T_LESS_THAN_EQUAL -> "<="
  | T_GREATER_THAN_EQUAL -> ">="
  | T_LESS_THAN -> "<"
  | T_GREATER_THAN -> ">"
  | T_LSHIFT -> "<<"
  | T_RSHIFT -> ">>"
  | T_RSHIFT3 -> ">>>"
  | T_PLUS -> "+"
  | T_MINUS -> "-"
  | T_DIV -> "/"
  | T_MULT -> "*"
  | T_EXP -> "**"
  | T_MOD -> "%"
  | T_NOT -> "!"
  | T_BIT_NOT -> "~"
  | T_INCR -> "++"
  | T_DECR -> "--"
  (* Extra tokens *)
  | T_ERROR raw -> raw
  | T_EOF -> ""
  | T_JSX_IDENTIFIER { raw; _ } -> raw
  | T_JSX_TEXT (_, _, raw) -> raw
  (* Type primitives *)
  | T_ANY_TYPE -> "any"
  | T_MIXED_TYPE -> "mixed"
  | T_EMPTY_TYPE -> "empty"
  | T_BOOLEAN_TYPE kind -> begin
    match kind with
    | BOOL -> "bool"
    | BOOLEAN -> "boolean"
  end
  | T_NUMBER_TYPE -> "number"
  | T_BIGINT_TYPE -> "bigint"
  | T_NUMBER_SINGLETON_TYPE { raw; _ } -> raw
  | T_BIGINT_SINGLETON_TYPE { raw; _ } -> raw
  | T_STRING_TYPE -> "string"
  | T_VOID_TYPE -> "void"
  | T_SYMBOL_TYPE -> "symbol"

let quote_token_value value = Printf.sprintf "token `%s`" value

let explanation_of_token ?(use_article = false) token =
  let (value, article) =
    match token with
    | T_NUMBER_SINGLETON_TYPE _
    | T_NUMBER _ ->
      ("number", "a")
    | T_BIGINT_SINGLETON_TYPE _
    | T_BIGINT _ ->
      ("bigint", "a")
    | T_JSX_TEXT _
    | T_STRING _ ->
      ("string", "a")
    | T_TEMPLATE_PART _ -> ("template literal part", "a")
    | T_JSX_IDENTIFIER _
    | T_IDENTIFIER _ ->
      ("identifier", "an")
    | T_REGEXP _ -> ("regexp", "a")
    | T_EOF -> ("end of input", "the")
    | _ -> (quote_token_value (value_of_token token), "the")
  in
  if use_article then
    article ^ " " ^ value
  else
    value
