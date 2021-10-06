(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Sedlexing = Flow_sedlexing

type bol = {
  line: int;
  offset: int;
}

type lex_state = { lex_errors_acc: (Loc.t * Parse_error.t) list } [@@ocaml.unboxed]

type t = {
  lex_source: File_key.t option;
  lex_lb: Sedlexing.lexbuf;
  lex_bol: bol;
  lex_in_comment_syntax: bool;
  lex_enable_comment_syntax: bool;
  lex_state: lex_state;
  lex_last_loc: Loc.t;
}

let empty_lex_state = { lex_errors_acc = [] }

let initial_last_loc =
  { Loc.source = None; start = { Loc.line = 1; column = 0 }; _end = { Loc.line = 1; column = 0 } }

let new_lex_env lex_source lex_lb ~enable_types_in_comments =
  {
    lex_source;
    lex_lb;
    lex_bol = { line = 1; offset = 0 };
    lex_in_comment_syntax = false;
    lex_enable_comment_syntax = enable_types_in_comments;
    lex_state = empty_lex_state;
    lex_last_loc = initial_last_loc;
  }

let clone env =
  let lex_lb = Sedlexing.lexbuf_clone env.lex_lb in
  { env with lex_lb }

let lexbuf env = env.lex_lb

let source env = env.lex_source

let state env = env.lex_state

let line env = env.lex_bol.line

let bol_offset env = env.lex_bol.offset

let is_in_comment_syntax env = env.lex_in_comment_syntax

let is_comment_syntax_enabled env = env.lex_enable_comment_syntax

let in_comment_syntax is_in env =
  if is_in <> env.lex_in_comment_syntax then
    { env with lex_in_comment_syntax = is_in }
  else
    env

let debug_string_of_lexbuf _lb = ""

let debug_string_of_lex_env (env : t) =
  let source =
    match source env with
    | None -> "None"
    | Some x -> Printf.sprintf "Some %S" (File_key.to_string x)
  in
  Printf.sprintf
    "{\n  lex_source = %s\n  lex_lb = %s\n  lex_in_comment_syntax = %b\n  lex_enable_comment_syntax = %b\n  lex_state = {errors = (count = %d)}\n}"
    source
    (debug_string_of_lexbuf env.lex_lb)
    (is_in_comment_syntax env)
    (is_comment_syntax_enabled env)
    (List.length (state env).lex_errors_acc)
