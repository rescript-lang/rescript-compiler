/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/* The parsing engine */

open Lexing

/* Internal interface to the parsing engine */

type parser_env = {
  mutable s_stack: array<int> /* States */,
  mutable v_stack: array<Obj.t> /* Semantic attributes */,
  mutable symb_start_stack: array<position> /* Start positions */,
  mutable symb_end_stack: array<position> /* End positions */,
  mutable stacksize: int /* Size of the stacks */,
  mutable stackbase: int /* Base sp for current parse */,
  mutable curr_char: int /* Last token read */,
  mutable lval: Obj.t /* Its semantic attribute */,
  mutable symb_start: position /* Start pos. of the current symbol */,
  mutable symb_end: position /* End pos. of the current symbol */,
  mutable asp: int /* The stack pointer for attributes */,
  mutable rule_len: int /* Number of rhs items in the rule */,
  mutable rule_number: int /* Rule number to reduce by */,
  mutable sp: int /* Saved sp for parse_engine */,
  mutable state: int /* Saved state for parse_engine */,
  mutable errflag: int,
} /* Saved error flag for parse_engine */

type parse_tables = {
  actions: array<parser_env => Obj.t>,
  transl_const: array<int>,
  transl_block: array<int>,
  lhs: string,
  len: string,
  defred: string,
  dgoto: string,
  sindex: string,
  rindex: string,
  gindex: string,
  tablesize: int,
  table: string,
  check: string,
  error_function: string => unit,
  names_const: string,
  names_block: string,
}

exception YYexit(Obj.t)
exception Parse_error

type parser_input =
  | Start
  | Token_read
  | Stacks_grown_1
  | Stacks_grown_2
  | Semantic_action_computed
  | Error_detected

type parser_output =
  | Read_token
  | Raise_parse_error
  | Grow_stacks_1
  | Grow_stacks_2
  | Compute_semantic_action
  | Call_error_function

/* to avoid warnings */
let _ = list{
  Read_token,
  Raise_parse_error,
  Grow_stacks_1,
  Grow_stacks_2,
  Compute_semantic_action,
  Call_error_function,
}

external parse_engine: (parse_tables, parser_env, parser_input, Obj.t) => parser_output =
  "?parse_engine"

external set_trace: bool => bool = "?set_parser_trace"

let env = {
  s_stack: Array.make(100, 0),
  v_stack: Array.make(100, Obj.repr()),
  symb_start_stack: Array.make(100, dummy_pos),
  symb_end_stack: Array.make(100, dummy_pos),
  stacksize: 100,
  stackbase: 0,
  curr_char: 0,
  lval: Obj.repr(),
  symb_start: dummy_pos,
  symb_end: dummy_pos,
  asp: 0,
  rule_len: 0,
  rule_number: 0,
  sp: 0,
  state: 0,
  errflag: 0,
}

let grow_stacks = () => {
  let oldsize = env.stacksize
  let newsize = oldsize * 2
  let new_s = Array.make(newsize, 0)
  and new_v = Array.make(newsize, Obj.repr())
  and new_start = Array.make(newsize, dummy_pos)
  and new_end = Array.make(newsize, dummy_pos)
  Array.blit(env.s_stack, 0, new_s, 0, oldsize)
  env.s_stack = new_s
  Array.blit(env.v_stack, 0, new_v, 0, oldsize)
  env.v_stack = new_v
  Array.blit(env.symb_start_stack, 0, new_start, 0, oldsize)
  env.symb_start_stack = new_start
  Array.blit(env.symb_end_stack, 0, new_end, 0, oldsize)
  env.symb_end_stack = new_end
  env.stacksize = newsize
}

let clear_parser = () => {
  Array.fill(env.v_stack, 0, env.stacksize, Obj.repr())
  env.lval = Obj.repr()
}

let current_lookahead_fun = ref((_: Obj.t) => false)

let yyparse = (tables, start, lexer, lexbuf) => {
  let rec loop = (cmd, arg) =>
    switch parse_engine(tables, env, cmd, arg) {
    | Read_token =>
      let t = Obj.repr(lexer(lexbuf))
      env.symb_start = lexbuf.lex_start_p
      env.symb_end = lexbuf.lex_curr_p
      loop(Token_read, t)
    | Raise_parse_error => raise(Parse_error)
    | Compute_semantic_action =>
      let (action, value) = try (
        Semantic_action_computed,
        tables.actions[env.rule_number](env),
      ) catch {
      | Parse_error => (Error_detected, Obj.repr())
      }
      loop(action, value)
    | Grow_stacks_1 =>
      grow_stacks()
      loop(Stacks_grown_1, Obj.repr())
    | Grow_stacks_2 =>
      grow_stacks()
      loop(Stacks_grown_2, Obj.repr())
    | Call_error_function =>
      tables.error_function("syntax error")
      loop(Error_detected, Obj.repr())
    }
  let init_asp = env.asp
  and init_sp = env.sp
  and init_stackbase = env.stackbase
  and init_state = env.state
  and init_curr_char = env.curr_char
  and init_lval = env.lval
  and init_errflag = env.errflag
  env.stackbase = env.sp + 1
  env.curr_char = start
  env.symb_end = lexbuf.lex_curr_p
  try loop(Start, Obj.repr()) catch {
  | exn =>
    let curr_char = env.curr_char
    env.asp = init_asp
    env.sp = init_sp
    env.stackbase = init_stackbase
    env.state = init_state
    env.curr_char = init_curr_char
    env.lval = init_lval
    env.errflag = init_errflag
    switch exn {
    | YYexit(v) => Obj.magic(v)
    | _ =>
      current_lookahead_fun :=
        (
          tok =>
            if Js.typeof(tok) != "number" {
              tables.transl_block[Obj.tag(tok)] == curr_char
            } else {
              tables.transl_const[Obj.magic(tok)] == curr_char
            }
        )
      raise(exn)
    }
  }
}

let peek_val = (env, n) => Obj.magic(env.v_stack[env.asp - n])

let symbol_start_pos = () => {
  let rec loop = i =>
    if i <= 0 {
      env.symb_end_stack[env.asp]
    } else {
      let st = env.symb_start_stack[env.asp - i + 1]
      let en = env.symb_end_stack[env.asp - i + 1]
      if st != en {
        st
      } else {
        loop(i - 1)
      }
    }

  loop(env.rule_len)
}

let symbol_end_pos = () => env.symb_end_stack[env.asp]
let rhs_start_pos = n => env.symb_start_stack[env.asp - (env.rule_len - n)]
let rhs_end_pos = n => env.symb_end_stack[env.asp - (env.rule_len - n)]

let symbol_start = () => symbol_start_pos().pos_cnum
let symbol_end = () => symbol_end_pos().pos_cnum
let rhs_start = n => rhs_start_pos(n).pos_cnum
let rhs_end = n => rhs_end_pos(n).pos_cnum

let is_current_lookahead = tok => current_lookahead_fun.contents(Obj.repr(tok))

let parse_error = (_: string) => ()
