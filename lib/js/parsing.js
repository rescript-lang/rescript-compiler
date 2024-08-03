'use strict';

let $$Array = require("./array.js");
let Lexing = require("./lexing.js");
let Caml_obj = require("./caml_obj.js");
let Caml_array = require("./caml_array.js");
let Caml_parser = require("./caml_parser.js");
let Caml_exceptions = require("./caml_exceptions.js");
let Caml_js_exceptions = require("./caml_js_exceptions.js");

let YYexit = /* @__PURE__ */Caml_exceptions.create("Parsing.YYexit");

let Parse_error = /* @__PURE__ */Caml_exceptions.create("Parsing.Parse_error");

let env = {
  s_stack: Caml_array.make(100, 0),
  v_stack: Caml_array.make(100, undefined),
  symb_start_stack: Caml_array.make(100, Lexing.dummy_pos),
  symb_end_stack: Caml_array.make(100, Lexing.dummy_pos),
  stacksize: 100,
  stackbase: 0,
  curr_char: 0,
  lval: undefined,
  symb_start: Lexing.dummy_pos,
  symb_end: Lexing.dummy_pos,
  asp: 0,
  rule_len: 0,
  rule_number: 0,
  sp: 0,
  state: 0,
  errflag: 0
};

function grow_stacks() {
  let oldsize = env.stacksize;
  let newsize = (oldsize << 1);
  let new_s = Caml_array.make(newsize, 0);
  let new_v = Caml_array.make(newsize, undefined);
  let new_start = Caml_array.make(newsize, Lexing.dummy_pos);
  let new_end = Caml_array.make(newsize, Lexing.dummy_pos);
  $$Array.blit(env.s_stack, 0, new_s, 0, oldsize);
  env.s_stack = new_s;
  $$Array.blit(env.v_stack, 0, new_v, 0, oldsize);
  env.v_stack = new_v;
  $$Array.blit(env.symb_start_stack, 0, new_start, 0, oldsize);
  env.symb_start_stack = new_start;
  $$Array.blit(env.symb_end_stack, 0, new_end, 0, oldsize);
  env.symb_end_stack = new_end;
  env.stacksize = newsize;
}

function clear_parser() {
  $$Array.fill(env.v_stack, 0, env.stacksize, undefined);
  env.lval = undefined;
}

let current_lookahead_fun = {
  contents: (function (param) {
    return false;
  })
};

function yyparse(tables, start, lexer, lexbuf) {
  let init_asp = env.asp;
  let init_sp = env.sp;
  let init_stackbase = env.stackbase;
  let init_state = env.state;
  let init_curr_char = env.curr_char;
  let init_lval = env.lval;
  let init_errflag = env.errflag;
  env.stackbase = env.sp + 1 | 0;
  env.curr_char = start;
  env.symb_end = lexbuf.lex_curr_p;
  try {
    let _cmd = "Start";
    let _arg;
    while (true) {
      let arg = _arg;
      let cmd = _cmd;
      let match = Caml_parser.parse_engine(tables, env, cmd, arg);
      switch (match) {
        case "Read_token" :
            let t = lexer(lexbuf);
            env.symb_start = lexbuf.lex_start_p;
            env.symb_end = lexbuf.lex_curr_p;
            _arg = t;
            _cmd = "Token_read";
            continue;
        case "Raise_parse_error" :
            throw new Error(Parse_error, {
                  cause: {
                    RE_EXN_ID: Parse_error
                  }
                });
        case "Grow_stacks_1" :
            grow_stacks();
            _arg = undefined;
            _cmd = "Stacks_grown_1";
            continue;
        case "Grow_stacks_2" :
            grow_stacks();
            _arg = undefined;
            _cmd = "Stacks_grown_2";
            continue;
        case "Compute_semantic_action" :
            let match$1;
            try {
              match$1 = [
                "Semantic_action_computed",
                Caml_array.get(tables.actions, env.rule_number)(env)
              ];
            } catch (raw_exn) {
              let exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
              if (exn.RE_EXN_ID === Parse_error) {
                match$1 = [
                  "Error_detected",
                  undefined
                ];
              } else {
                throw new Error(exn.RE_EXN_ID, {
                      cause: exn
                    });
              }
            }
            _arg = match$1[1];
            _cmd = match$1[0];
            continue;
        case "Call_error_function" :
            tables.error_function("syntax error");
            _arg = undefined;
            _cmd = "Error_detected";
            continue;
        
      }
    };
  } catch (raw_exn$1) {
    let exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
    let curr_char = env.curr_char;
    env.asp = init_asp;
    env.sp = init_sp;
    env.stackbase = init_stackbase;
    env.state = init_state;
    env.curr_char = init_curr_char;
    env.lval = init_lval;
    env.errflag = init_errflag;
    if (exn$1.RE_EXN_ID === YYexit) {
      return exn$1._1;
    }
    current_lookahead_fun.contents = (function (tok) {
      if (typeof tok !== "number") {
        return Caml_array.get(tables.transl_block, tok.TAG) === curr_char;
      } else {
        return Caml_array.get(tables.transl_const, tok) === curr_char;
      }
    });
    throw new Error(exn$1.RE_EXN_ID, {
          cause: exn$1
        });
  }
}

function peek_val(env, n) {
  return Caml_array.get(env.v_stack, env.asp - n | 0);
}

function symbol_start_pos() {
  let _i = env.rule_len;
  while (true) {
    let i = _i;
    if (i <= 0) {
      return Caml_array.get(env.symb_end_stack, env.asp);
    }
    let st = Caml_array.get(env.symb_start_stack, (env.asp - i | 0) + 1 | 0);
    let en = Caml_array.get(env.symb_end_stack, (env.asp - i | 0) + 1 | 0);
    if (Caml_obj.notequal(st, en)) {
      return st;
    }
    _i = i - 1 | 0;
    continue;
  };
}

function symbol_end_pos() {
  return Caml_array.get(env.symb_end_stack, env.asp);
}

function rhs_start_pos(n) {
  return Caml_array.get(env.symb_start_stack, env.asp - (env.rule_len - n | 0) | 0);
}

function rhs_end_pos(n) {
  return Caml_array.get(env.symb_end_stack, env.asp - (env.rule_len - n | 0) | 0);
}

function symbol_start() {
  return symbol_start_pos().pos_cnum;
}

function symbol_end() {
  return symbol_end_pos().pos_cnum;
}

function rhs_start(n) {
  return rhs_start_pos(n).pos_cnum;
}

function rhs_end(n) {
  return rhs_end_pos(n).pos_cnum;
}

function is_current_lookahead(tok) {
  return current_lookahead_fun.contents(tok);
}

function parse_error(param) {
  
}

let set_trace = Caml_parser.set_parser_trace;

exports.symbol_start = symbol_start;
exports.symbol_end = symbol_end;
exports.rhs_start = rhs_start;
exports.rhs_end = rhs_end;
exports.symbol_start_pos = symbol_start_pos;
exports.symbol_end_pos = symbol_end_pos;
exports.rhs_start_pos = rhs_start_pos;
exports.rhs_end_pos = rhs_end_pos;
exports.clear_parser = clear_parser;
exports.Parse_error = Parse_error;
exports.set_trace = set_trace;
exports.YYexit = YYexit;
exports.yyparse = yyparse;
exports.peek_val = peek_val;
exports.is_current_lookahead = is_current_lookahead;
exports.parse_error = parse_error;
/* No side effect */
