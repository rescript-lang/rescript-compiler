'use strict';

var $$Array = require("./array.js");
var Curry = require("./curry.js");
var Lexing = require("./lexing.js");
var Caml_obj = require("./caml_obj.js");
var Caml_array = require("./caml_array.js");
var Caml_parser = require("./caml_parser.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");

var YYexit = /* @__PURE__ */Caml_exceptions.create("Parsing.YYexit");

var Parse_error = /* @__PURE__ */Caml_exceptions.create("Parsing.Parse_error");

var env = {
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

function grow_stacks(param) {
  var oldsize = env.stacksize;
  var newsize = (oldsize << 1);
  var new_s = Caml_array.make(newsize, 0);
  var new_v = Caml_array.make(newsize, undefined);
  var new_start = Caml_array.make(newsize, Lexing.dummy_pos);
  var new_end = Caml_array.make(newsize, Lexing.dummy_pos);
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

function clear_parser(param) {
  $$Array.fill(env.v_stack, 0, env.stacksize, undefined);
  env.lval = undefined;
}

var current_lookahead_fun = {
  contents: (function (param) {
      return false;
    })
};

function yyparse(tables, start, lexer, lexbuf) {
  var init_asp = env.asp;
  var init_sp = env.sp;
  var init_stackbase = env.stackbase;
  var init_state = env.state;
  var init_curr_char = env.curr_char;
  var init_lval = env.lval;
  var init_errflag = env.errflag;
  env.stackbase = env.sp + 1 | 0;
  env.curr_char = start;
  env.symb_end = lexbuf.lex_curr_p;
  try {
    var _cmd = "Start";
    var _arg;
    while(true) {
      var arg = _arg;
      var cmd = _cmd;
      var match = Caml_parser.parse_engine(tables, env, cmd, arg);
      switch (match) {
        case "Read_token" :
            var t = Curry._1(lexer, lexbuf);
            env.symb_start = lexbuf.lex_start_p;
            env.symb_end = lexbuf.lex_curr_p;
            _arg = t;
            _cmd = "Token_read";
            continue ;
        case "Raise_parse_error" :
            throw {
                  RE_EXN_ID: Parse_error,
                  Error: new Error()
                };
        case "Grow_stacks_1" :
            grow_stacks();
            _arg = undefined;
            _cmd = "Stacks_grown_1";
            continue ;
        case "Grow_stacks_2" :
            grow_stacks();
            _arg = undefined;
            _cmd = "Stacks_grown_2";
            continue ;
        case "Compute_semantic_action" :
            var match$1;
            try {
              match$1 = [
                "Semantic_action_computed",
                Curry._1(Caml_array.get(tables.actions, env.rule_number), env)
              ];
            }
            catch (raw_exn){
              var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
              if (exn.RE_EXN_ID === Parse_error) {
                match$1 = [
                  "Error_detected",
                  undefined
                ];
              } else {
                throw exn;
              }
            }
            _arg = match$1[1];
            _cmd = match$1[0];
            continue ;
        case "Call_error_function" :
            Curry._1(tables.error_function, "syntax error");
            _arg = undefined;
            _cmd = "Error_detected";
            continue ;
        
      }
    };
  }
  catch (raw_exn$1){
    var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn$1);
    var curr_char = env.curr_char;
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
    throw exn$1;
  }
}

function peek_val(env, n) {
  return Caml_array.get(env.v_stack, env.asp - n | 0);
}

function symbol_start_pos(param) {
  var _i = env.rule_len;
  while(true) {
    var i = _i;
    if (i <= 0) {
      return Caml_array.get(env.symb_end_stack, env.asp);
    }
    var st = Caml_array.get(env.symb_start_stack, (env.asp - i | 0) + 1 | 0);
    var en = Caml_array.get(env.symb_end_stack, (env.asp - i | 0) + 1 | 0);
    if (Caml_obj.notequal(st, en)) {
      return st;
    }
    _i = i - 1 | 0;
    continue ;
  };
}

function symbol_end_pos(param) {
  return Caml_array.get(env.symb_end_stack, env.asp);
}

function rhs_start_pos(n) {
  return Caml_array.get(env.symb_start_stack, env.asp - (env.rule_len - n | 0) | 0);
}

function rhs_end_pos(n) {
  return Caml_array.get(env.symb_end_stack, env.asp - (env.rule_len - n | 0) | 0);
}

function symbol_start(param) {
  return symbol_start_pos().pos_cnum;
}

function symbol_end(param) {
  return symbol_end_pos().pos_cnum;
}

function rhs_start(n) {
  return rhs_start_pos(n).pos_cnum;
}

function rhs_end(n) {
  return rhs_end_pos(n).pos_cnum;
}

function is_current_lookahead(tok) {
  return Curry._1(current_lookahead_fun.contents, tok);
}

function parse_error(param) {
  
}

var set_trace = Caml_parser.set_parser_trace;

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
