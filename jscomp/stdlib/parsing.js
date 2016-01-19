// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Lexing = require("./lexing");
var Caml_lexer = require("../runtime/caml_lexer");
var Caml_primitive = require("../runtime/caml_primitive");
var Caml_array = require("../runtime/caml_array");
var $$Array = require("./array");

var YYexit = [
  248,
  "Parsing.YYexit",
  ++ Caml_exceptions.caml_oo_last_id
];

var Parse_error = [
  248,
  "Parsing.Parse_error",
  ++ Caml_exceptions.caml_oo_last_id
];

var env = [
  /* record */0,
  Caml_array.caml_make_vect(100, 0),
  Caml_array.caml_make_vect(100, /* () */0),
  Caml_array.caml_make_vect(100, Lexing.dummy_pos),
  Caml_array.caml_make_vect(100, Lexing.dummy_pos),
  100,
  0,
  0,
  /* () */0,
  Lexing.dummy_pos,
  Lexing.dummy_pos,
  0,
  0,
  0,
  0,
  0,
  0
];

function grow_stacks() {
  var oldsize = env[5];
  var newsize = oldsize * 2;
  var new_s = Caml_array.caml_make_vect(newsize, 0);
  var new_v = Caml_array.caml_make_vect(newsize, /* () */0);
  var new_start = Caml_array.caml_make_vect(newsize, Lexing.dummy_pos);
  var new_end = Caml_array.caml_make_vect(newsize, Lexing.dummy_pos);
  $$Array.blit(env[1], 0, new_s, 0, oldsize);
  env[1] = new_s;
  $$Array.blit(env[2], 0, new_v, 0, oldsize);
  env[2] = new_v;
  $$Array.blit(env[3], 0, new_start, 0, oldsize);
  env[3] = new_start;
  $$Array.blit(env[4], 0, new_end, 0, oldsize);
  env[4] = new_end;
  env[5] = newsize;
  return /* () */0;
}

function clear_parser() {
  $$Array.fill(env[2], 0, env[5], /* () */0);
  env[8] = /* () */0;
  return /* () */0;
}

var current_lookahead_fun = [
  0,
  function () {
    return /* false */0;
  }
];

function yyparse(tables, start, lexer, lexbuf) {
  var loop = function (_cmd, _arg) {
    while(/* true */1) {
      var arg = _arg;
      var cmd = _cmd;
      var match = Caml_lexer.caml_parse_engine(tables, env, cmd, arg);
      switch (match) {
        case 0 : 
            var t = lexer(lexbuf);
            env[9] = lexbuf[11];
            env[10] = lexbuf[12];
            _arg = t;
            _cmd = /* Token_read */1;
            break;
        case 1 : 
            throw Parse_error;
        case 2 : 
            grow_stacks(/* () */0);
            _arg = /* () */0;
            _cmd = /* Stacks_grown_1 */2;
            break;
        case 3 : 
            grow_stacks(/* () */0);
            _arg = /* () */0;
            _cmd = /* Stacks_grown_2 */3;
            break;
        case 4 : 
            var match$1;
            try {
              match$1 = [
                /* tuple */0,
                /* Semantic_action_computed */4,
                tables[1][env[13]](env)
              ];
            }
            catch (exn){
              if (exn === Parse_error) {
                match$1 = [
                  /* tuple */0,
                  /* Error_detected */5,
                  /* () */0
                ];
              }
              else {
                throw exn;
              }
            }
            _arg = match$1[2];
            _cmd = match$1[1];
            break;
        case 5 : 
            tables[14]("syntax error");
            _arg = /* () */0;
            _cmd = /* Error_detected */5;
            break;
        
      }
    };
  };
  var init_asp = env[11];
  var init_sp = env[14];
  var init_stackbase = env[6];
  var init_state = env[15];
  var init_curr_char = env[7];
  var init_lval = env[8];
  var init_errflag = env[16];
  env[6] = env[14] + 1;
  env[7] = start;
  env[10] = lexbuf[12];
  try {
    return loop(/* Start */0, /* () */0);
  }
  catch (exn){
    var curr_char = env[7];
    env[11] = init_asp;
    env[14] = init_sp;
    env[6] = init_stackbase;
    env[15] = init_state;
    env[7] = init_curr_char;
    env[8] = init_lval;
    env[16] = init_errflag;
    if (exn[1] === YYexit) {
      return exn[2];
    }
    else {
      current_lookahead_fun[1] = function (tok) {
        if (Caml_obj_runtime.caml_obj_is_block(tok)) {
          return +(tables[3][Caml_obj_runtime.caml_obj_tag(tok)] === curr_char);
        }
        else {
          return +(tables[2][tok] === curr_char);
        }
      };
      throw exn;
    }
  }
}

function peek_val(env, n) {
  return env[2][env[11] - n];
}

function symbol_start_pos() {
  var loop = function (_i) {
    while(/* true */1) {
      var i = _i;
      if (i <= 0) {
        return env[4][env[11]];
      }
      else {
        var st = env[3][env[11] - i + 1];
        var en = env[4][env[11] - i + 1];
        if (Caml_primitive.caml_notequal(st, en)) {
          return st;
        }
        else {
          _i = i - 1;
        }
      }
    };
  };
  return loop(env[12]);
}

function symbol_end_pos() {
  return env[4][env[11]];
}

function rhs_start_pos(n) {
  return env[3][env[11] - (env[12] - n)];
}

function rhs_end_pos(n) {
  return env[4][env[11] - (env[12] - n)];
}

function symbol_start() {
  return symbol_start_pos(/* () */0)[4];
}

function symbol_end() {
  return symbol_end_pos(/* () */0)[4];
}

function rhs_start(n) {
  return rhs_start_pos(n)[4];
}

function rhs_end(n) {
  return rhs_end_pos(n)[4];
}

function is_current_lookahead(tok) {
  return current_lookahead_fun[1](tok);
}

function parse_error() {
  return /* () */0;
}

function set_trace(prim) {
  return Caml_lexer.caml_set_parser_trace(prim);
}

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
