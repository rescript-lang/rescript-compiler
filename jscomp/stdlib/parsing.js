// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_obj                = require("../runtime/caml_obj");
var Caml_parser             = require("../runtime/caml_parser");
var Lexing                  = require("./lexing");
var Caml_array              = require("../runtime/caml_array");
var $$Array                 = require("./array");
var Caml_curry              = require("../runtime/caml_curry");

var YYexit = {
  0: "Parsing.YYexit",
  1: ++ Caml_builtin_exceptions.caml_oo_last_id,
  length: 2,
  tag: 248
};

var Parse_error = {
  0: "Parsing.Parse_error",
  1: ++ Caml_builtin_exceptions.caml_oo_last_id,
  length: 2,
  tag: 248
};

var env = /* record */[
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
  var oldsize = env[4];
  var newsize = oldsize * 2;
  var new_s = Caml_array.caml_make_vect(newsize, 0);
  var new_v = Caml_array.caml_make_vect(newsize, /* () */0);
  var new_start = Caml_array.caml_make_vect(newsize, Lexing.dummy_pos);
  var new_end = Caml_array.caml_make_vect(newsize, Lexing.dummy_pos);
  $$Array.blit(env[0], 0, new_s, 0, oldsize);
  env[0] = new_s;
  $$Array.blit(env[1], 0, new_v, 0, oldsize);
  env[1] = new_v;
  $$Array.blit(env[2], 0, new_start, 0, oldsize);
  env[2] = new_start;
  $$Array.blit(env[3], 0, new_end, 0, oldsize);
  env[3] = new_end;
  env[4] = newsize;
  return /* () */0;
}

function clear_parser() {
  $$Array.fill(env[1], 0, env[4], /* () */0);
  env[7] = /* () */0;
  return /* () */0;
}

var current_lookahead_fun = [function () {
    return /* false */0;
  }];

function yyparse(tables, start, lexer, lexbuf) {
  var init_asp = env[10];
  var init_sp = env[13];
  var init_stackbase = env[5];
  var init_state = env[14];
  var init_curr_char = env[6];
  var init_lval = env[7];
  var init_errflag = env[15];
  env[5] = env[13] + 1;
  env[6] = start;
  env[9] = lexbuf[11];
  try {
    var _cmd = /* Start */0;
    var _arg = /* () */0;
    while(true) {
      var arg = _arg;
      var cmd = _cmd;
      var match = Caml_parser.caml_parse_engine(tables, env, cmd, arg);
      switch (match) {
        case 0 : 
            var t = Caml_curry.app1(lexer, lexbuf);
            env[8] = lexbuf[10];
            env[9] = lexbuf[11];
            _arg = t;
            _cmd = /* Token_read */1;
            continue ;
            case 1 : 
            throw Parse_error;
        case 2 : 
            grow_stacks(/* () */0);
            _arg = /* () */0;
            _cmd = /* Stacks_grown_1 */2;
            continue ;
            case 3 : 
            grow_stacks(/* () */0);
            _arg = /* () */0;
            _cmd = /* Stacks_grown_2 */3;
            continue ;
            case 4 : 
            var match$1;
            try {
              match$1 = /* tuple */[
                /* Semantic_action_computed */4,
                Caml_curry.app1(tables[0][env[12]], env)
              ];
            }
            catch (exn){
              if (exn === Parse_error) {
                match$1 = /* tuple */[
                  /* Error_detected */5,
                  /* () */0
                ];
              }
              else {
                throw exn;
              }
            }
            _arg = match$1[1];
            _cmd = match$1[0];
            continue ;
            case 5 : 
            Caml_curry.app1(tables[13], "syntax error");
            _arg = /* () */0;
            _cmd = /* Error_detected */5;
            continue ;
            
      }
    };
  }
  catch (exn$1){
    var curr_char = env[6];
    env[10] = init_asp;
    env[13] = init_sp;
    env[5] = init_stackbase;
    env[14] = init_state;
    env[6] = init_curr_char;
    env[7] = init_lval;
    env[15] = init_errflag;
    if (exn$1[0] === YYexit) {
      return exn$1[1];
    }
    else {
      current_lookahead_fun[0] = function (tok) {
        if (Caml_obj.caml_obj_is_block(tok)) {
          return +(tables[2][tok.tag | 0] === curr_char);
        }
        else {
          return +(tables[1][tok] === curr_char);
        }
      };
      throw exn$1;
    }
  }
}

function peek_val(env, n) {
  return env[1][env[10] - n];
}

function symbol_start_pos() {
  var _i = env[11];
  while(true) {
    var i = _i;
    if (i <= 0) {
      return env[3][env[10]];
    }
    else {
      var st = env[2][env[10] - i + 1];
      var en = env[3][env[10] - i + 1];
      if (Caml_obj.caml_notequal(st, en)) {
        return st;
      }
      else {
        _i = i - 1;
        continue ;
        
      }
    }
  };
}

function symbol_end_pos() {
  return env[3][env[10]];
}

function rhs_start_pos(n) {
  return env[2][env[10] - (env[11] - n)];
}

function rhs_end_pos(n) {
  return env[3][env[10] - (env[11] - n)];
}

function symbol_start() {
  return symbol_start_pos(/* () */0)[3];
}

function symbol_end() {
  return symbol_end_pos(/* () */0)[3];
}

function rhs_start(n) {
  return rhs_start_pos(n)[3];
}

function rhs_end(n) {
  return rhs_end_pos(n)[3];
}

function is_current_lookahead(tok) {
  return Caml_curry.app1(current_lookahead_fun[0], tok);
}

function parse_error() {
  return /* () */0;
}

function set_trace(prim) {
  return Caml_parser.caml_set_parser_trace(prim);
}

exports.symbol_start         = symbol_start;
exports.symbol_end           = symbol_end;
exports.rhs_start            = rhs_start;
exports.rhs_end              = rhs_end;
exports.symbol_start_pos     = symbol_start_pos;
exports.symbol_end_pos       = symbol_end_pos;
exports.rhs_start_pos        = rhs_start_pos;
exports.rhs_end_pos          = rhs_end_pos;
exports.clear_parser         = clear_parser;
exports.Parse_error          = Parse_error;
exports.set_trace            = set_trace;
exports.YYexit               = YYexit;
exports.yyparse              = yyparse;
exports.peek_val             = peek_val;
exports.is_current_lookahead = is_current_lookahead;
exports.parse_error          = parse_error;
/* No side effect */
