'use strict';

var $$Array = require("./array.js");
var Curry = require("./curry.js");
var Lexing = require("./lexing.js");
var Caml_obj = require("./caml_obj.js");
var Caml_array = require("./caml_array.js");
var Caml_parser = require("./caml_parser.js");
var Caml_exceptions = require("./caml_exceptions.js");
var Caml_js_exceptions = require("./caml_js_exceptions.js");

var YYexit = Caml_exceptions.create("Parsing.YYexit");

var Parse_error = Caml_exceptions.create("Parsing.Parse_error");

var env = /* record */[
  /* s_stack */Caml_array.caml_make_vect(100, 0),
  /* v_stack */Caml_array.caml_make_vect(100, /* () */0),
  /* symb_start_stack */Caml_array.caml_make_vect(100, Lexing.dummy_pos),
  /* symb_end_stack */Caml_array.caml_make_vect(100, Lexing.dummy_pos),
  /* stacksize */100,
  /* stackbase */0,
  /* curr_char */0,
  /* lval : () */0,
  /* symb_start */Lexing.dummy_pos,
  /* symb_end */Lexing.dummy_pos,
  /* asp */0,
  /* rule_len */0,
  /* rule_number */0,
  /* sp */0,
  /* state */0,
  /* errflag */0
];

function grow_stacks(param) {
  var oldsize = env[/* stacksize */4];
  var newsize = (oldsize << 1);
  var new_s = Caml_array.caml_make_vect(newsize, 0);
  var new_v = Caml_array.caml_make_vect(newsize, /* () */0);
  var new_start = Caml_array.caml_make_vect(newsize, Lexing.dummy_pos);
  var new_end = Caml_array.caml_make_vect(newsize, Lexing.dummy_pos);
  $$Array.blit(env[/* s_stack */0], 0, new_s, 0, oldsize);
  env[/* s_stack */0] = new_s;
  $$Array.blit(env[/* v_stack */1], 0, new_v, 0, oldsize);
  env[/* v_stack */1] = new_v;
  $$Array.blit(env[/* symb_start_stack */2], 0, new_start, 0, oldsize);
  env[/* symb_start_stack */2] = new_start;
  $$Array.blit(env[/* symb_end_stack */3], 0, new_end, 0, oldsize);
  env[/* symb_end_stack */3] = new_end;
  env[/* stacksize */4] = newsize;
  return /* () */0;
}

function clear_parser(param) {
  $$Array.fill(env[/* v_stack */1], 0, env[/* stacksize */4], /* () */0);
  env[/* lval */7] = /* () */0;
  return /* () */0;
}

var current_lookahead_fun = /* record */[/* contents */(function (x) {
      return false;
    })];

function yyparse(tables, start, lexer, lexbuf) {
  var init_asp = env[/* asp */10];
  var init_sp = env[/* sp */13];
  var init_stackbase = env[/* stackbase */5];
  var init_state = env[/* state */14];
  var init_curr_char = env[/* curr_char */6];
  var init_lval = env[/* lval */7];
  var init_errflag = env[/* errflag */15];
  env[/* stackbase */5] = env[/* sp */13] + 1 | 0;
  env[/* curr_char */6] = start;
  env[/* symb_end */9] = lexbuf[/* lex_curr_p */11];
  try {
    var _cmd = /* Start */0;
    var _arg = /* () */0;
    while(true) {
      var arg = _arg;
      var cmd = _cmd;
      var match = Caml_parser.caml_parse_engine(tables, env, cmd, arg);
      switch (match) {
        case 0 : 
            var t = Curry._1(lexer, lexbuf);
            env[/* symb_start */8] = lexbuf[/* lex_start_p */10];
            env[/* symb_end */9] = lexbuf[/* lex_curr_p */11];
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
                Curry._1(Caml_array.caml_array_get(tables[/* actions */0], env[/* rule_number */12]), env)
              ];
            }
            catch (exn){
              if (exn === Parse_error) {
                match$1 = /* tuple */[
                  /* Error_detected */5,
                  /* () */0
                ];
              } else {
                throw exn;
              }
            }
            _arg = match$1[1];
            _cmd = match$1[0];
            continue ;
        case 5 : 
            Curry._1(tables[/* error_function */13], "syntax error");
            _arg = /* () */0;
            _cmd = /* Error_detected */5;
            continue ;
        
      }
    };
  }
  catch (raw_exn){
    var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn);
    var curr_char = env[/* curr_char */6];
    env[/* asp */10] = init_asp;
    env[/* sp */13] = init_sp;
    env[/* stackbase */5] = init_stackbase;
    env[/* state */14] = init_state;
    env[/* curr_char */6] = init_curr_char;
    env[/* lval */7] = init_lval;
    env[/* errflag */15] = init_errflag;
    if (exn$1[0] === YYexit) {
      return exn$1[1];
    } else {
      current_lookahead_fun[0] = (function (tok) {
          if (typeof tok !== "number") {
            return Caml_array.caml_array_get(tables[/* transl_block */2], tok.tag | 0) === curr_char;
          } else {
            return Caml_array.caml_array_get(tables[/* transl_const */1], tok) === curr_char;
          }
        });
      throw exn$1;
    }
  }
}

function peek_val(env, n) {
  return Caml_array.caml_array_get(env[/* v_stack */1], env[/* asp */10] - n | 0);
}

function symbol_start_pos(param) {
  var _i = env[/* rule_len */11];
  while(true) {
    var i = _i;
    if (i <= 0) {
      return Caml_array.caml_array_get(env[/* symb_end_stack */3], env[/* asp */10]);
    } else {
      var st = Caml_array.caml_array_get(env[/* symb_start_stack */2], (env[/* asp */10] - i | 0) + 1 | 0);
      var en = Caml_array.caml_array_get(env[/* symb_end_stack */3], (env[/* asp */10] - i | 0) + 1 | 0);
      if (Caml_obj.caml_notequal(st, en)) {
        return st;
      } else {
        _i = i - 1 | 0;
        continue ;
      }
    }
  };
}

function symbol_end_pos(param) {
  return Caml_array.caml_array_get(env[/* symb_end_stack */3], env[/* asp */10]);
}

function rhs_start_pos(n) {
  return Caml_array.caml_array_get(env[/* symb_start_stack */2], env[/* asp */10] - (env[/* rule_len */11] - n | 0) | 0);
}

function rhs_end_pos(n) {
  return Caml_array.caml_array_get(env[/* symb_end_stack */3], env[/* asp */10] - (env[/* rule_len */11] - n | 0) | 0);
}

function symbol_start(param) {
  return symbol_start_pos(/* () */0)[/* pos_cnum */3];
}

function symbol_end(param) {
  return symbol_end_pos(/* () */0)[/* pos_cnum */3];
}

function rhs_start(n) {
  return rhs_start_pos(n)[/* pos_cnum */3];
}

function rhs_end(n) {
  return rhs_end_pos(n)[/* pos_cnum */3];
}

function is_current_lookahead(tok) {
  return Curry._1(current_lookahead_fun[0], tok);
}

function parse_error(msg) {
  return /* () */0;
}

var set_trace = Caml_parser.caml_set_parser_trace;

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
