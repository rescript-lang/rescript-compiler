

import * as $$Array from "./array.js";
import * as Curry from "./curry.js";
import * as Lexing from "./lexing.js";
import * as Caml_obj from "./caml_obj.js";
import * as Caml_array from "./caml_array.js";
import * as Caml_parser from "./caml_parser.js";
import * as Caml_exceptions from "./caml_exceptions.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";

var YYexit = Caml_exceptions.create("Parsing.YYexit");

var Parse_error = Caml_exceptions.create("Parsing.Parse_error");

var env = {
  s_stack: Caml_array.caml_make_vect(100, 0),
  v_stack: Caml_array.caml_make_vect(100, /* () */0),
  symb_start_stack: Caml_array.caml_make_vect(100, Lexing.dummy_pos),
  symb_end_stack: Caml_array.caml_make_vect(100, Lexing.dummy_pos),
  stacksize: 100,
  stackbase: 0,
  curr_char: 0,
  lval: /* () */0,
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
  var new_s = Caml_array.caml_make_vect(newsize, 0);
  var new_v = Caml_array.caml_make_vect(newsize, /* () */0);
  var new_start = Caml_array.caml_make_vect(newsize, Lexing.dummy_pos);
  var new_end = Caml_array.caml_make_vect(newsize, Lexing.dummy_pos);
  $$Array.blit(env.s_stack, 0, new_s, 0, oldsize);
  env.s_stack = new_s;
  $$Array.blit(env.v_stack, 0, new_v, 0, oldsize);
  env.v_stack = new_v;
  $$Array.blit(env.symb_start_stack, 0, new_start, 0, oldsize);
  env.symb_start_stack = new_start;
  $$Array.blit(env.symb_end_stack, 0, new_end, 0, oldsize);
  env.symb_end_stack = new_end;
  env.stacksize = newsize;
  return /* () */0;
}

function clear_parser(param) {
  $$Array.fill(env.v_stack, 0, env.stacksize, /* () */0);
  env.lval = /* () */0;
  return /* () */0;
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
    var _cmd = /* Start */0;
    var _arg = /* () */0;
    while(true) {
      var arg = _arg;
      var cmd = _cmd;
      var match = Caml_parser.caml_parse_engine(tables, env, cmd, arg);
      switch (match) {
        case /* Read_token */0 :
            var t = Curry._1(lexer, lexbuf);
            env.symb_start = lexbuf.lex_start_p;
            env.symb_end = lexbuf.lex_curr_p;
            _arg = t;
            _cmd = /* Token_read */1;
            continue ;
        case /* Raise_parse_error */1 :
            throw Parse_error;
        case /* Grow_stacks_1 */2 :
            grow_stacks(/* () */0);
            _arg = /* () */0;
            _cmd = /* Stacks_grown_1 */2;
            continue ;
        case /* Grow_stacks_2 */3 :
            grow_stacks(/* () */0);
            _arg = /* () */0;
            _cmd = /* Stacks_grown_2 */3;
            continue ;
        case /* Compute_semantic_action */4 :
            var match$1;
            try {
              match$1 = /* tuple */[
                /* Semantic_action_computed */4,
                Curry._1(Caml_array.caml_array_get(tables.actions, env.rule_number), env)
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
        case /* Call_error_function */5 :
            Curry._1(tables.error_function, "syntax error");
            _arg = /* () */0;
            _cmd = /* Error_detected */5;
            continue ;
        
      }
    };
  }
  catch (raw_exn){
    var exn$1 = Caml_js_exceptions.internalToOCamlException(raw_exn);
    var curr_char = env.curr_char;
    env.asp = init_asp;
    env.sp = init_sp;
    env.stackbase = init_stackbase;
    env.state = init_state;
    env.curr_char = init_curr_char;
    env.lval = init_lval;
    env.errflag = init_errflag;
    if (exn$1[0] === YYexit) {
      return exn$1[1];
    } else {
      current_lookahead_fun.contents = (function (tok) {
          if (typeof tok !== "number") {
            return Caml_array.caml_array_get(tables.transl_block, tok.tag | 0) === curr_char;
          } else {
            return Caml_array.caml_array_get(tables.transl_const, tok) === curr_char;
          }
        });
      throw exn$1;
    }
  }
}

function peek_val(env, n) {
  return Caml_array.caml_array_get(env.v_stack, env.asp - n | 0);
}

function symbol_start_pos(param) {
  var _i = env.rule_len;
  while(true) {
    var i = _i;
    if (i <= 0) {
      return Caml_array.caml_array_get(env.symb_end_stack, env.asp);
    } else {
      var st = Caml_array.caml_array_get(env.symb_start_stack, (env.asp - i | 0) + 1 | 0);
      var en = Caml_array.caml_array_get(env.symb_end_stack, (env.asp - i | 0) + 1 | 0);
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
  return Caml_array.caml_array_get(env.symb_end_stack, env.asp);
}

function rhs_start_pos(n) {
  return Caml_array.caml_array_get(env.symb_start_stack, env.asp - (env.rule_len - n | 0) | 0);
}

function rhs_end_pos(n) {
  return Caml_array.caml_array_get(env.symb_end_stack, env.asp - (env.rule_len - n | 0) | 0);
}

function symbol_start(param) {
  return symbol_start_pos(/* () */0).pos_cnum;
}

function symbol_end(param) {
  return symbol_end_pos(/* () */0).pos_cnum;
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
  return /* () */0;
}

var set_trace = Caml_parser.caml_set_parser_trace;

export {
  symbol_start ,
  symbol_end ,
  rhs_start ,
  rhs_end ,
  symbol_start_pos ,
  symbol_end_pos ,
  rhs_start_pos ,
  rhs_end_pos ,
  clear_parser ,
  Parse_error ,
  set_trace ,
  YYexit ,
  yyparse ,
  peek_val ,
  is_current_lookahead ,
  parse_error ,
  
}
/* No side effect */
