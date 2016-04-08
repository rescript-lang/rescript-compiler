// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Bytes                   = require("./bytes");
var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Pervasives              = require("./pervasives");
var Caml_lexer              = require("../runtime/caml_lexer");
var Sys                     = require("./sys");
var Caml_curry              = require("../runtime/caml_curry");
var Caml_string             = require("../runtime/caml_string");

function engine(tbl, state, buf) {
  var result = Caml_lexer.caml_lex_engine(tbl, state, buf);
  if (result >= 0) {
    buf[/* lex_start_p */10] = buf[/* lex_curr_p */11];
    var init = buf[/* lex_curr_p */11];
    buf[/* lex_curr_p */11] = /* record */[
      init[/* pos_fname */0],
      init[/* pos_lnum */1],
      init[/* pos_bol */2],
      buf[/* lex_abs_pos */3] + buf[/* lex_curr_pos */5] | 0
    ];
  }
  return result;
}

function new_engine(tbl, state, buf) {
  var result = Caml_lexer.caml_new_lex_engine(tbl, state, buf);
  if (result >= 0) {
    buf[/* lex_start_p */10] = buf[/* lex_curr_p */11];
    var init = buf[/* lex_curr_p */11];
    buf[/* lex_curr_p */11] = /* record */[
      init[/* pos_fname */0],
      init[/* pos_lnum */1],
      init[/* pos_bol */2],
      buf[/* lex_abs_pos */3] + buf[/* lex_curr_pos */5] | 0
    ];
  }
  return result;
}

var zero_pos = /* record */[
  "",
  1,
  0,
  0
];

function from_function(f) {
  var partial_arg = new Array(512);
  return /* record */[
          function (param) {
            var read_fun = f;
            var aux_buffer = partial_arg;
            var lexbuf = param;
            var read = Caml_curry.app2(read_fun, aux_buffer, aux_buffer.length);
            var n = read > 0 ? read : (lexbuf[/* lex_eof_reached */8] = /* true */1, 0);
            if ((lexbuf[/* lex_buffer_len */2] + n | 0) > lexbuf[/* lex_buffer */1].length) {
              if (((lexbuf[/* lex_buffer_len */2] - lexbuf[/* lex_start_pos */4] | 0) + n | 0) <= lexbuf[/* lex_buffer */1].length) {
                Bytes.blit(lexbuf[/* lex_buffer */1], lexbuf[/* lex_start_pos */4], lexbuf[/* lex_buffer */1], 0, lexbuf[/* lex_buffer_len */2] - lexbuf[/* lex_start_pos */4] | 0);
              }
              else {
                var newlen = Pervasives.min((lexbuf[/* lex_buffer */1].length << 1), Sys.max_string_length);
                if (((lexbuf[/* lex_buffer_len */2] - lexbuf[/* lex_start_pos */4] | 0) + n | 0) > newlen) {
                  throw [
                        Caml_builtin_exceptions.failure,
                        "Lexing.lex_refill: cannot grow buffer"
                      ];
                }
                var newbuf = Caml_string.caml_create_string(newlen);
                Bytes.blit(lexbuf[/* lex_buffer */1], lexbuf[/* lex_start_pos */4], newbuf, 0, lexbuf[/* lex_buffer_len */2] - lexbuf[/* lex_start_pos */4] | 0);
                lexbuf[/* lex_buffer */1] = newbuf;
              }
              var s = lexbuf[/* lex_start_pos */4];
              lexbuf[/* lex_abs_pos */3] = lexbuf[/* lex_abs_pos */3] + s | 0;
              lexbuf[/* lex_curr_pos */5] = lexbuf[/* lex_curr_pos */5] - s | 0;
              lexbuf[/* lex_start_pos */4] = 0;
              lexbuf[/* lex_last_pos */6] = lexbuf[/* lex_last_pos */6] - s | 0;
              lexbuf[/* lex_buffer_len */2] = lexbuf[/* lex_buffer_len */2] - s | 0;
              var t = lexbuf[/* lex_mem */9];
              for(var i = 0 ,i_finish = t.length - 1 | 0; i<= i_finish; ++i){
                var v = t[i];
                if (v >= 0) {
                  t[i] = v - s | 0;
                }
                
              }
            }
            Bytes.blit(aux_buffer, 0, lexbuf[/* lex_buffer */1], lexbuf[/* lex_buffer_len */2], n);
            lexbuf[/* lex_buffer_len */2] = lexbuf[/* lex_buffer_len */2] + n | 0;
            return /* () */0;
          },
          new Array(1024),
          0,
          0,
          0,
          0,
          0,
          0,
          /* false */0,
          /* int array */[],
          zero_pos,
          zero_pos
        ];
}

function from_channel(ic) {
  return from_function(function (buf, n) {
              return Pervasives.input(ic, buf, 0, n);
            });
}

function from_string(s) {
  return /* record */[
          function (lexbuf) {
            lexbuf[/* lex_eof_reached */8] = /* true */1;
            return /* () */0;
          },
          Bytes.of_string(s),
          s.length,
          0,
          0,
          0,
          0,
          0,
          /* true */1,
          /* int array */[],
          zero_pos,
          zero_pos
        ];
}

function lexeme(lexbuf) {
  var len = lexbuf[/* lex_curr_pos */5] - lexbuf[/* lex_start_pos */4] | 0;
  return Bytes.sub_string(lexbuf[/* lex_buffer */1], lexbuf[/* lex_start_pos */4], len);
}

function sub_lexeme(lexbuf, i1, i2) {
  var len = i2 - i1 | 0;
  return Bytes.sub_string(lexbuf[/* lex_buffer */1], i1, len);
}

function sub_lexeme_opt(lexbuf, i1, i2) {
  if (i1 >= 0) {
    var len = i2 - i1 | 0;
    return /* Some */[Bytes.sub_string(lexbuf[/* lex_buffer */1], i1, len)];
  }
  else {
    return /* None */0;
  }
}

function sub_lexeme_char(lexbuf, i) {
  return lexbuf[/* lex_buffer */1][i];
}

function sub_lexeme_char_opt(lexbuf, i) {
  if (i >= 0) {
    return /* Some */[lexbuf[/* lex_buffer */1][i]];
  }
  else {
    return /* None */0;
  }
}

function lexeme_char(lexbuf, i) {
  return lexbuf[/* lex_buffer */1][lexbuf[/* lex_start_pos */4] + i | 0];
}

function lexeme_start(lexbuf) {
  return lexbuf[/* lex_start_p */10][/* pos_cnum */3];
}

function lexeme_end(lexbuf) {
  return lexbuf[/* lex_curr_p */11][/* pos_cnum */3];
}

function lexeme_start_p(lexbuf) {
  return lexbuf[/* lex_start_p */10];
}

function lexeme_end_p(lexbuf) {
  return lexbuf[/* lex_curr_p */11];
}

function new_line(lexbuf) {
  var lcp = lexbuf[/* lex_curr_p */11];
  lexbuf[/* lex_curr_p */11] = /* record */[
    lcp[/* pos_fname */0],
    lcp[/* pos_lnum */1] + 1 | 0,
    lcp[/* pos_cnum */3],
    lcp[/* pos_cnum */3]
  ];
  return /* () */0;
}

function flush_input(lb) {
  lb[/* lex_curr_pos */5] = 0;
  lb[/* lex_abs_pos */3] = 0;
  var init = lb[/* lex_curr_p */11];
  lb[/* lex_curr_p */11] = /* record */[
    init[/* pos_fname */0],
    init[/* pos_lnum */1],
    init[/* pos_bol */2],
    0
  ];
  lb[/* lex_buffer_len */2] = 0;
  return /* () */0;
}

var dummy_pos = /* record */[
  "",
  0,
  0,
  -1
];

exports.dummy_pos           = dummy_pos;
exports.from_channel        = from_channel;
exports.from_string         = from_string;
exports.from_function       = from_function;
exports.lexeme              = lexeme;
exports.lexeme_char         = lexeme_char;
exports.lexeme_start        = lexeme_start;
exports.lexeme_end          = lexeme_end;
exports.lexeme_start_p      = lexeme_start_p;
exports.lexeme_end_p        = lexeme_end_p;
exports.new_line            = new_line;
exports.flush_input         = flush_input;
exports.sub_lexeme          = sub_lexeme;
exports.sub_lexeme_opt      = sub_lexeme_opt;
exports.sub_lexeme_char     = sub_lexeme_char;
exports.sub_lexeme_char_opt = sub_lexeme_char_opt;
exports.engine              = engine;
exports.new_engine          = new_engine;
/* No side effect */
