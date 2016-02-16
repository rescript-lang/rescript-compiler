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
    buf[10] = buf[11];
    var init = buf[11];
    buf[11] = /* record */[
      init[0],
      init[1],
      init[2],
      buf[3] + buf[5]
    ];
  }
  return result;
}

function new_engine(tbl, state, buf) {
  var result = Caml_lexer.caml_new_lex_engine(tbl, state, buf);
  if (result >= 0) {
    buf[10] = buf[11];
    var init = buf[11];
    buf[11] = /* record */[
      init[0],
      init[1],
      init[2],
      buf[3] + buf[5]
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
  return /* record */[
          function (param) {
            var read_fun = f;
            var aux_buffer = new Array(512);
            var lexbuf = param;
            var read = Caml_curry.app2(read_fun, aux_buffer, aux_buffer.length);
            var n = read > 0 ? read : (lexbuf[8] = /* true */1, 0);
            if (lexbuf[2] + n > lexbuf[1].length) {
              if (lexbuf[2] - lexbuf[4] + n <= lexbuf[1].length) {
                Bytes.blit(lexbuf[1], lexbuf[4], lexbuf[1], 0, lexbuf[2] - lexbuf[4]);
              }
              else {
                var newlen = Pervasives.min(2 * lexbuf[1].length, Sys.max_string_length);
                if (lexbuf[2] - lexbuf[4] + n > newlen) {
                  throw [
                        Caml_builtin_exceptions.Failure,
                        "Lexing.lex_refill: cannot grow buffer"
                      ];
                }
                var newbuf = Caml_string.caml_create_string(newlen);
                Bytes.blit(lexbuf[1], lexbuf[4], newbuf, 0, lexbuf[2] - lexbuf[4]);
                lexbuf[1] = newbuf;
              }
              var s = lexbuf[4];
              lexbuf[3] += s;
              lexbuf[5] -= s;
              lexbuf[4] = 0;
              lexbuf[6] -= s;
              lexbuf[2] -= s;
              var t = lexbuf[9];
              for(var i = 0 ,i_finish = t.length - 1; i<= i_finish; ++i){
                var v = t[i];
                if (v >= 0) {
                  t[i] = v - s;
                }
                
              }
            }
            Bytes.blit(aux_buffer, 0, lexbuf[1], lexbuf[2], n);
            lexbuf[2] += n;
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
            lexbuf[8] = /* true */1;
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
  var len = lexbuf[5] - lexbuf[4];
  return Bytes.sub_string(lexbuf[1], lexbuf[4], len);
}

function sub_lexeme(lexbuf, i1, i2) {
  var len = i2 - i1;
  return Bytes.sub_string(lexbuf[1], i1, len);
}

function sub_lexeme_opt(lexbuf, i1, i2) {
  if (i1 >= 0) {
    var len = i2 - i1;
    return /* Some */[Bytes.sub_string(lexbuf[1], i1, len)];
  }
  else {
    return /* None */0;
  }
}

function sub_lexeme_char(lexbuf, i) {
  return lexbuf[1][i];
}

function sub_lexeme_char_opt(lexbuf, i) {
  if (i >= 0) {
    return /* Some */[lexbuf[1][i]];
  }
  else {
    return /* None */0;
  }
}

function lexeme_char(lexbuf, i) {
  return lexbuf[1][lexbuf[4] + i];
}

function lexeme_start(lexbuf) {
  return lexbuf[10][3];
}

function lexeme_end(lexbuf) {
  return lexbuf[11][3];
}

function lexeme_start_p(lexbuf) {
  return lexbuf[10];
}

function lexeme_end_p(lexbuf) {
  return lexbuf[11];
}

function new_line(lexbuf) {
  var lcp = lexbuf[11];
  lexbuf[11] = /* record */[
    lcp[0],
    lcp[1] + 1,
    lcp[3],
    lcp[3]
  ];
  return /* () */0;
}

function flush_input(lb) {
  lb[5] = 0;
  lb[3] = 0;
  var init = lb[11];
  lb[11] = /* record */[
    init[0],
    init[1],
    init[2],
    0
  ];
  lb[2] = 0;
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
