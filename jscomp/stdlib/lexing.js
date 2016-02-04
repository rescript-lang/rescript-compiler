// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Bytes       = require("./bytes");
var Pervasives  = require("./pervasives");
var Caml_lexer  = require("../runtime/caml_lexer");
var Sys         = require("./sys");
var Caml_string = require("../runtime/caml_string");

function engine(tbl, state, buf) {
  var result = Caml_lexer.caml_lex_engine(tbl, state, buf);
  if (result >= 0) {
    buf[11] = buf[12];
    var init = buf[12];
    buf[12] = [
      /* record */0,
      init[1],
      init[2],
      init[3],
      buf[4] + buf[6]
    ];
  }
  return result;
}

function new_engine(tbl, state, buf) {
  var result = Caml_lexer.caml_new_lex_engine(tbl, state, buf);
  if (result >= 0) {
    buf[11] = buf[12];
    var init = buf[12];
    buf[12] = [
      /* record */0,
      init[1],
      init[2],
      init[3],
      buf[4] + buf[6]
    ];
  }
  return result;
}

var zero_pos = [
  /* record */0,
  "",
  1,
  0,
  0
];

function from_function(f) {
  return [
          /* record */0,
          function (param) {
            var read_fun = f;
            var aux_buffer = new Array(512);
            var lexbuf = param;
            var read = read_fun(aux_buffer, aux_buffer.length);
            var n = read > 0 ? read : (lexbuf[9] = /* true */1, 0);
            if (lexbuf[3] + n > lexbuf[2].length) {
              if (lexbuf[3] - lexbuf[5] + n <= lexbuf[2].length) {
                Bytes.blit(lexbuf[2], lexbuf[5], lexbuf[2], 0, lexbuf[3] - lexbuf[5]);
              }
              else {
                var newlen = Pervasives.min(2 * lexbuf[2].length, Sys.max_string_length);
                if (lexbuf[3] - lexbuf[5] + n > newlen) {
                  Pervasives.failwith("Lexing.lex_refill: cannot grow buffer");
                }
                var newbuf = Caml_string.caml_create_string(newlen);
                Bytes.blit(lexbuf[2], lexbuf[5], newbuf, 0, lexbuf[3] - lexbuf[5]);
                lexbuf[2] = newbuf;
              }
              var s = lexbuf[5];
              lexbuf[4] += s;
              lexbuf[6] -= s;
              lexbuf[5] = 0;
              lexbuf[7] -= s;
              lexbuf[3] -= s;
              var t = lexbuf[10];
              for(var i = 0 ,i_finish = t.length - 1; i<= i_finish; ++i){
                var v = t[i];
                if (v >= 0) {
                  t[i] = v - s;
                }
                
              }
            }
            Bytes.blit(aux_buffer, 0, lexbuf[2], lexbuf[3], n);
            lexbuf[3] += n;
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
          /* array */[],
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
  return [
          /* record */0,
          function (lexbuf) {
            lexbuf[9] = /* true */1;
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
          /* array */[],
          zero_pos,
          zero_pos
        ];
}

function lexeme(lexbuf) {
  var len = lexbuf[6] - lexbuf[5];
  return Bytes.sub_string(lexbuf[2], lexbuf[5], len);
}

function sub_lexeme(lexbuf, i1, i2) {
  var len = i2 - i1;
  return Bytes.sub_string(lexbuf[2], i1, len);
}

function sub_lexeme_opt(lexbuf, i1, i2) {
  if (i1 >= 0) {
    var len = i2 - i1;
    return [
            /* Some */0,
            Bytes.sub_string(lexbuf[2], i1, len)
          ];
  }
  else {
    return /* None */0;
  }
}

function sub_lexeme_char(lexbuf, i) {
  return lexbuf[2][i];
}

function sub_lexeme_char_opt(lexbuf, i) {
  if (i >= 0) {
    return [
            /* Some */0,
            lexbuf[2][i]
          ];
  }
  else {
    return /* None */0;
  }
}

function lexeme_char(lexbuf, i) {
  return lexbuf[2][lexbuf[5] + i];
}

function lexeme_start(lexbuf) {
  return lexbuf[11][4];
}

function lexeme_end(lexbuf) {
  return lexbuf[12][4];
}

function lexeme_start_p(lexbuf) {
  return lexbuf[11];
}

function lexeme_end_p(lexbuf) {
  return lexbuf[12];
}

function new_line(lexbuf) {
  var lcp = lexbuf[12];
  lexbuf[12] = [
    /* record */0,
    lcp[1],
    lcp[2] + 1,
    lcp[4],
    lcp[4]
  ];
  return /* () */0;
}

function flush_input(lb) {
  lb[6] = 0;
  lb[4] = 0;
  var init = lb[12];
  lb[12] = [
    /* record */0,
    init[1],
    init[2],
    init[3],
    0
  ];
  lb[3] = 0;
  return /* () */0;
}

var dummy_pos = [
  /* record */0,
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
