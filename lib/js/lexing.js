'use strict';

var Bytes = require("./bytes.js");
var Curry = require("./curry.js");
var Caml_array = require("./caml_array.js");
var Caml_bytes = require("./caml_bytes.js");
var Caml_lexer = require("./caml_lexer.js");

function engine(tbl, state, buf) {
  var result = Caml_lexer.lex_engine(tbl, state, buf);
  if (result >= 0) {
    buf.lex_start_p = buf.lex_curr_p;
    var init = buf.lex_curr_p;
    buf.lex_curr_p = {
      pos_fname: init.pos_fname,
      pos_lnum: init.pos_lnum,
      pos_bol: init.pos_bol,
      pos_cnum: buf.lex_abs_pos + buf.lex_curr_pos | 0
    };
  }
  return result;
}

function new_engine(tbl, state, buf) {
  var result = Caml_lexer.new_lex_engine(tbl, state, buf);
  if (result >= 0) {
    buf.lex_start_p = buf.lex_curr_p;
    var init = buf.lex_curr_p;
    buf.lex_curr_p = {
      pos_fname: init.pos_fname,
      pos_lnum: init.pos_lnum,
      pos_bol: init.pos_bol,
      pos_cnum: buf.lex_abs_pos + buf.lex_curr_pos | 0
    };
  }
  return result;
}

var zero_pos = {
  pos_fname: "",
  pos_lnum: 1,
  pos_bol: 0,
  pos_cnum: 0
};

function from_function(f) {
  var partial_arg = Caml_bytes.create(512);
  return {
          refill_buff: (function (param) {
              var read = Curry._2(f, partial_arg, partial_arg.length);
              var n = read > 0 ? read : (param.lex_eof_reached = true, 0);
              if ((param.lex_buffer_len + n | 0) > param.lex_buffer.length) {
                if (((param.lex_buffer_len - param.lex_start_pos | 0) + n | 0) <= param.lex_buffer.length) {
                  Bytes.blit(param.lex_buffer, param.lex_start_pos, param.lex_buffer, 0, param.lex_buffer_len - param.lex_start_pos | 0);
                } else {
                  var newlen = (param.lex_buffer.length << 1);
                  if (((param.lex_buffer_len - param.lex_start_pos | 0) + n | 0) > newlen) {
                    throw {
                          RE_EXN_ID: "Failure",
                          _1: "Lexing.lex_refill: cannot grow buffer",
                          Error: new Error()
                        };
                  }
                  var newbuf = Caml_bytes.create(newlen);
                  Bytes.blit(param.lex_buffer, param.lex_start_pos, newbuf, 0, param.lex_buffer_len - param.lex_start_pos | 0);
                  param.lex_buffer = newbuf;
                }
                var s = param.lex_start_pos;
                param.lex_abs_pos = param.lex_abs_pos + s | 0;
                param.lex_curr_pos = param.lex_curr_pos - s | 0;
                param.lex_start_pos = 0;
                param.lex_last_pos = param.lex_last_pos - s | 0;
                param.lex_buffer_len = param.lex_buffer_len - s | 0;
                var t = param.lex_mem;
                for(var i = 0 ,i_finish = t.length; i < i_finish; ++i){
                  var v = Caml_array.get(t, i);
                  if (v >= 0) {
                    Caml_array.set(t, i, v - s | 0);
                  }
                  
                }
              }
              Bytes.blit(partial_arg, 0, param.lex_buffer, param.lex_buffer_len, n);
              param.lex_buffer_len = param.lex_buffer_len + n | 0;
            }),
          lex_buffer: Caml_bytes.create(1024),
          lex_buffer_len: 0,
          lex_abs_pos: 0,
          lex_start_pos: 0,
          lex_curr_pos: 0,
          lex_last_pos: 0,
          lex_last_action: 0,
          lex_eof_reached: false,
          lex_mem: [],
          lex_start_p: zero_pos,
          lex_curr_p: zero_pos
        };
}

function from_string(s) {
  return {
          refill_buff: (function (lexbuf) {
              lexbuf.lex_eof_reached = true;
            }),
          lex_buffer: Bytes.of_string(s),
          lex_buffer_len: s.length,
          lex_abs_pos: 0,
          lex_start_pos: 0,
          lex_curr_pos: 0,
          lex_last_pos: 0,
          lex_last_action: 0,
          lex_eof_reached: true,
          lex_mem: [],
          lex_start_p: zero_pos,
          lex_curr_p: zero_pos
        };
}

function lexeme(lexbuf) {
  var len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos | 0;
  return Bytes.sub_string(lexbuf.lex_buffer, lexbuf.lex_start_pos, len);
}

function sub_lexeme(lexbuf, i1, i2) {
  var len = i2 - i1 | 0;
  return Bytes.sub_string(lexbuf.lex_buffer, i1, len);
}

function sub_lexeme_opt(lexbuf, i1, i2) {
  if (i1 < 0) {
    return ;
  }
  var len = i2 - i1 | 0;
  return Bytes.sub_string(lexbuf.lex_buffer, i1, len);
}

function sub_lexeme_char(lexbuf, i) {
  return Caml_bytes.get(lexbuf.lex_buffer, i);
}

function sub_lexeme_char_opt(lexbuf, i) {
  if (i >= 0) {
    return Caml_bytes.get(lexbuf.lex_buffer, i);
  }
  
}

function lexeme_char(lexbuf, i) {
  return Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos + i | 0);
}

function lexeme_start(lexbuf) {
  return lexbuf.lex_start_p.pos_cnum;
}

function lexeme_end(lexbuf) {
  return lexbuf.lex_curr_p.pos_cnum;
}

function lexeme_start_p(lexbuf) {
  return lexbuf.lex_start_p;
}

function lexeme_end_p(lexbuf) {
  return lexbuf.lex_curr_p;
}

function new_line(lexbuf) {
  var lcp = lexbuf.lex_curr_p;
  lexbuf.lex_curr_p = {
    pos_fname: lcp.pos_fname,
    pos_lnum: lcp.pos_lnum + 1 | 0,
    pos_bol: lcp.pos_cnum,
    pos_cnum: lcp.pos_cnum
  };
}

function flush_input(lb) {
  lb.lex_curr_pos = 0;
  lb.lex_abs_pos = 0;
  var init = lb.lex_curr_p;
  lb.lex_curr_p = {
    pos_fname: init.pos_fname,
    pos_lnum: init.pos_lnum,
    pos_bol: init.pos_bol,
    pos_cnum: 0
  };
  lb.lex_buffer_len = 0;
}

var dummy_pos = {
  pos_fname: "",
  pos_lnum: 0,
  pos_bol: 0,
  pos_cnum: -1
};

exports.dummy_pos = dummy_pos;
exports.from_string = from_string;
exports.from_function = from_function;
exports.lexeme = lexeme;
exports.lexeme_char = lexeme_char;
exports.lexeme_start = lexeme_start;
exports.lexeme_end = lexeme_end;
exports.lexeme_start_p = lexeme_start_p;
exports.lexeme_end_p = lexeme_end_p;
exports.new_line = new_line;
exports.flush_input = flush_input;
exports.sub_lexeme = sub_lexeme;
exports.sub_lexeme_opt = sub_lexeme_opt;
exports.sub_lexeme_char = sub_lexeme_char;
exports.sub_lexeme_char_opt = sub_lexeme_char_opt;
exports.engine = engine;
exports.new_engine = new_engine;
/* No side effect */
