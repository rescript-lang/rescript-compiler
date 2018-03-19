'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function peek_queue() {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "format_regression.ml",
          10,
          19
        ]
      ];
}

function int_of_size() {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "format_regression.ml",
          11,
          20
        ]
      ];
}

function take_queue() {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "format_regression.ml",
          12,
          19
        ]
      ];
}

function format_pp_token(_, _$1) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "format_regression.ml",
          13,
          26
        ]
      ];
}

function advance_loop(state) {
  while(true) {
    var match = peek_queue(state[/* pp_queue */3]);
    var size = match[/* elem_size */0];
    var size$1 = int_of_size(size);
    if (size$1 < 0 && (state[/* pp_right_total */2] - state[/* pp_left_total */1] | 0) < state[/* pp_space_left */0]) {
      return 0;
    } else {
      take_queue(state[/* pp_queue */3]);
      Curry._1(format_pp_token(state, size$1 < 0 ? 1000000010 : size$1), match[/* token */1]);
      state[/* pp_left_total */1] = match[/* length */2] + state[/* pp_left_total */1] | 0;
      continue ;
    }
  };
}

var pp_infinity = 1000000010;

exports.peek_queue = peek_queue;
exports.int_of_size = int_of_size;
exports.take_queue = take_queue;
exports.format_pp_token = format_pp_token;
exports.pp_infinity = pp_infinity;
exports.advance_loop = advance_loop;
/* No side effect */
