'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function bool_equal(x, y) {
  if (x !== 0) {
    if (y !== 0) {
      return /* true */1;
    } else {
      return /* false */0;
    }
  } else if (y !== 0) {
    return /* false */0;
  } else {
    return /* true */1;
  }
}

function assertions() {
  if (!bool_equal(/* true */1, /* true */1)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_bool_equal.ml",
            21,
            2
          ]
        ];
  }
  if (!bool_equal(/* false */0, /* false */0)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_bool_equal.ml",
            22,
            2
          ]
        ];
  }
  if (bool_equal(/* true */1, /* false */0)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_bool_equal.ml",
            23,
            2
          ]
        ];
  }
  if (bool_equal(/* false */0, /* true */1)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_bool_equal.ml",
            24,
            2
          ]
        ];
  }
  return 0;
}

exports.bool_equal = bool_equal;
exports.assertions = assertions;
/* No side effect */
