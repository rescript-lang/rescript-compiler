'use strict';

var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function bool_equal(x, y) {
  if (x !== false) {
    if (y !== false) {
      return true;
    } else {
      return false;
    }
  } else if (y !== false) {
    return false;
  } else {
    return true;
  }
}

function assertions() {
  if (!bool_equal(true, true)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_bool_equal.ml",
            21,
            2
          ]
        ];
  }
  if (!bool_equal(false, false)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_bool_equal.ml",
            22,
            2
          ]
        ];
  }
  if (!!bool_equal(true, false)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_bool_equal.ml",
            23,
            2
          ]
        ];
  }
  if (!!bool_equal(false, true)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_bool_equal.ml",
            24,
            2
          ]
        ];
  }
  if (true !== true) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_bool_equal.ml",
            25,
            2
          ]
        ];
  }
  if (false !== false) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_bool_equal.ml",
            26,
            2
          ]
        ];
  }
  if (true === false) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_bool_equal.ml",
            27,
            2
          ]
        ];
  }
  if (false !== true) {
    return 0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "test_bool_equal.ml",
            28,
            2
          ]
        ];
  }
}

exports.bool_equal = bool_equal;
exports.assertions = assertions;
/* No side effect */
