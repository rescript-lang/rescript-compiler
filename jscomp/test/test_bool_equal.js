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

function f0(x) {
  if (x === /* true */1) {
    return 1;
  } else {
    return 2;
  }
}

function f1(x) {
  if (x !== /* true */1) {
    return 1;
  } else {
    return 2;
  }
}

function f2(x) {
  if (x === true) {
    return 1;
  } else {
    return 2;
  }
}

function f3(x) {
  if (x === false) {
    return 1;
  } else {
    return 2;
  }
}

function f4(x) {
  if (x !== true) {
    return 1;
  } else {
    return 2;
  }
}

function f5(x) {
  if (x) {
    return 2;
  } else {
    return 1;
  }
}

function f6(x) {
  if (x === /* [] */0) {
    return 1;
  } else {
    return 2;
  }
}

function f7(x) {
  if (x.length !== 0) {
    return 1;
  } else {
    return 2;
  }
}

function f8() {
  return 2;
}

exports.bool_equal = bool_equal;
exports.assertions = assertions;
exports.f0 = f0;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.f8 = f8;
/* No side effect */
