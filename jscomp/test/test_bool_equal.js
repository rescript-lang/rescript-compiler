// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_exceptions = require("../runtime/caml_exceptions");

function bool_equal(x, y) {
  if (x !== 0) {
    if (y !== 0) {
      return /* true */1;
    }
    else {
      return /* false */0;
    }
  }
  else {
    if (y !== 0) {
      return /* false */0;
    }
    else {
      return /* true */1;
    }
  }
}

function assertions() {
  if (!bool_equal(/* true */1, /* true */1)) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "test_bool_equal.ml",
            21,
            2
          ]
        ];
  }
  if (!bool_equal(/* false */0, /* false */0)) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "test_bool_equal.ml",
            22,
            2
          ]
        ];
  }
  if (!!bool_equal(/* true */1, /* false */0)) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "test_bool_equal.ml",
            23,
            2
          ]
        ];
  }
  if (!!bool_equal(/* false */0, /* true */1)) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "test_bool_equal.ml",
            24,
            2
          ]
        ];
  }
  if (/* true */1 !== /* true */1) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "test_bool_equal.ml",
            25,
            2
          ]
        ];
  }
  if (/* false */0 !== /* false */0) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "test_bool_equal.ml",
            26,
            2
          ]
        ];
  }
  if (/* false */0 !== /* true */1) {
    return 0;
  }
  else {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
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
