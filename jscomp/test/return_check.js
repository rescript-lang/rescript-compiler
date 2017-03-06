'use strict';

var Js_primitive            = require("../../lib/js/js_primitive");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions");

function test(dom) {
  var elem = dom.getElementById("haha");
  if (elem !== null) {
    console.log(elem);
    return 2;
  }
  else {
    return 1;
  }
}

function f_undefined(xs, i) {
  var match = xs[i];
  if (match !== undefined) {
    return match;
  }
  else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "return_check.ml",
            29,
            14
          ]
        ];
  }
}

function f_escaped_not(xs, i) {
  var x = xs[i];
  console.log("hei");
  if (x !== undefined) {
    return x;
  }
  else {
    return 1;
  }
}

function f_escaped_1(xs, i) {
  var x = xs[i];
  return function () {
    if (x !== undefined) {
      return x;
    }
    else {
      return 1;
    }
  };
}

function f_escaped_2(xs, i) {
  console.log(Js_primitive.js_from_def(xs[i]));
  return /* () */0;
}

function f_null(xs, i) {
  var match = xs[i];
  if (match !== null) {
    return match;
  }
  else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "return_check.ml",
            57,
            14
          ]
        ];
  }
}

function f_null_undefined(xs, i) {
  var match = xs[i];
  if (Js_primitive.js_is_nil_undef(match)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "return_check.ml",
            66,
            14
          ]
        ];
  }
  else {
    return match;
  }
}

exports.test             = test;
exports.f_undefined      = f_undefined;
exports.f_escaped_not    = f_escaped_not;
exports.f_escaped_1      = f_escaped_1;
exports.f_escaped_2      = f_escaped_2;
exports.f_null           = f_null;
exports.f_null_undefined = f_null_undefined;
/* No side effect */
