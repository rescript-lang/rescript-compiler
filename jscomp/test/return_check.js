'use strict';

var Js_primitive = require("../../lib/js/js_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function test(dom) {
  var elem = dom.getElementById("haha");
  if ((
      elem === null ? /* None */0 : [elem]
    ) !== /* None */0) {
    console.log(elem);
    return 2;
  } else {
    return 1;
  }
}

function f_undefined(xs, i) {
  var match = xs[i];
  if ((
      match === undefined ? /* None */0 : [match]
    ) !== /* None */0) {
    return match;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "return_check.ml",
            31,
            14
          ]
        ];
  }
}

function f_escaped_not(xs, i) {
  var x = xs[i];
  console.log("hei");
  if ((
      x === undefined ? /* None */0 : [x]
    ) !== /* None */0) {
    return x;
  } else {
    return 1;
  }
}

function f_escaped_1(xs, i) {
  var x = xs[i];
  var x$1 = x === undefined ? /* None */0 : [x];
  return (function () {
      if (x$1 !== /* None */0) {
        return x;
      } else {
        return 1;
      }
    });
}

function f_escaped_2(xs, i) {
  console.log(Js_primitive.undefined_to_opt(xs[i]));
  return /* () */0;
}

function f_null(xs, i) {
  var match = xs[i];
  if ((
      match === null ? /* None */0 : [match]
    ) !== /* None */0) {
    return match;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "return_check.ml",
            59,
            14
          ]
        ];
  }
}

function f_null_undefined(xs, i) {
  var match = xs[i];
  if ((
      (match == null) ? /* None */0 : [match]
    ) !== /* None */0) {
    return match;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "return_check.ml",
            68,
            14
          ]
        ];
  }
}

exports.test = test;
exports.f_undefined = f_undefined;
exports.f_escaped_not = f_escaped_not;
exports.f_escaped_1 = f_escaped_1;
exports.f_escaped_2 = f_escaped_2;
exports.f_null = f_null;
exports.f_null_undefined = f_null_undefined;
/* No side effect */
