'use strict';

var Caml_option = require("../../lib/js/caml_option.js");

function test(dom) {
  var elem = dom.getElementById("haha");
  if (elem !== null) {
    console.log(elem);
    return 2;
  } else {
    return 1;
  }
}

function f_undefined(xs, i) {
  var k = xs[i];
  if (k !== undefined) {
    return k;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: /* tuple */[
          "return_check.ml",
          31,
          14
        ],
        Error: new Error()
      };
}

function f_escaped_not(xs, i) {
  var x = xs[i];
  console.log("hei");
  if (x !== undefined) {
    return x;
  } else {
    return 1;
  }
}

function f_escaped_1(xs, i) {
  var x = xs[i];
  return (function (param) {
      if (x !== undefined) {
        return x;
      } else {
        return 1;
      }
    });
}

function f_escaped_2(xs, i) {
  console.log(Caml_option.undefined_to_opt(xs[i]));
  
}

function f_null(xs, i) {
  var k = xs[i];
  if (k !== null) {
    return k;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: /* tuple */[
          "return_check.ml",
          59,
          14
        ],
        Error: new Error()
      };
}

function f_null_undefined(xs, i) {
  var k = xs[i];
  if (!(k == null)) {
    return k;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: /* tuple */[
          "return_check.ml",
          68,
          14
        ],
        Error: new Error()
      };
}

exports.test = test;
exports.f_undefined = f_undefined;
exports.f_escaped_not = f_escaped_not;
exports.f_escaped_1 = f_escaped_1;
exports.f_escaped_2 = f_escaped_2;
exports.f_null = f_null;
exports.f_null_undefined = f_null_undefined;
/* No side effect */
