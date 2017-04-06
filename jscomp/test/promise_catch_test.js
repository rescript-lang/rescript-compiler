'use strict';

var Js_exn                  = require("../../lib/js/js_exn.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function handler(e) {
  if (e[0] === Js_exn.$$Error) {
    console.log("js error");
    return Promise.resolve(0);
  } else if (e === Caml_builtin_exceptions.not_found) {
    console.log("hi");
    return Promise.resolve(0);
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "promise_catch_test.ml",
            14,
            9
          ]
        ];
  }
}

function f(x) {
  return x.catch(handler);
}

exports.handler = handler;
exports.f       = f;
/* No side effect */
