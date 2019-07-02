'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f(param, v) {
  return ((((param[/* x0 */0] + param[/* x1 */1] | 0) + param[/* x2 */2] | 0) + param[/* x3 */3] | 0) + param[/* x4 */4] | 0) + v | 0;
}

function f2(param, param$1) {
  return (((((param[/* x0 */0] + param[/* x1 */1] | 0) + param[/* x2 */2] | 0) + param[/* x3 */3] | 0) + param[/* x4 */4] | 0) + param$1[/* a */0] | 0) + param$1[/* b */1] | 0;
}

function f3(param) {
  var lhs = param[/* rank */0];
  return (function (param) {
      var rhs = param[/* rank */0];
      if (typeof lhs === "number") {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "fun_pattern_match.ml",
                43,
                9
              ]
            ];
      }
      if (typeof rhs === "number") {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "fun_pattern_match.ml",
                43,
                9
              ]
            ];
      }
      return Caml_primitive.caml_int_compare(lhs[0], rhs[0]);
    });
}

function f4(param) {
  var lhs = param[/* rank */0];
  return (function (param) {
      var rhs = param[/* rank */0];
      if (typeof lhs === "number") {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "fun_pattern_match.ml",
                51,
                9
              ]
            ];
      }
      if (typeof rhs === "number") {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "fun_pattern_match.ml",
                51,
                9
              ]
            ];
      }
      return Caml_primitive.caml_int_compare(lhs[0], rhs[0]);
    });
}

var x = /* `A */[
  65,
  r
];

function r(param) {
  return x;
}

var match = r(/* () */0);

var v = Curry._1(match[1], /* () */0);

console.log(v);

exports.f = f;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.r = r;
exports.v = v;
/* match Not a pure module */
