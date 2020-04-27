'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function f(param, v) {
  return ((((param.x0 + param.x1 | 0) + param.x2 | 0) + param.x3 | 0) + param.x4 | 0) + v | 0;
}

function f2(param, param$1) {
  return (((((param.x0 + param.x1 | 0) + param.x2 | 0) + param.x3 | 0) + param.x4 | 0) + param$1.a | 0) + param$1.b | 0;
}

function f3(param) {
  var lhs = param.rank;
  return (function (param) {
      var rhs = param.rank;
      if (typeof lhs === "number") {
        throw {
              CamlExt: Caml_builtin_exceptions.assert_failure,
              _1: /* tuple */[
                "fun_pattern_match.ml",
                44,
                9
              ]
            };
      }
      if (typeof rhs === "number") {
        throw {
              CamlExt: Caml_builtin_exceptions.assert_failure,
              _1: /* tuple */[
                "fun_pattern_match.ml",
                44,
                9
              ]
            };
      }
      return Caml_primitive.caml_int_compare(lhs[0], rhs[0]);
    });
}

function f4(param) {
  var lhs = param.rank;
  return (function (param) {
      var rhs = param.rank;
      if (typeof lhs === "number") {
        throw {
              CamlExt: Caml_builtin_exceptions.assert_failure,
              _1: /* tuple */[
                "fun_pattern_match.ml",
                52,
                9
              ]
            };
      }
      if (typeof rhs === "number") {
        throw {
              CamlExt: Caml_builtin_exceptions.assert_failure,
              _1: /* tuple */[
                "fun_pattern_match.ml",
                52,
                9
              ]
            };
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

var match = r(undefined);

var v = Curry._1(match[1], undefined);

console.log(v);

function handle_tuple(x, y) {
  if (x !== 0) {
    if (x === 1 && y === 2) {
      return 3;
    }
    
  } else if (y === 1) {
    return 2;
  }
  console.log(/* tuple */[
        x,
        y
      ]);
  return x + y | 0;
}

exports.f = f;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.r = r;
exports.v = v;
exports.handle_tuple = handle_tuple;
/* match Not a pure module */
