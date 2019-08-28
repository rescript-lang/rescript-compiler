'use strict';

var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var v = /* record */{
  contents: 0
};

function f(param) {
  var arr = Caml_array.caml_make_vect(10, (function (param) {
          return /* () */0;
        }));
  for(var i = 0; i <= 9; ++i){
    Caml_array.caml_array_set(arr, i, (function(i){
        return function (param) {
          v.contents = v.contents + i | 0;
          return /* () */0;
        }
        }(i)));
  }
  return arr;
}

var u = f(/* () */0);

$$Array.iter((function (x) {
        return Curry._1(x, /* () */0);
      }), u);

if (v.contents !== 45) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "test_closure.ml",
          53,
          2
        ]
      ];
}

exports.v = v;
exports.f = f;
/* u Not a pure module */
