'use strict';

var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var v = [0];

var arr = Caml_array.caml_make_vect(10, (function () {
        return /* () */0;
      }));

function f() {
  var n = 0;
  while(n < 10) {
    var j = n;
    Caml_array.caml_array_set(arr, j, (function(j){
        return function () {
          v[0] = v[0] + j | 0;
          return /* () */0;
        }
        }(j)));
    n = n + 1 | 0;
  };
  return /* () */0;
}

f(/* () */0);

$$Array.iter((function (x) {
        return Curry._1(x, /* () */0);
      }), arr);

console.log(String(v[0]));

if (v[0] !== 45) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "test_while_closure.ml",
          63,
          4
        ]
      ];
}

var count = 10;

exports.v = v;
exports.count = count;
exports.arr = arr;
exports.f = f;
/*  Not a pure module */
