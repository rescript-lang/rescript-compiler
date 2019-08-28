'use strict';

var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var v = /* record */{
  contents: 0
};

var arr = Caml_array.caml_make_vect(10, (function (param) {
        return /* () */0;
      }));

function f(param) {
  var n = /* record */{
    contents: 0
  };
  while(n.contents < 10) {
    var j = n.contents;
    Caml_array.caml_array_set(arr, j, (function(j){
        return function (param) {
          v.contents = v.contents + j | 0;
          return /* () */0;
        }
        }(j)));
    Pervasives.incr(n);
  };
  return /* () */0;
}

f(/* () */0);

$$Array.iter((function (x) {
        return Curry._1(x, /* () */0);
      }), arr);

console.log(String(v.contents));

if (v.contents !== 45) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
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
