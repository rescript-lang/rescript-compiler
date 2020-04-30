'use strict';

var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");

var v = {
  contents: 0
};

function f(param) {
  var arr = Caml_array.caml_make_vect(10, (function (param) {
          
        }));
  for(var i = 0; i <= 9; ++i){
    Caml_array.caml_array_set(arr, i, (function(i){
        return function (param) {
          v.contents = v.contents + i | 0;
          
        }
        }(i)));
  }
  return arr;
}

var u = f(undefined);

$$Array.iter((function (x) {
        return Curry._1(x, undefined);
      }), u);

if (v.contents !== 45) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: /* tuple */[
          "test_closure.ml",
          53,
          2
        ],
        Error: new Error()
      };
}

exports.v = v;
exports.f = f;
/* u Not a pure module */
