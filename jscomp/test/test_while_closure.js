'use strict';

var $$Array = require("../../lib/js/array.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");

var v = {
  contents: 0
};

var arr = Caml_array.caml_make_vect(10, (function (param) {
        
      }));

function f(param) {
  var n = 0;
  while(n < 10) {
    var j = n;
    Caml_array.caml_array_set(arr, j, (function(j){
        return function (param) {
          v.contents = v.contents + j | 0;
          
        }
        }(j)));
    n = n + 1 | 0;
  };
  
}

f(undefined);

$$Array.iter((function (x) {
        return Curry._1(x, undefined);
      }), arr);

console.log(String(v.contents));

if (v.contents !== 45) {
  throw {
        ExceptionID: "Assert_failure",
        _1: /* tuple */[
          "test_while_closure.ml",
          63,
          4
        ]
      };
}

var count = 10;

exports.v = v;
exports.count = count;
exports.arr = arr;
exports.f = f;
/*  Not a pure module */
