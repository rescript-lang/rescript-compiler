// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Caml_exceptions = require("../runtime/caml_exceptions");
var Caml_array = require("../runtime/caml_array");
var $$Array = require("../stdlib/array");

var v = [
  0,
  0
];

function f() {
  var arr = Caml_array.caml_make_vect(10, function () {
        return /* () */0;
      });
  for(var i = 0 ,i_finish = 10 - 1; i<= i_finish; ++i){
    arr[i] = (function(i){
    return function () {
      v[1] += i;
      return /* () */0;
    }
    }(i));
  }
  return arr;
}

var u = f(/* () */0);

$$Array.iter(function (x) {
      return x(/* () */0);
    }, u);

if (v[1] !== 45) {
  throw [
        0,
        Caml_exceptions.Assert_failure,
        [
          0,
          "test_closure.ml",
          53,
          2
        ]
      ];
}

exports.v = v;
exports.f = f;
/* u fail the pure module */
