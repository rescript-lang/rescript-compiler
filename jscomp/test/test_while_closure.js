// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_exceptions = require("../runtime/caml_exceptions");
var Caml_array      = require("../runtime/caml_array");
var $$Array         = require("../stdlib/array");

var v = [
  0,
  0
];

var count = 10;

var arr = Caml_array.caml_make_vect(count, function () {
      return /* () */0;
    });

function f() {
  var n = 0;
  while(n < count) {
    var j = n;
    arr[j] = (function(j){
    return function () {
      v[1] += j;
      return /* () */0;
    }
    }(j));
    ++ n;
  };
  return /* () */0;
}

f(/* () */0);

$$Array.iter(function (x) {
      return x(/* () */0);
    }, arr);

console.log("" + v[1]);

if (v[1] !== 45) {
  throw [
        0,
        Caml_exceptions.Assert_failure,
        [
          0,
          "test_while_closure.ml",
          63,
          4
        ]
      ];
}

exports.v     = v;
exports.count = count;
exports.arr   = arr;
exports.f     = f;
/*  Not a pure module */
