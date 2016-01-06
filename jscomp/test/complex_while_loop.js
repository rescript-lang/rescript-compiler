// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Pervasives = require("../stdlib/pervasives");

function f() {
  var n = 0;
  while(function () {
        var fib = function (n) {
          return 1 < (n >>> 0) ? fib(n - 1) + fib(n - 2) : 1;
        };
        return +(fib(n) > 10);
      }()) {
    console.log(Pervasives.string_of_int(n));
    ++ n;
  };
  return /* () */0;
}

function ff() {
  while(function () {
        var a = 3;
        var b = a * a;
        return +(a + b > 10);
      }()) {
    
  };
  return /* () */0;
}

exports.f = f;
exports.ff = ff;
/* No side effect */
