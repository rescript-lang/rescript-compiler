// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Pervasives = require("../stdlib/pervasives");

var v = [
  0,
  0
];

while(console.log(Pervasives.string_of_int(v[1])), ++ v[1], +(v[1] < 10)) {
  
};

function fib(n) {
  return 1 < (n >>> 0) ? fib(n - 1) + fib(n - 2) : 1;
}

var x = [
  0,
  3
];

while(function () {
      var y = 3;
      console.log(Pervasives.string_of_int(x[1]));
      ++ y;
      ++ x[1];
      return +(fib(x[1]) + fib(x[1]) < 20);
    }()) {
  console.log(Pervasives.string_of_int(3));
};

exports.v = v;
exports.fib = fib;
exports.x = x;
/*  Not a pure module */
