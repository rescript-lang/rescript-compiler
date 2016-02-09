// Generated CODE, PLEASE EDIT WITH CARE
'use strict';


var v = [
  0,
  0
];

while(function () {
      var n = v[1];
      console.log("" + n);
      ++ v[1];
      return +(v[1] < 10);
    }()) {
  
};

function fib(n) {
  if (n === 0 || n === 1) {
    return 1;
  }
  else {
    return fib(n - 1) + fib(n - 2);
  }
}

var x = [
  0,
  3
];

while(function () {
      var y = 3;
      var n = x[1];
      console.log("" + n);
      ++ y;
      ++ x[1];
      return +(fib(x[1]) + fib(x[1]) < 20);
    }()) {
  console.log("" + 3);
};

exports.v   = v;
exports.fib = fib;
exports.x   = x;
/*  Not a pure module */
