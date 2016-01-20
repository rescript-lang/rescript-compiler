// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

function fib(n) {
  if (1 < (n >>> 0)) {
    return fib(n - 1) + fib(n - 2);
  }
  else {
    return 1;
  }
}

function fib2(n) {
  var _a = 1;
  var _b = 1;
  var _i = 0;
  while(/* true */1) {
    var i = _i;
    var b = _b;
    var a = _a;
    if (n === i) {
      return a;
    }
    else {
      _i = i + 1;
      _b = a + b;
      _a = b;
    }
  };
}

function fib3(n) {
  var a = 1;
  var b = 1;
  for(var i = 1; i<= n; ++i){
    var tmp = a;
    a = b;
    b += tmp;
  }
  return a;
}

exports.fib = fib;
exports.fib2 = fib2;
exports.fib3 = fib3;
/* No side effect */
