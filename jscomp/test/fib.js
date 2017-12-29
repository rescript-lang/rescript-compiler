'use strict';


function fib(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return fib(n - 1 | 0) + fib(n - 2 | 0) | 0;
  }
}

function fib2(n) {
  var _a = 1;
  var _b = 1;
  var _i = 0;
  while(true) {
    var i = _i;
    var b = _b;
    var a = _a;
    if (n === i) {
      return a;
    } else {
      _i = i + 1 | 0;
      _b = a + b | 0;
      _a = b;
      continue ;
      
    }
  };
}

function fib3(n) {
  var a = 1;
  var b = 1;
  for(var i = 1; i <= n; ++i){
    var tmp = a;
    a = b;
    b = b + tmp | 0;
  }
  return a;
}

exports.fib = fib;
exports.fib2 = fib2;
exports.fib3 = fib3;
/* No side effect */
