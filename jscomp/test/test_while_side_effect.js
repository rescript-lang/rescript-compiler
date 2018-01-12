'use strict';


var v = [0];

while(console.log(String(v[0])), v[0] = v[0] + 1 | 0, v[0] < 10) {
  
};

function fib(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return fib(n - 1 | 0) + fib(n - 2 | 0) | 0;
  }
}

var x = [3];

while((function () {
        var y = 3;
        console.log(String(x[0]));
        y = y + 1 | 0;
        x[0] = x[0] + 1 | 0;
        return (fib(x[0]) + fib(x[0]) | 0) < 20;
      })()) {
  console.log(String(3));
};

exports.v = v;
exports.fib = fib;
exports.x = x;
/*  Not a pure module */
