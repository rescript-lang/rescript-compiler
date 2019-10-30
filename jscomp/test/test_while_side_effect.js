'use strict';


var v = /* record */[/* contents */0];

while(console.log(String(v[/* contents */0])), v[/* contents */0] = v[/* contents */0] + 1 | 0, v[/* contents */0] < 10) {
  
};

function fib(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return fib(n - 1 | 0) + fib(n - 2 | 0) | 0;
  }
}

var x = /* record */[/* contents */3];

while((function () {
        var y = 3;
        console.log(String(x[/* contents */0]));
        y = y + 1 | 0;
        x[/* contents */0] = x[/* contents */0] + 1 | 0;
        return (fib(x[/* contents */0]) + fib(x[/* contents */0]) | 0) < 20;
      })()) {
  console.log(String(3));
};

exports.v = v;
exports.fib = fib;
exports.x = x;
/*  Not a pure module */
